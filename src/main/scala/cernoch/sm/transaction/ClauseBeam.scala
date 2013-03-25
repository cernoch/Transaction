package cernoch.sm.transaction

import Tools._

import cernoch.scalistics.aggregator._
import cernoch.scalogic._
import sql.{JoinModel, SqlExecutor}
import cernoch.scalogic.tools.Labeler
import cernoch.sm.logic.Generator._
import cernoch.sm.space.{ThrowAway, Reconsider, BeamSearch}

import collection.mutable
import math.{BigInt, BigDecimal => BigDec}
import grizzled.slf4j.Logging
import java.sql.SQLException

/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
abstract class ClauseBeam
	(storage: SqlExecutor,
	 domains: Domains,
	 modeSet: () => Schema,
	 dataset: Map[Val,String],
	 vychozi: WekaBridge,
	 baseAcc: Double)
  extends BeamSearch[State,Result]
	with Logging {

	/**
	 * Timeout for evaluating a single query in seconds.
	 *
	 * Queries exceeding the timeout will be specialized.
	 */
	var queryRowLimit: Long = 1000 * 1000

	/**
	 * Maximum number of results per query.
	 *
	 * Queries exceeding this limit will be specialized.
	 *
	 * This value should >> number of instances in
	 * [[cernoch.sm.secret.transaction.ClauseBeam.dataset]].
	 */
	var queryTimeOut = 300

	def descendants(state: State)
	= onlySpecialize(state) ++
		onlyGeneralize(state)

	override def onlySpecialize
	(state: State)
	= state match {
		case saa: State with AddedAtom => instantiate(saa)
		case _ => List()
	}

	override def onlyGeneralize
	(state: State)
	= state match {
		case saa: State => addNewAtoms(saa)
		case _ => List()
	}

	/**
   * Adds new atoms to the horn clause based on the language bias
   */
  def addNewAtoms(orig: State)
  = {
		val schema = modeSet()
		schema.modes.flatMap(mode => {
			// Generate all possible substitutions
			allSubsts(mode.iVar.toList, orig.horn.vars.distinct)
				.distinct.map{s => mode subst {s.get(_)}} // and use them
		})
		// Throw away modes that got malformed during subst.
		.filter(_.maxSucc.getOrElse(1) > 0)
		// And create new states
		.flatMap(newMode => {
			// We cannot relate to the future
			val causality
			= Atom("<",
				newMode.vars.find(_.dom == domains.stamp).get,
				orig.head.stamp
			)

			causality.args(0) == causality.args(1) match {
				case true => None
				case false => {
					// Variables that can be aggregated
					val numericVars = newMode.atom.vars
						.filter{v => schema.datas.contains(v.dom)}

					val newBody = orig.horn.bodyAtoms +
						newMode.atom + causality

					Some(new State(orig.initSchema, numericVars, newBody)
					    with AddedAtom { val added = newMode.atom } )
				}
			}
		})
	}

  /**
   * Instantiates variables in the head of an added atom
   */
  def instantiate
  (orig: State with AddedAtom)
	= {
		val schema = modeSet()

		instantiable( orig.added.vars
			.filter(v => schema.insts.contains(v.dom))
		).map{ case (war,wal) => {

			val l = Labeler.alphabet[Var]
			debug("Instantiating"
				+ s" ${war.toString(short=false,names=l)} -> "
				+ s" ${wal.toString(short=false,names=l)}\nin"
				+ orig.horn.toString(short=true,names=l)
			)

			val newBody = orig.horn.bodyAtoms.map{ _ subst (war->wal) }
			val newHead = orig.head.others.filter{ _ != war }

			new State(orig.initSchema, newHead, newBody)
			with AddedAtom { val added = orig.added }
		}}
	}

	def stateResult(state: State)
	= {
		var best: Result = null

		val n = Labeler.alphabet[Var]
		info(s"Executing query: ${state.horn.toString(short=true,names=n)}")

		// Prepare for OutOfMemoryException
		val tooNew = new Reconsider(
			"Query produces too many results." +
				" Must be specialized")

		try {
			val joinModel = storage.prepare(state.horn.bodyAtoms)

			execQuery(
				joinModel,
				state.valuedVars.toSet,
				state.head.exId
			).foreach {

				case ((war,agg), exampleMap) => {

					val newAttr  = Var(DoubleDom(war.dom.name))
					val enriched = vychozi.enrich(newAttr, exampleMap.toMap)
					val accuracy = baseAcc - WekaBridge.classify(enriched)

					state.synchronized {
						if (best == null ||
							  best.acc < accuracy)
							best = new Result(war, agg,
								enriched, accuracy, joinModel)
					}
				}
			}

			if (best == null) throw new Reconsider(
				"No aggregable variable in the query.")

		} catch {
			case _:OutOfMemoryError => {
				System.gc()
				info("Clause not evaluated becaues of memory limits.")
				throw tooNew
			}

			case x:SQLException => {
				warn(s"Generic SQL exception (${x.getErrorCode}) met." +
					s" Hoping the clause was too specific.", x)
				throw tooNew
			}

			case t:ThrowAway => {
				info(s"Clause'll be trashed: ${t.getMessage}")
				throw t
			}
			case t:Reconsider => {
				info(s"Clause'll be specialized: ${t.getMessage}")
				throw t
			}
		}

		info(s"Best accuracy +${best.acc2print} achieved using ${best.agg}" +
		     s" on variable ${best.war.toString(short=false,names=n)}.")
		best
	}

	def onlySort
	( old: Iterable[(State,Result)],
		neu: Iterable[(State,Result)])
	= (old.view ++ neu).toArray.sortBy{- _._2.acc}



	/**
	 * Execute the query.
	 */
	def execQuery
	(query: JoinModel,
	 valuedVars: Set[Var],
	 exId: Var)
	: Map[(Var,String), Iterable[(Val,Double)]]
	= {
		Terminator(() => {

			// First chech if the query is not too big.
			try {
				if (query.count > queryRowLimit)
					throw new Reconsider(
						"Query would produce too many" +
							" reslts. Please specialize.") }
			catch {
				case _: InterruptedException => throw new ThrowAway(
					"Can't evaluate number of rows. That's really, really bad.")
			}

			// Start printing the status every 10s
			var done: Long = 0

			// COUNT aggregator is computed separately
			val cntExAgg = new mutable.HashMap[Val,Long]()
			// Create a triplet of aggregators for each example
			val varExAgg = valuedVars.map(
				war => war -> dataset.map{case (k,_) => k -> new DecAggs()}.toMap
			).toMap

			Informator(
				() => done,
				query.count,
				informator => {

					// Execute query and fill the aggregators
					query.select(valuedVars + exId, resMap => {

						if (Thread.interrupted()) throw new Reconsider(
							"Evaluation thread was interrupted.")

						// Get the example number
						val exNo = resMap(exId)

						// Increase number of results per examples
						cntExAgg += exNo -> (cntExAgg.get(exNo).getOrElse(0L) + 1L)

						// For each queried value
						valuedVars.foreach(war => {
							val wal = resMap(war)
							if (wal.value != null) {
								varExAgg(war).get(exNo).foreach(agg => wal match {
									case DecVal(v:BigDec,_) => agg += v
									case DecVal(v,f)        => agg += BigDec(f.toDouble(v))

									case NumVal(v:BigInt,_) => agg += BigDec(v)
									case NumVal(v,n)        => agg += BigDec(n.toLong(v))

									case _ => trace(s"Ignoring value $wal:${wal.dom} in examle $exNo.")
								})
							}
						})
						done += 1
					})

					if (done == 0) throw new ThrowAway(
						"Query did not produce a single result")

					val out = new mutable.HashMap[(Var,String),mutable.Set[(Val,Double)]]
						with mutable.MultiMap[(Var,String),(Val,Double)]

					for (
						(war, ex2agg) <- varExAgg;
						(exNo, agg)   <- ex2agg;
						(name, value) <- agg()
					) out.addBinding((war, name), (exNo, value))

					val usedMem = Tools.usedMem
					info(s"Total of $done records aggregated" +
						s" in ${informator.elapsed}s and ${usedMem}MB memory.")

					out.toMap + (
						(exId,"COUNT") -> cntExAgg.map{case (k,v) => k -> v.toDouble}
					)
				}
			)
		})
	}

	/**
	 * Print the result of the evaluation every 10 seconds.
	 *
	 * @param rowsDone Number of already evaluated rows
	 * @param rowsTotal Number of all rows
	 */
	class Informator(rowsDone:  () => Long, rowsTotal: Long) extends Thread {

		/** Time when system started */
		val started = System.currentTimeMillis()

		/** Number of seconds after the thread started */
		def elapsed = (System.currentTimeMillis() - started) / 1000

		override def run() {
			try {
				while (!isInterrupted) {
					Thread.sleep(10000)
					debug(s"${rowsDone()} results done in ${elapsed}s using ${usedMem}MB mem")
				}
			} catch {
				case _: OutOfMemoryError =>
				case _: InterruptedException =>
				case _: Throwable => error("Informator terminated due to an unknown reason.")
			}
		}
	}

	/**
	 * Convenience that creates and executes a thread
	 */
	object Informator {
		def apply[T]
		(rowsDone: () => Long,
		 rowsTotal: Long,
		 callback: Informator => T)
		= {
			val thread = new Informator(rowsDone, rowsTotal)
			try {
				thread.start()
				callback(thread)

			} finally  {
				thread.interrupt()
			}
		}
	}

	/**
	 * Convenience that creates and executes a thread
	 */
	object Terminator {

		def apply[T](worker: () => T)
		= {

			var retVal: Option[T] = None
			var thrown: Option[Throwable] = None

			val thread = new Thread() {
				override def run() {
					try   {
						retVal = Option(worker())
					} catch { case t : Throwable =>
						thrown = Some(t)
					}
				}
			}

			try {
				thread.start()
				thread.join(1000*queryTimeOut)

				thrown match {
					case Some(throwable) => throw throwable
					case None =>
				}

				retVal match {
					case Some(retval) => retval
					case None => throw new Reconsider("Query timed-out.")
				}

			} finally  {
				thread.interrupt()
			}
		}
	}


}
