package cernoch.sm.secret.transaction

import cernoch.scalogic._
import cernoch.sm.logic.Generator
import cernoch.sm.space.{ThrowAway, Reconsider, BeamSearch}
import cernoch.sm.sql.QueryExecutor
import cernoch.scalistics.aggregator._

import collection.mutable

import Generator._
import tools.Labeler
import grizzled.slf4j.Logging
import java.sql.SQLException

/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
abstract class ClauseBeam
	(storage: QueryExecutor,
	 modeSet: Set[Mode],
	 dataset: Map[Int,String],
	 vychozi: WekaBridge[Int],
	 vychRes: Double)
  extends BeamSearch[State,Result]
	with Logging {

  /**
   * Initial state is an empty horn clause without a head
   */
  def sourceState = State()

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
  = modeSet.flatMap(mode => {
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
				newMode.vars.find(_.dom == Domains.dt).get,
				orig.head.stamp
			)

			causality.args(0) == causality.args(1) match {
				case true => None
				case false => {
					// Variables that can be aggregated
					val numericVars = newMode.atom.vars
						.filter{v => Schema.domWithValue.contains(v.dom)}

					val newBody = orig.horn.bodyAtoms +
						newMode.atom + causality

					val state
					= new State(numericVars, newBody)
						with AddedAtom {
							val added = newMode.atom
						}
					Some(state)
				}
			}
		})

  /**
   * Instantiates variables in the head of an added atom
   */
  def instantiate
  (orig: State with AddedAtom)
	= instantiable( orig.added.vars
			.filter(v => Schema.instantiable.contains(v.dom))

		).map{ case (war,wal) => {

			val l = Labeler.alphabet[Var]
			debug("Instantiating"
				+ s" ${war.toString(false,l)} -> "
				+ s" ${wal.toString(false,l)}\nin"
				+ orig.horn.toString(true,l)
			)

			val newBody = orig.horn.bodyAtoms.map{ _ subst (war->wal) }
			val newHead = orig.head.others.filter{ _ != war }

			new State(newHead, newBody) with AddedAtom {
				val added = orig.added
			}
		}
	}

	def stateResult(state: State)
	= {
		var best: Result = null

		val n = Labeler.alphabet[Var]
		println("----- Executing a new query -----")
		println(state.horn.toString(true, n))

		// Prepare for OutOfMemoryException
		val tooNew = new Reconsider(
			"Query produces too much results." +
				" Must be specialized")

		try {
			execQuery(state).par.foreach {
				case ((war,agg), exampleMap) => {

					val newAttr = Var(Domain.dec(war.dom.name))
					val enriched = vychozi.enrich(newAttr, exampleMap.toMap)
					val accuracy = vychRes - WekaBridge.classify(enriched)

					state.synchronized {
						if (best == null || best.acc < accuracy)
							best = new Result(war, agg, enriched, accuracy)
					}
				}
			}
		} catch {
			case _:OutOfMemoryError => {
				System.gc()
				println()
				println("Clause not evaluated becaues of memory limits.")
				throw tooNew
			}
			case x:SQLException => {
				warn(s"Generic SQL exception (${x.getErrorCode}) met." +
					s" Hoping the clause was too specific.", x)
				throw tooNew
			}
		}

		if (best == null) throw new Reconsider(
			"No aggregable variable in the query.")

		val bestAcc = (math.round(best.acc * 100).toDouble / 100)
		println(s"Best accuracy +${bestAcc}% achieved using ${best.agg}" +
			s" on variable ${best.war.toString(false, n)}.")

		best
	}

	def onlySort
	( old: Iterable[(State,Result)],
		neu: Iterable[(State,Result)])
	= (old.view ++ neu).toArray.sortBy{- _._2.acc}



	/**
	 * Execute the query.
	 */
	def execQuery(state: State)
	: Map[(Var,String),Iterable[(Int,Double)]]
	= {

		// Start printing the status every 10s
		var results: Long = 0
		val started = System.currentTimeMillis()
		val writer  = new Thread("StatusWriter") {
			override def run() {
				try {
					while (true) {
						Thread.sleep(10000)
						val elapsed = (System.currentTimeMillis() - started) / 1000
						val usedMem = Tools.usedMem
						print(s"... ${results} results done in ${elapsed}s using ${usedMem}MB mem")
					}
				} catch {
					case _:OutOfMemoryError =>
					case _:InterruptedException =>
				}
			}
		}

		// COUNT is computed separately
		val cntExAgg = new mutable.HashMap[Int,Long]()

		// Create a triplet of aggregators for each example
		val varExAgg = state.head.others
			// This filtering should be done when adding an atom
			//.filter(war => Schema.semanticDoms.contains(war.dom))
			.map(war => war ->
				dataset.map{case (k,_) =>
					k -> new DoubleAggs()
				}.toMap
			).toMap

		try {
			writer.start()

			// Execute query and fill the aggregators
			storage.query(state.horn, resMap => {

				// Print info on first execution
				if (results == 0) {
					val delta = (System.currentTimeMillis().toDouble - started) / 1000
					print("... query evaluated in " + delta + "s")
				}

				// The example No. must be an integer! No other way!
				val exNo = resMap(State.exId).value.asInstanceOf[Int]

				cntExAgg += exNo -> (cntExAgg.get(exNo).getOrElse(0L) + 1L)

				// For each queried value
				state.head.others.foreach{war => {
					val wal = resMap(war)
					val agg = varExAgg(war)

					agg.get(exNo) match {
						case None => // No aggregator for the example
						case Some(agg) => wal match { // Aggregated as BigDouble

							case wal:Dec[_] => wal.get.foreach{ v =>
								agg += BigDecimal(wal.dom.toDouble(v))
							}
							case wal:Num[_] => wal.get.foreach{ v =>
								agg += BigDecimal(wal.dom.toLong(v))
							}
							case _ =>
						}
					}
				}}
				results += 1
			})

		} finally {
			writer.interrupt()
		}

		// All results are collected...
		val elapsed = (System.currentTimeMillis() - started) / 1000

		if (results == 0) throw new
				ThrowAway("Query did not produce a single result")

		val out = new mutable.HashMap[(Var,String),mutable.Set[(Int,Double)]]
		         with mutable.MultiMap[(Var,String),(Int,Double)]
		for (
			(war, ex2agg) <- varExAgg;
			(exNo, agg)   <- ex2agg;
		  (name, value) <- agg()
		) out.addBinding((war, name), (exNo, value))

		val usedMem = Tools.usedMem
		println(s"... total of ${results} records aggregated" +
			s" in ${elapsed}s and ${usedMem} MB memory.")

		out.toMap + ( (State.exId,"COUNT") -> cntExAgg.map{case (k,v) => k -> v.toDouble} )
	}
}
