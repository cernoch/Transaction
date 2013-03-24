package cernoch.sm.secret.transaction

import cernoch.scalogic._
import cernoch.scalogic.sql._

import cernoch.sm.space.SearchProbe

import tools.Labeler
import collection.mutable
import grizzled.slf4j.Logging
import java.util.logging._

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object CommandLine extends Logging {
	def main(args: Array[String]) {
		try {
			val options = CmdLineOpts(args)

			val connector
			= new Connect(
				host = options.host(),
				user = options.user(),
				pass = options.pass(),
				base = options.base()
			)

			println(options.datas())

			val adapter
			= options.drv().toLowerCase match {
				case "mysql"    => connector.toMySQL
				case "postgres" => connector.toPostgres
			}

			val domains = Domains(
				 ada  = adapter,
				table = options.table(),
				ident = options.ident(),
				klass = options.klass(),
				deNul = options.deNul(),
				stamp = options.stamp(),
				joins = options.joins(),
				insts = options.insts(),
				datas = options.datas()
			)

			if (options.deNul())
				adapter.deNull = Set(domains.klass)

			val new_Schema
			= Schema(
				domains = domains,
				table = options.table(),
				joins = options.joins(),
				insts = options.insts(),
				datas = options.datas()
			)

			val initSchema = new_Schema()
			val starter = State(initSchema)

			info(s"Search will be structured using modes:\n" +
				initSchema.modes
					.map{_.toString(short=false,names=Labeler.alphabet[Var])}
					.mkString("\n") )

			val storage
			= new SqlStorage(
				ada = adapter,
				sch = List(initSchema.atom)
			).open

			// New dataset with all variables, klass first
			val wekaBridge = new WekaBridge(
				(starter.head.klass :: starter.head.others)
					.filter{v => WekaBridge.supported(v.dom)} )

			// Example => klass mapping
			val dataSet = mutable.HashMap[Val,String]()

			// Load all instances from the database
			storage.query(starter.horn, mapa => {

				val ident = mapa(starter.head.exId)
				val klass = mapa(starter.head.klass)

				// Cannot recognize if klass is null
				if (klass.value == null)
					warn(s"Example $ident ignored, class is null.")

				else if (!klass.value.isInstanceOf[String])
					warn(s"Example $ident ignored, class is not String.")

				else {
					wekaBridge.add(ident, mapa)
					dataSet.put(ident, klass.toString())
				}
			})

			info(s"There are ${dataSet.size} examples in the database.")

			val baseLineAccuracy = WekaBridge.classify(wekaBridge)
			info(s"Baseline accuracy = ${math.round(baseLineAccuracy*100)/100}%")

			val beamSearch
			= new ClauseBeam(
				storage = storage,
				domains = domains,
				modeSet = new_Schema,
				dataset = dataSet.toMap,
				vychozi = wekaBridge,
				baseAcc = baseLineAccuracy
			){
				def sourceState = starter
				probe = new LoggingProbe()
				maxConsNonImp = 1
				beamWidth = 3
			}

			val (state,result) = beamSearch.call().head
			val n = Labeler.alphabet[Var]
			info(s"Search finished. Suggesting to use the following query: "
				+ LoggingProbe.stateScore(state,result) )

		} catch {
			case e: ParamException => {
				println(s"ERROR: ${e.getMessage}")
				println()
				println("Valid command line arguments:")
				CmdLineOpts.printHelp()
				System.exit(1)
			}
		}
  }

	/**
	 * Replaces the command-line logger with custom formatter
	 */
	def useCustomLogger() {
		Logger.getLogger("").getHandlers().view
			.filter{_.isInstanceOf[ConsoleHandler]}
			.foreach{_.setFormatter(new MinLogFormat())}
	}
}
