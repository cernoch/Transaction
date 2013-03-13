package cernoch.sm.secret.transaction

import cernoch._
import scalogic._
import sm.space.SearchProbe
import sm.sql._
import tools.Labeler
import collection.mutable
import grizzled.slf4j.Logging
import org.rogach.scallop.{LazyScallopConf, ScallopConf}
import java.util.logging._
import org.rogach.scallop.exceptions.{ScallopException, UnknownOption, ValidationFailure}

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object CommandLine extends Logging {
  def main(args: Array[String]) : Unit = {

		Logger.getLogger("").getHandlers().view
			.filter{_.isInstanceOf[ConsoleHandler]}
			.foreach{_.setFormatter(new MinLogFormat())}

    val opts = new CmdLineOpts(args)
		try {
			opts.verify()
		} catch {
			case e: ScallopException => {
				println(e.getMessage)
				println()
				println("Valid command line arguments:")
				opts.printHelp()
				System.exit(1)
			}
		}

		debug("Using driver: " + opts.drv())
		debug("SQL hostname: " + opts.host())
		debug("SQL database: " + opts.base())
		debug("SQL username: " + opts.user())
		debug("SQL password: " + opts.pass())
		debug("SQL table:    " + opts.table())

		Schema.tableName = opts.table()

    val connector = new Connect(
			host = opts.host(),
			user = opts.user(),
			pass = opts.pass(),
			base = opts.base()
		)

		val sc = opts.drv().toLowerCase() match {
			case "mysql"    => connector.toMySQL
			case "postgres" => connector.toPostgres
		}

    val ss = new SqlStorage(sc, List(Schema.starter)).open

    val bl = WekaBridge(State(),ss)
    println("Baseline accuracy = " +
      math.round(WekaBridge.classify(bl)) + "%")

		val dataSet = mutable.HashMap[Int,String]()

		ss.query(State().horn, resMap => {
			(resMap(State.exId).value,
				resMap(State.klass).value) match {
				case (ex: Int, cl:String) => dataSet += (ex -> cl)
				case (ex: Int, null) => warn(
					"Ignoring " + State.exId.dom.name + "=" + ex + ", " +
					"because '" + State.klass.dom.name + "' is not set." )
				case _ => throw new IllegalArgumentException("Unexpected SQL schema")
			}
		})
		info("There are " + dataSet.size + " examples in the database.")

		println(Schema.others)

    val nb = new ClauseBeam(
			ss, Schema.others.toSet,
			dataSet.toMap,
			bl, WekaBridge.classify(bl)
		){
      maxConsNonImp = 1
      beamWidth = 3
		}

    nb.probe = new SearchProbe[State,Result]() {
      override def searchIteration
      ( draughts: Iterable[(State,Result)],
        breedins: Iterable[State] ) {
        println("==================================")
        println("     STARTING A NEW ITERATION   ")
        println("==================================")
      }

      override def statesGenerated
      (states: Iterable[State]) {
        println("Generated " + states.size + " new clauses.")
      }

      override def bestUpdated
      (state: State, result: Result) {
        val n = Labeler.alphabet[Var]
        println(">>>>> In this iteration, a new optimum was found <<<<<")
        println("SCORE = +" + (math.round(result.acc * 100).toDouble / 100) + "% using "
          + result.agg + "(" + result.war.toString(false, n) + ").")
        println("STATE = " + state.horn.toString(true, n))
      }
    }

		val (state,result) = nb.call().head
		val n = Labeler.alphabet[Var]
		println()
		println("=====================================")
		println("               FINISHED              ")
		println("=====================================")
		println("Suggesting to use the following query:")
		println("SCORE = +" + (math.round(result.acc * 100).toDouble / 100.0) + "% using "
			+ result.agg + "(" + result.war.toString(false, n) + ").")
		println("STATE = " + state.horn.toString(false, n))
  }

	class CmdLineOpts(args: Array[String])
		extends LazyScallopConf(args) {

		val drv = opt[String]("driver",
			short = 's',
			descr = "Connect to 'MySQL' or 'Postgres' database",
			default = Some("MySQL"),
			validate = _.toLowerCase() match {
				case "postgres" => true
				case "mysql" => true
				case _ => false
			}
		)

		val user = opt[String]("username",
			descr = "Username for SQL connection",
			validate = _.trim.length > 0,
			required = true)


		val pass = opt[String]("password",
			descr = "Password for SQL connection",
			validate = _.trim.length > 0,
			required = true)

		val host = opt[String]("hostname",
			descr = "Hostname of the SQL server",
			validate = _.trim.length > 0,
			default = Some("localhost"))

		val base = opt[String]("database",
			short = 'b',
			descr = "Name of the database",
			validate = _.trim.length > 0,
			required = true)

		val table = opt[String]("table",
			descr = "Name of main table",
			validate = _.trim.length > 0,
			default = Some(Schema.tableName))
	}
}
