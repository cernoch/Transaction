package cernoch.sm.secret.transaction

import org.rogach.scallop.exceptions.ScallopException
import org.rogach.scallop.{ArgType, ValueConverter, LazyScallopConf}
import scala.reflect.runtime.universe.TypeTag
import grizzled.slf4j.Logging

/**
 * Parses command-line arguments and exits on failure
 */
object CmdLineOpts extends Logging {

	def apply(args: Array[String]) = {
		val o = new CmdLineOpts(args)
		try   { o.verify() }
		catch { case e:ScallopException => throw new ParamException(e) }

		info(s"SQL settings:"+
			s" driver=${o.drv()} host=${o.host()} database=${o.base()}" +
			s" user=${o.user()} pass=${o.pass().map(_=>'*').mkString("")}" +
			s" table=${o.table()}")
		o
	}

	def printHelp() {
		val opts = new CmdLineOpts(Array())
		opts.printHelp()
	}

}

/**
 * Parses command-line arguments
 */
class CmdLineOpts(args: Array[String])
	extends LazyScallopConf(args) {

	val drv = opt[String]("driver", short = 'a',
		descr = "Connect to 'MySQL' or 'Postgres' database",
		default = Some("MySQL"),
		validate = _.toLowerCase match {
			case "postgres" => true
			case "mysql" => true
			case _ => false
		} )

	val host = opt[String]("hostname", short='h',
		descr = "Hostname of the SQL server",
		validate = _.trim.length > 0,
		default = Some("localhost") )

	val user = opt[String]("username", short='u',
		descr = "Username for SQL connection",
		validate = _.trim.length > 0,
		required = true )

	val pass = opt[String]("password", short='p',
		descr = "Password for SQL connection",
		validate = _.trim.length > 0,
		required = true )

	val base = opt[String]("database", short='b',
		descr = "Name of the database",
		validate = _.trim.length > 0,
		required = true )

	val table = opt[String]("table", short='T',
		descr = "SQL table with the dataset",
		default = Some("tbl_atos_transactions"),
		validate = _.trim.length > 0 )

	val ident = opt[String]("ident", short='I',
		descr = "Column with unique transaction identifier",
		default = Some("transactionId"),
		validate = _.trim.length > 0 )

	val deNul = toggle("denull-klass", short='n', prefix="no-",
		descrYes = "Convert NULL to 'none' and other values to 'some'",
		descrNo = "Keep values in 'klass' column intact",
		default = Some(true) )

	val klass = opt[String]("class", short='C',
		descr = "Column with transcation class, used for prediction",
		default = Some("fraudCode"),
		validate = _.trim.length > 0 )

	val stamp = opt[String]("stamp", short='S',
		descr = "Column with transaction time stamp, for preserving causality",
		default = Some("transactionDateTime"),
		validate = _.trim.length > 0 )

	val datas = opt[List[String]]("data", short='d',
		descr = "Additional data columns for classification",
		validate = _.map{_.trim.length > 0}.foldLeft(true){_ && _} )

	val insts = opt[List[String]]("instantiate", short='i',
		descr = "Columns which can be made equal to a value",
		validate = _.map{_.trim.length > 0}.foldLeft(true){_ && _} )

	val joins = opt[List[List[String]]]("join", short='j',
		validate = _.flatMap{_.map{_.trim.length > 0}}.foldLeft(true){_ && _},
		descr = "Join tables via these columns, comma-separated",
		default = Some(List(
			List("terminalId"),
			List("acceptorName"),
			List("issuerId", "cardIssueDate")
		))
	)(new ValueConverter[List[List[String]]] {
		def parse(s:List[(String,List[String])]) = Right(Some(s.map{_._2}))
		val tag = implicitly[TypeTag[List[List[String]]]]
		val argType = ArgType.LIST
	})

	val queryTimeOut = opt[Int]("query-timeout", noshort=true,
		descr = "Timeout for evaluating a single query in seconds." +
			" Queries exceeding the timeout will be specialized.",
		default = Some(300), validate = _ > 0 )

	val queryRowLimit = opt[Int]("query-row-limit", noshort=true,
		descr = "Maximum number of results per query." +
			" Queries exceeding this limit will be specialized." +
			" This value should >> number of instances.",
		default = Some(1000 * 1000), validate = _ > 0 )

	val beamConsNonImp = opt[Int]("beam-terminate", noshort=true,
		descr = "Maximum number beam-search iterations without" +
			" improvement in the score before the search is terminated.",
		default = Some(1), validate = _ > 0 )

	val beamWidth = opt[Int]("beam-width", noshort=true,
		descr = "Beam-search keeps only a number of best candidates," +
			" which are modified during the next iteration.",
		default = Some(3), validate = _ > 0 )

}
