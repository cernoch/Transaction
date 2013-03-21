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

	val drv = opt[String]("driver", short = 's',
		descr = "Connect to 'MySQL' or 'Postgres' database",
		default = Some("MySQL"),
		validate = _.toLowerCase match {
			case "postgres" => true
			case "mysql" => true
			case _ => false
		}
	)

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

	val table = opt[String]("table", short='t',
		descr = "SQL table with the dataset",
		default = Some("tbl_atos_transactions"),
		validate = _.trim.length > 0 )

	val ident = opt[String]("ident", short='i',
		descr = "Column with unique transaction identifier",
		default = Some("transactionId"),
		validate = _.trim.length > 0 )

	val deNul = toggle("denull-ident", short='n',
		descrYes = "Convert NULL to 'none' and other values to 'some'",
		descrNo = "Keep values in 'ident' column intact" )

	val klass = opt[String]("class", short='c',
		descr = "Column with transcation class (used for prediction)",
		default = Some("fraudCode"),
		validate = _.trim.length > 0 )

	val stamp = opt[String]("stamp", short='s',
		descr = "Column with transaction time stamp (for preserving causality)",
		default = Some("transactionDateTime"),
		validate = _.trim.length > 0 )

	val datas = opt[List[String]]("data", short='d',
		descr = "Additional data columns for classification",
		validate = _.map{_.trim.length > 0}.foldLeft(true){_ && _} )

	val insts = opt[List[String]]("instantiate", short='i',
		descr = "Columns which can be made equal to a value",
		validate = _.map{_.trim.length > 0}.foldLeft(true){_ && _} )

	val joins = opt[List[List[String]]]("joinVia", short='j',
		validate = _.flatMap{_.map{_.trim.length > 0}}.foldLeft(true){_ && _},
		descr = "Join tables via these columns (comma-separated)",
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
}
