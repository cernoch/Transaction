package cernoch.sm.transaction

import cernoch.scalogic.sql._
import grizzled.slf4j.Logging
import cernoch.scalogic.{Val, Domain}
import java.sql.Connection

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class DbConnector
	(host: String = "localhost",
	 user: String, pass: String,
	 base: String) extends Logging {

	def toMySQL
	= new MySQLAdaptor(
			host = host, user = user,
			pass = pass, base = base
		) with ConnectionCache
			with QueryLogger
		  with Nullable {

		override def queryLimit = Some(1000 * 1000)
		override def handle(s: String) {
			debug(s"Executing SQL query: $s")
		}
	}

	def toPostgres
	=	new PostgresAdaptor(
			host = host, user = user,
			pass = pass, base = base
		) with ConnectionCache
			with QueryLogger
			with Nullable {

		override def queryLimit = Some(1000 * 1000)
		override def handle(s: String) {
			debug(s"Executing SQL query: $s")
		}
	}



	trait Nullable extends Adaptor {
		var deNull = Set[Domain]()

		override def convFromSQL[T](o:T,d:Domain)
		= if (deNull.contains(d)) {
			val conv = if (o == null) "none" else "some"
			conv.asInstanceOf[T]
		} else o
	}
}
