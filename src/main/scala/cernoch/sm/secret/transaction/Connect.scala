package cernoch.sm.secret.transaction

import cernoch.scalogic.sql._
import grizzled.slf4j.Logging
import cernoch.scalogic.Domain

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Connect
	(host: String = "localhost",
	 user: String,
	 pass: String,
	 base: String) extends Logging {

	def toMySQL = {
		new com.mysql.jdbc.Driver()
		new MySQLAdaptor(
			host = host, user = user,
			pass = pass, base = base
		) with QueryLogger with Nullable
	}

	def toPostgres = {
		new org.postgresql.Driver()
		new PostgresAdaptor(
			host = host, user = user,
			pass = pass, base = base
		) with QueryLogger with Nullable
	}

	trait Nullable extends Adaptor {
		var deNull = Set[Domain]()

		override def convFromSQL[T](o:T,d:Domain)
		= if (deNull.contains(d) && (o==null || o.isInstanceOf[String])) {
			(if (o == null) "none" else "some").asInstanceOf[T]
		} else o
	}
}
