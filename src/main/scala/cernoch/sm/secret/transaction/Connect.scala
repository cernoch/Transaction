package cernoch.sm.secret.transaction

import cernoch.sm.sql.jdbc._
import java.sql.{PreparedStatement, Connection}
import cernoch.scalogic.Val
import grizzled.slf4j.Logging

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
			host = host, user = user, pass = pass, dtbs = base
		) {
			override protected def prepare
			(con: Connection,
			 sql: String, arg: List[Val] = List())
			: PreparedStatement
			= {
				println(s"SQL query: ${sql} with arguments ${arg.mkString(",")}.")
				super.prepare(con,sql,arg)
			}
		}
	}

	def toPostgres = {
		new org.postgresql.Driver()
		new PostgresAdaptor(
			host = host, user = user, pass = pass, dtbs = base, prefix="")
	}
}
