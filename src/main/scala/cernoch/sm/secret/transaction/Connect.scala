package cernoch.sm.secret.transaction

import cernoch.sm.sql.jdbc._

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Connect
	(host: String = "localhost",
	 user: String,
	 pass: String,
	 base: String) {

	def toMySQL = {
		new com.mysql.jdbc.Driver()
		new MySQLAdaptor(
			host = host, user = user, pass = pass, dtbs = base
		)
	}

	def toPostgres = {
		new org.postgresql.Driver()
		new PostgresAdaptor(
			host = host, user = user, pass = pass, dtbs = base, prefix="")
	}
}
