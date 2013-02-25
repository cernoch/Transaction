package cernoch.sm.secret.transaction

import cernoch.sm.sql.jdbc._
import java.sql.Connection

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Connect(
    user: String = "sm",
    pass: String = "sm",
    dtbs: String = "sm",
    host: String = "localhost") {

  def toMySQL = new MySQLAdaptor(
    host = host, user = user, pass = pass, dtbs = dtbs
  )

  def toPostgres = new PostgresAdaptor(
    host = host, user = user, pass = pass, dtbs = dtbs)
}
