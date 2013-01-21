package cernoch.sm.secret.transaction

import cernoch.sm.sql._


/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class SqlConnector(
    host: String = "localhost",
    port: Int = 3306,
    user: String,
    pass: String,
    dtbs: String)

  extends MySQLAdaptor(host,port,user,pass,dtbs,"") {

  override def escapeTable(s:String) = quote(s)
  override def escapeColumn(s: String) = quote(s)

}
