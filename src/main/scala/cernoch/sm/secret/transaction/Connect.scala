package cernoch.sm.secret.transaction

import cernoch.sm.sql.jdbc._

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Connect(
    user: String = "sm",
    pass: String = "sm",
    dtbs: String = "sm",
    host: String = "localhost",
    port: Int = 3306)
  extends MySQLAdaptor(
    host,port,user,pass,dtbs,"") {


}
