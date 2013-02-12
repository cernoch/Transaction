package cernoch.sm.secret.transaction

import cernoch.scalogic._
import tools.StringUtils._

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Schema {

  private def allDomains = Domains.all

  private def declaration(inDomain:Domain[_])
  = Schema.tableName + mkStringIfNonEmpty(
      allDomains.map(d =>
        (d == inDomain match {
          case true => "+"
          case false => "-"
        }) +  (d.name))
  )( "(", ", ", ")" )

  private def createMode(inDomain: Domain[_] = null)
  = Btom(
    declaration(inDomain),
    Domains.all toSet
  )

  val starter = createMode()
  val others = Domains.all map createMode
}


object Schema {
  def tableName = "tbl_atos_transactions"
}
