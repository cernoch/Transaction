package cernoch.sm.secret.transaction

import cernoch.sm.space.HistBeam
import cernoch.scalogic._
import tools.Labeler

/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class BumpHunting {

  object domain extends (() => List[Domain[_]]) {

    val all = collection.mutable.ListBuffer[Domain[_]]()
    
    all += NumDom("id", true)

    val acceptorCountryCode
    = CatDom("acceptorCountryCode", true,
      Set("AUT", "FRA", "GBR", "GER", "ITA",
          "ROM", "RUS", "SPA", "USA"))

    val acceptorName = CatDom("acceptorName", true)
    val acquirerInstitutionId = CatDom("acquirerInstitutionId", true)
    val argosTransactionId = CatDom("argosTransactionId")
    val billingAmount = DecDom("billingAmount")

    all += DecDom("billingAmount")
    all += CatDom("billingCurrencyCode", true, Set("EUR"))
    all += CatDom("cardEntryMode", true, Set(
      "00", "01", "02", "03", "04", "91", "92"))

    all += CatDom("cardId")
    all += NumDom("cardIssueDate", false)
    all += CatDom("externalResultCode")
    all += CatDom("fiid")
    all += CatDom("fraudCode", true, Set(
      "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "51"))

    all += CatDom("internalResultCode")
    all += CatDom("issuerId")
    all += CatDom("mcc", true, Set("2500",
      "2600", "3200", "3210", "3220",
      "4050", "4060", "4500", "4600"))

    all += CatDom("posEntryMode", true, Set(
      "01", "02", "03", "04", "91", "92"))

    all += NumDom("realTimeScore")

    all += CatDom("retrievalReferenceNumber", true)
    all += NumDom("securityType", true)
    all += CatDom("terminalId", true)
    all += DecDom("transactionAmount")
    all += CatDom("transactionCurrencyCode", true, Set("EUR", "RON", "USD"))
    all += NumDom("transactionDateTime")
    all += NumDom("transactionId")
    all += CatDom("transactionType", true)

    def apply() = all toList
  }

  object schema {

    val starter = createMode(null)

    def createDecl
      (inDomain:Domain[_])
    = "tbl_atos_transactions(" +
      domain().map(domain => {
        (if (domain == inDomain) "+" else "-") +
          domain.name
      })
      .mkString(", ") +
      ")"
    
    def createMode
      (inDomain:Domain[_])
    = Btom(
      createDecl(inDomain),
      domain() toSet)


    val others
    = domain()
      .filter{_.isKey}
      .map(createMode)
  }
}


object BumpHunting {

  def main(args: Array[String])
  : Unit
  = {
    val a = new BumpHunting
    
    println(a.schema.createDecl(null))
    
    a.domain()
      .filter{_.isKey}
      .map(a.schema.createDecl)
      .foreach(println)

    /*
    (a.schema.starter :: a.schema.others)
      .foreach(x => {
        println(x.modeIn + " " + x)
      })
    */
  }
}
