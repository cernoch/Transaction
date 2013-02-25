package cernoch.sm.secret.transaction

import cernoch.scalogic._

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Domains {

  // buf domains with values
  private val buf = collection.mutable.ListBuffer[Domain[_]]()

  val ex = NumDom("transactionId", true)
  //val ex = NumDom("id", true)
  val cl = CatDom("fraudCode",
    allowed = Set("00", "01", "02", "03",
      "04", "05", "06", "07", "08", "09", "51"))

  val acceptorCountryCode = CatDom("acceptorCountryCode",
    allowed = Set("AUT", "FRA", "GBR", "GER", "ITA", "ROM", "RUS", "SPA", "USA"))
  buf += acceptorCountryCode

  val acceptorName = CatDom("acceptorName")
  buf += acceptorName

  val acquirerInstitutionId = CatDom("acquirerInstitutionId")
  buf += acquirerInstitutionId

  val argosTransactionId = CatDom("argosTransactionId", true)
  buf += argosTransactionId

  val billingAmount = DecDom("billingAmount")
  buf += billingAmount

  val billingCurrencyCode = CatDom("billingCurrencyCode",
    allowed = Set("EUR"))
  buf += billingCurrencyCode

  val cardEntryMode = CatDom("cardEntryMode",
    allowed = Set("00", "01", "02", "03", "04", "91", "92"))
  buf += cardEntryMode

  val cardId = CatDom("cardId", true)
  buf += cardId

  val cardIssueDate = NumDom("cardIssueDate")
  buf += cardIssueDate

  val externalResultCode = CatDom("externalResultCode")
  buf += externalResultCode

  val fiid = CatDom("fiid", true)
  buf += fiid

  val internalResultCode = CatDom("internalResultCode")
  buf += internalResultCode

  val issuerId = CatDom("issuerId")
  buf += issuerId

  val mcc = CatDom("mcc",
    allowed = Set("2500", "2600", "3200", "3210", "3220", "4050", "4060", "4500", "4600"))
  buf += mcc

  val posEntryCode = CatDom("posEntryMode",
    allowed = Set("00", "01", "02", "03", "04", "91", "92"))
  buf += posEntryCode

  val realTimeScore = NumDom("realTimeScore")
  buf += realTimeScore

  val retrievalReferenceNumber = CatDom("retrievalReferenceNumber", true)
  buf += retrievalReferenceNumber

  val securityType = NumDom("securityType")
  buf += securityType

  val terminalId = CatDom("terminalId", true)
  buf += terminalId

  val transactionAmount = DecDom("transactionAmount")
  buf += transactionAmount

  val transactionCurrencyCode = CatDom("transactionCurrencyCode",
    allowed = Set("EUR", "RON", "USD"))
  buf += transactionCurrencyCode

  val dt = DecDom("transactionDateTime")
  buf += dt

  val transactionType = CatDom("transactionType")
  buf += transactionType

  // Absolutely buf domains
  def all = ex :: cl :: (buf toList)

  // buf domains without the example ID and class ID
  def sem = buf toList
}
