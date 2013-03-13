package cernoch.sm.secret.transaction

import cernoch.scalogic._
import collection.mutable.ListBuffer

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Domains {

	private val _all = ListBuffer[Domain]()
	private def add[T<:Domain](d:T) = { _all += d; d }

	val ex = Domain.int("transactionId")

	val dt = Domain.long("transactionDateTime")

	val cl = Domain.cat("fraudCode",
		allowed = Set("00", "01", "02", "03",
			"04", "05", "06", "07", "08", "09", "51"))


	val acceptorCountryCode = add(Domain.cat("acceptorCountryCode",
		allowed = Set("AUT", "FRA", "GBR", "GER", "ITA", "ROM", "RUS", "SPA", "USA") ))

	val acceptorName          = add(Domain.cat("acceptorName"))
	val acquirerInstitutionId = add(Domain.cat("acquirerInstitutionId"))
	val argosTransactionId    = add(Domain.cat("argosTransactionId"))

	val billingAmount       = add(Domain.dec("billingAmount"))
	val billingCurrencyCode = add(Domain.cat("billingCurrencyCode",
		allowed = Set("EUR") ))

	val cardEntryMode = add(Domain.cat("cardEntryMode",
		allowed = Set("00", "01", "02", "03", "04", "91", "92") ))

	val cardId        = add(Domain.cat("cardId"))
	val cardIssueDate = add(Domain.long("cardIssueDate"))
	// TODO: Convert into (Domain with Integral[Date])

	val externalResultCode = add(Domain.cat("externalResultCode"))
	val internalResultCode = add(Domain.cat("internalResultCode"))

	val issuerId = add(Domain.cat("issuerId"))
	val fiid     = add(Domain.cat("fiid"))
	val mcc      = add(Domain.cat("mcc", allowed = Set(
		"2500", "2600", "3200", "3210", "3220", "4050", "4060", "4500", "4600") ))

	/*
  val posEntryCode = add(Domain.cat("posEntryMode",
    allowed = Set("00", "01", "02", "03", "04", "91", "92") ))
	*/

	val realTimeScore = add(Domain.bigNum("realTimeScore"))
	val retrievalReferenceNumber = add(Domain.cat("retrievalReferenceNumber"))

	val securityType = add(Domain.bigNum("securityType"))
	val terminalId   = add(Domain.cat("terminalId"))

	val transactionType         = add(Domain.cat("transactionType"))
	val transactionAmount       = add(Domain.dec("transactionAmount"))
	val transactionCurrencyCode = add(Domain.cat("transactionCurrencyCode",
		allowed = Set("EUR", "RON", "USD")))

	// All domains in the schema
	def all = ex :: cl :: dt :: (_all.toList)

	// Without the exampleID, class and date
	def sem = _all.toList
}
