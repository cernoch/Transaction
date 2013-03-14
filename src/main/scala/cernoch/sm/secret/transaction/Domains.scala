package cernoch.sm.secret.transaction

import cernoch.scalogic._
import collection.mutable.ListBuffer

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Domains {

	private val _all = ListBuffer[Domain]()
	private def add[T<:Domain](d:T) = { _all += d; d }

	val ex = Domain.long("transactionId")

	val dt = Domain.long("transactionDateTime")

	/*
	val cl = Domain.cat("fraudCode",
		allowed = Set("00", "01", "02", "03",
			"04", "05", "06", "07", "08", "09",
			"51"))
	*/

	val cl = Domain.cat("fraudCode", allowed = Set("0","1"))


	val acceptorCountryCode = add(Domain.cat("acceptorCountryCode",
		allowed = Set("84","27","47","O84","44","036","R27","372",
			"442","E44","826","724","25","276","04","250","20","07",
			"70","75","82","643","U27","203","208","G72","840","528",
			"37","42","292","03","52","34","A20","30","784","38","78",
			"V80","804","80","818","082","61","A78","05","N75","492",
			"72","380","725","040","756","H27","156","L25","N","S82",
			"L27","616","327","E27","470","62","64","C84","O82","246",
			"\\25","196","H84","O38","032","484","504","376","191","620",
			"056","076","480","344","144","752","578","792","705","703",
			"428","124","702","348","300","764","533","356","170","530",
			"052","780","608","044","410","554","360","642","19","A04") ))

	val acceptorName          = add(Domain.cat("acceptorName"))
	val acquirerInstitutionId = add(Domain.cat("acquirerInstitutionId"))
	val argosTransactionId    = add(Domain.cat("argosTransactionId"))

	val billingAmount       = add(Domain.dec("billingAmount"))
	val billingCurrencyCode = add(Domain.cat("billingCurrencyCode",
		allowed = Set("978","N") ))

	val cardEntryMode = add(Domain.cat("cardEntryMode",
		allowed = Set("0","2","3","1") ))

	val cardId        = add(Domain.cat("cardId"))
	val cardIssueDate = add(Domain.long("cardIssueDate"))
	// TODO: Convert into (Domain with Integral[Date])

	val externalResultCode = add(Domain.cat("externalResultCode",
		allowed = Set("00","51","05","55","13","57","94","N","75","01","04","N7","76","12","03") ))

	val internalResultCode = add(Domain.cat("internalResultCode", allowed = Set(
		"APP_OLW_O","DEC_PF156","DEC_PAB","DEC_PINW","DEC_PINO","DEC_PF271A","DEC_CVV2W",
		"APP_CI00","DEC_PF271C","DEC_PFNA","DEC_PF157ED","DEC_PF55NF","DEC_PF514",
		"APP_BI00","APP_PF156","DEC_PINL","DEC_PINM","DEC_OLW_AWP_ADV","APP_SYS_OLW",
		"REF_OLW_AWP_ADV","PUP_PAB","DEC_CVVW","REF_FR","REF_PAB","APP_PTO_OLW",
		"DEC_CERTW","DEC_BI21","DEC_PF34","DEC_CI05","REF_PF271A","REF_CI01",
		"DEC_BI05","APP_PNA_OLW","DEC_PF52") ))

	val issuerId = add(Domain.cat("issuerId"))
	val fiid     = add(Domain.cat("fiid",
		allowed = Set("523224","5527571","5209132","5527568", "5527561","4018490",
			"5193440","4546183","401849","454618","479292","4792928","401850",
			"400697","5527578","5209138","4546188","533873","548327","4277423",
			"448356","548339","4277422","413585","460315","4277424","417721") ))

	val mcc = add(Domain.cat("mcc"))

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
		allowed = Set("840","978","036","826","208","986","756","643","203",
			"752","784","980","985","156","818","N","344","032","484","376",
			"191","480","144","578","949","124","702","348","356","428","170",
			"780","608","410","554","360","946") ))

	// All domains in the schema
	def all = ex :: cl :: dt :: (_all.toList)

	// Without the exampleID, class and date
	def sem = _all.toList
}
