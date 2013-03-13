package cernoch.sm.secret.transaction

import cernoch.scalogic._
import cernoch.sm.secret.transaction.Domains._
import scala.Predef._

/**
 * Defines the language bias for the search
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Schema {
	import Domains._

	var tableName = "tbl_atos_transactions"

	private val joints
	: List[Set[Domain]]
	= List(
		Set(acceptorName, transactionType),
		Set(billingCurrencyCode),
		Set(issuerId),
		Set(terminalId),
		Set(acceptorCountryCode, cardEntryMode),
		Set(transactionType, transactionCurrencyCode)
	)

	val domWithValue: List[Domain with Numeric[_]] = List(
		billingAmount,
		cardIssueDate,
		transactionAmount
	)

	val instantiable: List[Domain with Iterable[String]]
	= List(mcc)



	val atom = Atom(tableName, all.map{Var(_)})
  val starter = Mode(atom)
  val others = joints.map{joint => {
		// Immitate the Prolog's copy_term
		val copied = atom.subst( atom.vars
			.map{v => v -> Var(v.dom) }
			.toMap[Term,Term].get(_)
		)
		val iVar = copied.args
			.collect{case v: Var => Var(v.dom)}
			.filter{arg => joint.contains(arg.dom)}
		Mode(copied, iVar.toSet)
	}}
}
