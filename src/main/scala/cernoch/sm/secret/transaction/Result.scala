package cernoch.sm.secret.transaction

import cernoch.scalogic.Var
import java.text.{DecimalFormat, ParsePosition, FieldPosition, NumberFormat}
import cernoch.scalogic.sql.JoinModel

/**
 * Bump-hunting algorithm via ncALP metric
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */

case class Result
	(war: Var, agg: String,
	 dat: WekaBridge, acc: Double,
	 joinModel: JoinModel) {

	lazy val acc2print
	= Result.Format.format(acc) + "%"
}

object Result {
	val Format = new DecimalFormat("#0.00")
}