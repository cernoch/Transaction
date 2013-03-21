package cernoch.sm.secret.transaction

import cernoch.scalogic.Var

/**
 * Bump-hunting algorithm via ncALP metric
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */

case class Result
	(war: Var, agg: String,
	 dat: WekaBridge,
	 acc: Double )
