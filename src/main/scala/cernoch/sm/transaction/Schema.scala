package cernoch.sm.transaction

import cernoch.scalogic._
import scala.Predef._

object Schema {

	def apply
		(domains: Domains,
		 table: String,
		 datas: List[String],
		 insts: List[String],
		 joins: List[List[String]])
	= {

		val dataDoms = datas
			.map{domains.byName}
			.map{ _ match {
				case d: Domain with Numeric[_] => d
				case d => throw new ParamException(s"Domain '${d.name}'" +
					" is used for classification, but it is not numeric." )
			}}

		val instDoms = insts
			.map{domains.byName}
			.map{_ match {

			case i: Domain with Iterable[_] => i.isEmpty match {

				case false => i.head match {
					case _:String => i.asInstanceOf[Domain with Iterable[String]]
					case _ => throw new ParamException(s"Domain '${i.name}'" +
						s" has values for instantiations, but not Strings." +
						s" This looks like an internal error.")
				}

				case true => throw new ParamException(
					s"Domain '${i.name}' has no values for instantiation." )
			}

			case d => throw new ParamException(
				s"Domain '${d.name}' has no values for instantiation.")
		}}

		val joinDoms = joins.map{_.map{domains.byName}}

		() => new Schema(domains, table, dataDoms, instDoms, joinDoms)
	}
}

/**
 * Defines the language bias for the search
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Schema
	(domains: Domains,
	 val table: String,
	 val datas: List[Domain with Numeric[_]],
	 val insts: List[Domain with Iterable[String]],
	 val joins: List[List[Domain]]
) {
	val ident = Var(domains.ident)
	val klass = Var(domains.klass)
	val stamp = Var(domains.stamp)

	val other = domains.other.map{Var(_)}

	val byDom = (
			ident.dom -> ident ::
			klass.dom -> ident ::
			stamp.dom -> ident ::
			other.map{d => d.dom -> d}
		).toMap

	val atom = Atom(table, ident :: klass :: stamp :: other)

	val modes = joins
		.map{_.map(byDom).toSet}
		.map{Mode(atom,_)}
}
