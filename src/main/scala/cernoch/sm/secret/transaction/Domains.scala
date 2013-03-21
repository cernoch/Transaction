package cernoch.sm.secret.transaction

import cernoch.scalogic.sql._
import cernoch.scalogic._
import grizzled.slf4j.Logging
import collection.mutable.ListBuffer


object Domains extends Logging {

	def apply
		(ada: Adaptor,
		 table: String,
		 ident: String,
		 klass: String,
		 stamp: String,
		 datas: List[String],
		 insts: List[String],
		 joins: List[List[String]])
	= {

		val allNames = Set(ident, klass, stamp) ++ datas ++ insts ++ joins.flatten
		trace(s"Detecting domains for columns: ${allNames.mkString(", ")}.")

		val sniffer = new SchemaSniffer(ada)
		val allDomains = sniffer.detectAtoms(table, allNames)

		allDomains.map(domainAttributes).foreach(dom => {
			val x = dom.map{case (k,v) => s"$k => $v"}.mkString("\n")
			debug("Detected a domain.\n" + x)
		})

		val byName = allDomains.map{d => d.name -> d}.toMap

		val identDom = byName(ident) match {
			case IntDom(_,i) => i
			case _ => throw new ParamException(
				"SQL schema error: Domain of 'ident' column is not of type Integer.")
		}

		val klassDom = byName(klass) match {
			case d: Domain with Iterable[_] => d.head match {
				case _:String => d.asInstanceOf[Domain with Iterable[String]]
				case _ => throw new ParamException(
					"SQL schema error: Domain of 'class'" +
					" column is not of type String.")
			}
			case _ => throw new ParamException(
				"SQL schema error: Domain of 'class'" +
				" column does not have restricted values.")
		}

		val stampDom = byName(stamp) match {
			case i: Domain with Numeric[_] => i
			case _ => throw new ParamException(
				"SQL schema error: Domain of 'stamp'" +
					" column does not have a numeric type.")
		}

		new Domains(identDom, klassDom, stampDom,
			(allDomains - identDom- klassDom- stampDom).toList )
	}

	def domainAttributes(d: Domain) : List[(String,String)] = {
		val o = new ListBuffer[(String,String)]()

		o += "name" -> d.name

		d match {
			case _:Fractional[_] => o += "numeric" -> "fractional"
			case _:Integral[_]   => o += "numeric" -> "integral"
			case _:Numeric[_]    => o += "numeric" -> "numeric"
			case _ =>
		}

		d match {
			case n:Numeric[_]  => o += "numType" -> n.zero.getClass.getName
			case _ =>
		}

		d match {
			case i:Iterable[_] => o += "values" -> i.mkString(", ")
			case _ =>
		}

		o.toList
	}
}

case class Domains
	(ident: Domain with Integral[Int],
	 klass: Domain with Iterable[String],
	 stamp: Domain with Numeric[_],
	 other: List[Domain] )
{
	val all = ident :: klass :: stamp :: other

	val byName = all.map{d => d.name -> d }.toMap
}
