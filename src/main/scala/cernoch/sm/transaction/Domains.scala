package cernoch.sm.transaction

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
		 deNul: Boolean,
		 stamp: String,
		 datas: List[String],
		 insts: List[String],
		 joins: List[List[String]])
	= {

		val allNames = Set(ident, klass, stamp) ++ datas ++ insts ++ joins.flatten
		trace(s"Detecting domains for columns: ${allNames.mkString(", ")}.")

		val sniffer = new SchemaSniffer(ada)
		val allDomains = sniffer.detectAtoms(table, allNames).toSet.map{d:Domain =>
				deNul match {
					case false => d
					case true => if (klass != d.name) d else
						StrDom.Limited(klass, Set("none","some"))
				}
			}


		allDomains.map(domainAttributes).foreach(dom => {
			val x = dom.map{case (k,v) => s"$k => $v"}.mkString(", ")
			info("Detected a domain: " + x)
		})

		val byName = allDomains.map{d => d.name -> d}.toMap

		//info("Class of 'ident' domain is " + byName(ident).getClass.getName)
		val identDom = byName(ident)

		val klassDom = byName(klass) match {
			case d: Domain with Iterable[_] => d.head match {
				case _:String => d.asInstanceOf[Domain with Iterable[String]]
				case _ => throw new ParamException("SQL schema error:" +
					" Domain of 'class' column is not of type String.")
			}
			case _ => throw new ParamException("SQL schema error:" +
				" Domain of 'class' column does not have restricted values.")
		}

		val stampDom = byName(stamp)
		if (stampDom.isInstanceOf[Numeric[_]]) warn("Domain of" +
			s" 'stamp' ($stamp) column does not have a numeric type.")

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
	(ident: Domain,
	 klass: Domain with Iterable[String],
	 stamp: Domain,
	 other: List[Domain] )
{
	val all = ident :: klass :: stamp :: other

	val byName = all.map{d => d.name -> d }.toMap
}
