package cernoch.sm.secret.transaction

import cernoch.scalogic._
import weka.classifiers.Evaluation
import weka.classifiers.bayes.NaiveBayes
import java.util.Random
import weka.core.{Instance, Instances, Attribute, FastVector}
import cernoch.sm.sql.QueryExecutor
import cernoch.sm.sql.Tools._
import collection.mutable
import java.lang.IllegalArgumentException
import scala.IllegalArgumentException
import grizzled.slf4j.Logging


class WekaBridge[Index]
	(ralation: String = "table", atts: List[Var],
	 var names: Map[Var,String] = null)
	extends Logging {

	if (names == null)
		names = name(atts){_.dom.name}

	if (atts.size <= 1) throw new IllegalArgumentException(
		"At least 2 attributes needed: 1 for class and 1 for data.")

	/** The dataset itself */
	private val wekaSet
	= new Instances(Schema.tableName,
		atts.map(createAttribute), 1000)
	wekaSet.setClassIndex(0)


	/** Maps each column index to its row in the dataset */
	private var instIdx = new mutable.ListBuffer[Index]()

	/** Converts a domain into an attribute */
	protected def createAttribute(v: Var)
	= v.dom match {
		case _:Numeric[_]  => new Attribute(names(v))
		case i:Iterable[_] => new Attribute(names(v), toFastVec(i))
		case _ => new Attribute(names(v))
	}

	/** Add new instance into the dataset */
	def add(id: Index, row: Map[Var,Val]) = {
		val instance = new Instance(atts.size)
		instance.setDataset(wekaSet)

		for (
			(wal,col) <- atts.view map row.get zip (Stream from 0)
			if wal.isDefined && wal.get.value != null
		) {
			wal.get match {
				case n:Num[_] => instance.setValue(col, n.dom.toDouble(n.get.get))
				case d:Dec[_] => instance.setValue(col, d.dom.toDouble(d.get.get))
				case c:Cat[_] => {
					try {
						instance.setValue(col, c.get.map{_.toString()}.orNull)
					} catch {
						case t: IllegalArgumentException => {
							throw new IllegalArgumentException(
								s"Value ${c.get} is not part of the schema for column ${wal.get.dom}", t)
						}
					}
				}
			}
		}
		wekaSet.add(instance)
		instIdx += id
	}



	/** Adds an attribute to a dataset */
	def enrich( attr: Var, vals: Map[Index,Double] )
	= {
		val neu = new WekaBridge[Index]( atts = atts ::: List(attr) )
		neu.instIdx.clear()
		neu.instIdx ++= instIdx

		for (i <- 0 to wekaSet.numInstances() - 1) {
			val oi = wekaSet.instance(i)
			val ni = new Instance(oi.numAttributes() + 1)
			ni. setDataset(neu.wekaSet)

			for (j <- 0 to oi.numAttributes() - 1) {
				val attr = oi.attribute(j)
				if (attr.isNumeric) ni.setValue(j, oi.value(j))
				if (attr.isString || attr.isNominal)
					ni.setValue(j, oi.stringValue(j))
			}

			vals.get(instIdx(i)) match {
				case Some(dbl) => ni.setValue(oi.numAttributes(), dbl)
				case None =>
			}
			neu.wekaSet.add(ni)
		}
		neu
	}

	/** Converts any iterable to a [[weka.core.FastVector]] */
	implicit def toFastVec(a: Iterable[Any])
	= if (a == null) null else {
		val vector = new FastVector(a.size)
		a.foreach(vector.addElement)
		vector
	}
}



object WekaBridge {

	/** Dataset constructed from the given query */
	def apply(st: State, store: QueryExecutor)
	= {
		val wb = new WekaBridge[Int](
			atts = (st.head.klass :: st.head.others)
				.filter{v => supported(v.dom)})

		store.query(st.horn, mapa =>
			wb.add(mapa(st.head.exId).value.asInstanceOf[Int], mapa))

		wb
	}

	/** Classifies all instances in the dataset */
	def classify(w: WekaBridge[_]) = {
		val eval = new Evaluation(w.wekaSet)
		eval.crossValidateModel(new NaiveBayes(),
			w.wekaSet, 2, new Random(1))
		eval.pctCorrect()
	}

	/** Is a domain supported? */
	def supported(d: Domain) = d match {
		case i:Iterable[_] => !i.isEmpty
		case _:Numeric[_] => true
		case _ => false
	}
}
