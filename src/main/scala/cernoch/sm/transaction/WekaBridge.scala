package cernoch.sm.transaction

import cernoch.scalogic._
import cernoch.scalogic.sql.SqlExecutor
import cernoch.scalogic.sql.Tools._

import weka.classifiers.Evaluation
import weka.classifiers.bayes.NaiveBayes
import weka.core.{Instance, Instances, Attribute, FastVector}

import java.util.Random
import collection.mutable
import grizzled.slf4j.Logging


class WekaBridge
	(attributes: List[Var])
	extends Logging
{

	val names = name(attributes){_.dom.name}

	if (attributes.size <= 1) throw new IllegalArgumentException(
		"At least 2 attributes needed: 1 for class and 1 for data.")

	/** The Weka dataset */
	private val wekaSet = new Instances(
		"transaction", attributes.map(createAttribute), 1000 )
	wekaSet.setClassIndex(0)

	/** Maps each column index to its row in the dataset */
	private var instIdx = new mutable.ListBuffer[Val]()

	/** Converts a domain into an attribute */
	protected def createAttribute(v: Var)
	= v.dom match {
		case _:Numeric[_]  => new Attribute(names(v))
		case i:Iterable[_] => new Attribute(names(v), toFastVec(i))
		case _ => new Attribute(names(v))
	}

	/** Add new instance into the dataset */
	def add(ident: Val, result: Map[Var,Val]) = {
		val instance = new Instance(attributes.size)
		instance.setDataset(wekaSet)

		for (
			(wal,col) <- attributes.view map result.get zip (Stream from 0)
			if wal.isDefined && wal.get.value != null
		) {
			wal.get match {
				case NumVal(v,n) => instance.setValue(col, n.toDouble(v))
				case StrVal(v,i)
				=>try { instance.setValue(col, Option(v).map{_.toString()}.orNull) }
				catch { case t: IllegalArgumentException => {
					info(s"Ignoring value '$v' in ex. $ident, bacause" +
						s" not part of '${wal.get.dom.name}' domain.", t)
					instance.setMissing(col)
				}}
			}
		}
		wekaSet.add(instance)
		instIdx += ident
	}



	/** Adds an attribute to a dataset */
	def enrich( attr: Var, vals: Map[Val,Double] )
	= {
		val neu = new WekaBridge(attributes ::: List(attr))
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
		a.foreach{vector.addElement(_)}
		vector
	}
}



object WekaBridge extends Logging {

	/** Dataset constructed from the given query */
	def apply(st: State, store: SqlExecutor)
	= {
		// New dataset with all variables, klass first
		val wekaBridge = new WekaBridge(
			(st.head.klass :: st.head.others)
				.filter{v => supported(v.dom)}
		)
		// Load all instances from the database
		store.query(st.horn, mapa => {

			val ident = mapa(st.head.exId)
			val klass = mapa(st.head.klass)

			// Cannot recognize if klass is null
			if (klass.value == null)
				warn(s"Example '$ident' ignored, its class is null.")
			else
				wekaBridge.add(ident,mapa)
		})
	}

	/** Classifies all instances in the dataset */
	def classify(w: WekaBridge) = {
		val eval = new Evaluation(w.wekaSet)

		eval.crossValidateModel(
			new NaiveBayes(),
			w.wekaSet,
			2,
			new Random(1)
		)
		eval.pctCorrect()
	}

	/** Is a domain supported? */
	def supported(d: Domain) = d match {
		case i:Iterable[_] => !i.isEmpty
		case _:Numeric[_] => true
		case _ => false
	}
}
