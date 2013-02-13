package cernoch.sm.secret.transaction

import cernoch.scalogic._
import weka.classifiers.Evaluation
import weka.classifiers.bayes.NaiveBayes
import java.util.Random
import weka.core.{Instance, Instances, Attribute, FastVector}
import cernoch.sm.sql.QueryExecutor
import cernoch.sm.sql.Tools._
import collection.mutable


class WekaBridge[Index]
  ( ralation: String = "table", atts: List[Var],
    var names: Map[Var,String] = null) {

  if (names == null)
    names = name(atts){_.dom.name}

  if (atts.size < 2) throw new IllegalArgumentException(
    "At least 2 attributes neede: 1 for class and 1 for data.")

  /** The dataset itself */
  private val wekaSet
  = new Instances(Schema.tableName,
    atts.map(createAttribute), 1000)
  wekaSet.setClassIndex(0)


  /** Maps each column index to its row in the dataset */
  private var instIdx = new mutable.ListBuffer[Index]()

  /** Converts a domain into an attribute */
  protected def createAttribute(v: Var)
  = {
    val name = names(v)
    v.dom match {
    case DecDom(_) => new Attribute(name)
    case NumDom(_,_) => new Attribute(name)
    case CatDom(_, _, allow) => new Attribute(name,
      if (allow.isEmpty) null else toFastVec(allow))
  }}

  /** Add new instance into the dataset */
  def add(id: Index, row: Map[Var, Val[_]]) = {
    val instance = new Instance(atts.size)
    instance.setDataset(wekaSet)

    for (
      (value,col) <- atts.view map row.get zip (Stream from 0);
      if value.isDefined && value.get.get != null
    ) value.get match {
      case Num(int) => instance.setValue(col, int.toDouble)
      case Dec(dbl) => instance.setValue(col, dbl.toDouble)
      case Cat(str) => instance.setValue(col, str)
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
        case Some(dbl)
        => ni.setValue(oi.numAttributes(), dbl)
        /*case Some(Num(int)) => ni.setValue(oi.numAttributes(), int.toDouble)
        case Some(Dec(dbl)) => ni.setValue(oi.numAttributes(), dbl.toDouble)
        case Some(Cat(str)) => ni.setValue(oi.numAttributes(), str)*/
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
  def apply( st: Starter, store: QueryExecutor )
  = {
    val wb = new WekaBridge[Val[_]](
      atts = (st.q.head.clVar :: st.q.head.histVars)
        .filter{v => supported(v.dom)})

    store.query(st.q, mapa =>
      wb.add(mapa(st.q.head.exVar), mapa))

    wb
  }

  /** Classifies all instances in the dataset */
  def classify(w: WekaBridge[_]) = {
    try {
      val eval = new Evaluation(w.wekaSet)
      eval.crossValidateModel(new NaiveBayes(),
        w.wekaSet, 2, new Random(1))
      eval.pctCorrect()
    } catch {
      case e => {
        println(w.wekaSet)
        throw new RuntimeException(e)
      }
    }
  }

  /** Is a domain supported? */
  def supported(d: Domain[_]) = d match {
    case CatDom(_,_,allowed) => !allowed.isEmpty
    case _ => true
  }
}
