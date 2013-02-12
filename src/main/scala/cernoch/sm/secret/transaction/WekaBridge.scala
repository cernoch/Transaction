package cernoch.sm.secret.transaction

import cernoch.scalogic._
import weka.classifiers.Evaluation
import weka.classifiers.bayes.NaiveBayes
import java.util.Random
import weka.core.{Instance, Instances, Attribute, FastVector}
import cernoch.sm.sql.QueryExecutor

object WekaBridge {

  def supported(d: Domain[_]) = d match {
    case CatDom(_,_,allowed) => !allowed.isEmpty
    case _ => true
  }

  /**
   * Dataset constructed from the given query
   */
  def apply
  ( st: Starter, store: QueryExecutor )
  = {
    val attributes = (st.q.head.clVar :: st.q.head.histVars)
      .filter{v => supported(v.dom)}

    val instances = new Instances(Schema.tableName,
      attributes.map{_.dom}.map(attr), 1000)
    instances.setClassIndex(0)

    for (rowMap <- store.query(st.q)) {
      val instance = new Instance(attributes.size)
      instance.setDataset(instances)

      for (
        (value,col) <- attributes.view
          map rowMap zip (Stream from 0);
        if value.get != null)
        value match {
          case Num(int) => instance.setValue(col, int.toDouble)
          case Dec(dbl) => instance.setValue(col, dbl.toDouble)
          case Cat(str) => instance.setValue(col, str)
        }

      instances.add(instance)
    }
    instances
  }

  /**
   *  Converts iterable to a [[weka.core.FastVector]]
   */
  implicit def toFastVec
  (a: Iterable[Any])
  : FastVector
  = if (a == null) null else {
    val vector = new FastVector(a.size)
    a.foreach(vector.addElement)
    vector
  }

  /**
   * Converts a domain into an attribute
   */
  def attr(d: Domain[_]) : Attribute
  = d match {
    case CatDom(name, key, allow) => new Attribute(name,
      if (allow.isEmpty) null else allow)
    case NumDom(name,_) => new Attribute(name)
    case DecDom(name) => new Attribute(name)
  }



  implicit def inst2enricher(old: Instances) = new DatasetEncap(old)
  class DatasetEncap(old: Instances) {

    /**
     * Adds an attribute to a dataset
     */
    def enrich
    ( domain: Domain[_],
      newValues: Map[Int,Double] )
    = {

      val attributes = new FastVector(old.numAttributes() + 1)
      for (i <- 0 to (old.numAttributes() -1)) {
        attributes.setElementAt(old.attribute(i), i)
      }
      attributes.setElementAt(attr(domain), old.numAttributes())

      val instances = new Instances( old.relationName(),
        attributes, old.numInstances())
      instances.setClassIndex(old.classIndex())

      for (i <- 0 to old.numInstances() - 1) {
        val oi = old.instance(i)
        val ni = new Instance(oi.numAttributes() + 1)
        ni. setDataset(instances)

        for (j <- 0 to oi.numAttributes() - 1) {
          val attr = oi.attribute(j)
          if (attr.isNumeric) ni.setValue(i, oi.value(i))
          if (attr.isString || attr.isNominal) ni.setValue(i, oi.stringValue(i))
        }

        val exampleId = oi.value(0).toInt
        newValues.get(exampleId) match {
          case Some(v) => ni.setValue(oi.numAttributes(), v)
          case _ =>
        }
        /* values(oi.value(0).toInt) match {
          case Num(int) => ni.setValue(oi.numAttributes(), int.toDouble)
          case Dec(dbl) => ni.setValue(oi.numAttributes(), dbl.toDouble)
          case Cat(str) => ni.setValue(oi.numAttributes(), str)
          } */
        instances.add(ni)
      }
      instances
    }

    /**
      * Classifies all instances in the dataset
     */
    def classify = {
      val classifier = new NaiveBayes()

      val eval = new Evaluation(old)
      eval.crossValidateModel(classifier, old, 2, new Random(1))
      eval.pctCorrect()
    }
  }
}
