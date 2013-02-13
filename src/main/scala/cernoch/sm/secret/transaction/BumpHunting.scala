package cernoch.sm.secret.transaction

import cernoch._
import scalogic._
import sm.sql.{QueryLogger, SqlStorage}
import WekaBridge._

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object BumpHunting {

  val domains = Domains all
  val schema = new Schema

  def main(args: Array[String])
  : Unit
  = {

    val sc = new Connect().toPostgres
    val ss = new SqlStorage(sc, List(schema.starter)).open
    val st = new Starter(schema.starter)

    val bl = WekaBridge(st,ss)
    println("Baseline accuracy = " +
      math.round(WekaBridge.classify(bl)) + "%")

    val ch = schema.others.toSet[Btom[FFT]]
    val nb = new NaiveBayesSearch(st,ch,ss,bl)

    println(nb.start)
  }
}
