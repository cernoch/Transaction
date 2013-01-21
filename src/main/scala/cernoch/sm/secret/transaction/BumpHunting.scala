package cernoch.sm.secret.transaction

import cernoch._
import scalistics.Hist
import scalogic._
import sm.algebra.{StdDev, Variance, Aggregate}
import sm.space._
import bumphunt.BeamHistSearch
import sm.sql.{LoggingInterceptor, SqlStorage}

/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class BumpHunting {

  object domain extends (() => List[Domain[_]]) {

    val all = collection.mutable.ListBuffer[Domain[_]]()
    
    all += NumDom("id", true)

    val acceptorCountryCode
    = CatDom("acceptorCountryCode", true,
      Set("AUT", "FRA", "GBR", "GER", "ITA",
          "ROM", "RUS", "SPA", "USA"))

    val acceptorName = CatDom("acceptorName", true)
    val acquirerInstitutionId = CatDom("acquirerInstitutionId", true)
    val argosTransactionId = CatDom("argosTransactionId")
    val billingAmount = DecDom("billingAmount")

    all += DecDom("billingAmount")
    all += CatDom("billingCurrencyCode", true, Set("EUR"))
    all += CatDom("cardEntryMode", true, Set(
      "00", "01", "02", "03", "04", "91", "92"))

    all += CatDom("cardId")
    all += NumDom("cardIssueDate", false)
    all += CatDom("externalResultCode")
    all += CatDom("fiid")
    all += CatDom("fraudCode", true, Set(
      "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "51"))

    all += CatDom("internalResultCode")
    all += CatDom("issuerId")
    all += CatDom("mcc", true, Set("2500",
      "2600", "3200", "3210", "3220",
      "4050", "4060", "4500", "4600"))

    all += CatDom("posEntryMode", true, Set(
      "01", "02", "03", "04", "91", "92"))

    all += NumDom("realTimeScore")

    all += CatDom("retrievalReferenceNumber", true)
    all += NumDom("securityType", true)
    all += CatDom("terminalId", true)
    all += DecDom("transactionAmount")
    all += CatDom("transactionCurrencyCode", true, Set("EUR", "RON", "USD"))
    all += NumDom("transactionDateTime")
    all += NumDom("transactionId")
    all += CatDom("transactionType", true)

    def apply() = all toList
  }

  object schema {

    val starter = createMode(null)

    def createDecl
      (inDomain:Domain[_])
    = "tbl_atos_transactions(" +
      domain().map(domain => {
        (if (domain == inDomain) "+" else "-") +
          domain.name
      })
      .mkString(", ") +
      ")"
    
    def createMode
      (inDomain:Domain[_])
    = Btom(
      createDecl(inDomain),
      domain() toSet)


    val others
    = domain()
      .filter{_.isKey}
      .map(createMode)
  }
}


object BumpHunting {

  def printSchema {
    val a = new BumpHunting
    
    println(a.schema.createDecl(null))
    
    a.domain()
      .filter{_.isKey}
      .map(a.schema.createDecl)
      .foreach(println)
  }

  def main(args: Array[String])
  : Unit
  = {
    val bh = new BumpHunting
    import bh.schema._

    val sc = new SqlConnector(user="sm",pass="sm",dtbs="sm")
    //      with LoggingInterceptor

    val ss = new SqlStorage(sc, List(BLC(starter))).open
    val qs = new QueriableAdaptor(ss)

    val hs = new BeamHistSearch((starter :: others) toSet) {

      maxConsNonImp = 1

      override def aggregators = {
        val doubleAggregators
        = new Aggregate[Double]()
        import doubleAggregators._

        List(min, max, sum, mean,
          median, new StdDev())
      }

      def execQuery
        (state: Horn[HeadAtom, Set[Atom[FFT]]])
      = { if (state.head == null)
        throw new TooNew("No query-var in the clause.")
        else qs(state)
      }

      override def stateResult
        (state: Horn[HeadAtom, Set[Atom[FFT]]])
      = try {
        //println()
        //println(state)

        val out = super.stateResult(state)
        val (n,a,h,s) = out

        //println("\t" + n + "\t" + a + "\t" + s + "\t")
        //println("\t" + h)
        out

      } catch {
        case e => {
          //println("\t" + e.getMessage())
          throw e
        }
      }

      override def bestUpdated
        (state: Horn[HeadAtom, Set[Atom[FFT]]],
         result: (Int,String,Hist[Double],Double))
      = {
        val (bins, func, hist, score) = result

        println("===== NEW OPTIMUM =====")
        println("Horn: " + state)
        println("Bins: " + bins)
        println("Func: " + func)
        println("Hist: " + hist)
        println("mALP: " + score)
      }

    }

    val out = hs.start

    for ((clause, (bins, func, hist, score)) <- out) {
      println("Bins: " + bins)
      println("Func: " + func)
      println("Horn: " + clause)
      println("Hist: " + hist)
    }
  }


}
