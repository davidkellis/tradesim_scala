package dke.tradesim

import org.rogach.scallop._
import scala.slick.session.{Database}
import net.sf.ehcache.{CacheManager}
import dke.tradesim.strategies.buyandhold
import dke.tradesim.db.{Adapter, SlickAdapter}
import dke.tradesim.db.SlickAdapter.{AnnualReports, QuarterlyReports, CorporateActions, EodBars}

object Runner {
  class RuntimeConfig(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("tradesim 1.0.0 (c) 2013 David K Ellis")
    banner("""Usage: tradesim [--setup-db | --scenario <scenarioName>]
             |Options:
             |""".stripMargin)

    val setupDb = opt[Boolean](default = Some(false), descr = "Setup the DB tables", noshort = true)
    val scenario = opt[String](descr = "Identify the scenario to run.", short = 's')
  }

  def main(args: Array[String]) {
    readLine("Press any key to start")

//    MongoAdapter.withAdapter("mongodb://localhost") {
    SlickAdapter.withAdapter("jdbc:postgresql:tradesim", "org.postgresql.Driver") {

      val config = new RuntimeConfig(args)
      if (config.setupDb()) {
        Adapter.threadLocalAdapter.createDb()
      } else if (config.scenario.isSupplied) {
        config.scenario.get match {
          case Some("bah1") => buyandhold.scenarios.runSingleTrial1()
          case Some("bah2") => buyandhold.scenarios.runSingleTrial2()
          case Some("bah3") => buyandhold.scenarios.runMultipleTrials1()
          case Some("bah4") => buyandhold.scenarios.runMultipleTrials2()
          case Some(name) => println(s"Unknown scenario: $name")
          case _ => println("No scenario was specified.")
        }
      }
    }

    cleanupCache()
  }

  def cleanupCache() {
    CacheManager.getInstance().shutdown()
  }
}