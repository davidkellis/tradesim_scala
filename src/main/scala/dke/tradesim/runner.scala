package dke.tradesim

import org.rogach.scallop._
import net.sf.ehcache.{CacheManager}
import dke.tradesim.strategies.buyandhold
import dke.tradesim.db.{Adapter, SlickAdapter}

// run this from within sbt like this: sbt "run -s bah4"

object Runner {
  class RuntimeConfig(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("tradesim 1.0.0 (c) 2013 David K Ellis")
    banner("""Usage: tradesim [--jdbc jdbc:postgresql://localhost[:port]/tradesim] [--username <username>] [--password <password>] [--setup-db | --build-trial-samples | --scenario <scenarioName>]
             |Options:
             |""".stripMargin)

    val setupDb = opt[Boolean](default = Some(false), descr = "Setup the DB tables", noshort = true)
    val buildTrialSamples = opt[Boolean](default = Some(false), descr = "Build missing trial samples", noshort = true)
    val scenario = opt[String](descr = "Identify the scenario to run.", short = 's')
    val jdbc = opt[String](descr = "Identify the scenario to run.", short = 'j')
    val username = opt[String](descr = "Database username", short = 'u')
    val password = opt[String](descr = "Database password", short = 'p')
  }

  def main(args: Array[String]) {
    val config = new RuntimeConfig(args)
    val jdbcConnectionString = config.jdbc.get.getOrElse("jdbc:postgresql:tradesim")
    val username = config.username.get.getOrElse("david")
    val password = config.password.get.getOrElse("")

    SlickAdapter.withAdapter(jdbcConnectionString, "org.postgresql.Driver", username, password) {

      if (config.setupDb()) {
        Adapter.dynamicAdapter.createDb()
      } else if (config.buildTrialSamples()) {
        distributionBuilder.buildMissingTrialSamples(distributionBuilder.Yield)
        distributionBuilder.buildMissingTrialSamples(distributionBuilder.MaximumFavorableExcursion)
        distributionBuilder.buildMissingTrialSamples(distributionBuilder.MaximumAdverseExcursion)
      } else if (config.scenario.isSupplied) {
        readLine("Press any key to start")
        config.scenario.get match {
          case Some("bah1") => buyandhold.scenarios.runSingleTrial1()
          case Some("bah2") => buyandhold.scenarios.runSingleTrial2()
          case Some("bah3") => buyandhold.scenarios.runMultipleTrials1()
          case Some("bah4") => buyandhold.scenarios.runMultipleTrials2()
          case Some("bah5") => buyandhold.scenarios.runMultipleTrials3()
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