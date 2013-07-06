package dke.tradesim

import org.rogach.scallop._
import scala.slick.session.{Database}
import net.sf.ehcache.{Cache, CacheManager}
import net.sf.ehcache.config.{CacheConfiguration}
import net.sf.ehcache.store.MemoryStoreEvictionPolicy
import dke.tradesim.strategies.buyandhold
import dke.tradesim.db.{Adapter, SlickAdapter}
import dke.tradesim.db.SlickAdapter.{AnnualReports, QuarterlyReports, CorporateActions, EodBars}

object Runner {
  def setupCaches() {
    val singletonManager = CacheManager.create()

    val priceHistoryCacheConfiguration = new CacheConfiguration("priceHistoryCache", 32)
      .memoryStoreEvictionPolicy(MemoryStoreEvictionPolicy.LRU)
      .eternal(false)
    singletonManager.addCache(new Cache(priceHistoryCacheConfiguration))

    val corporateActionCacheConfiguration = new CacheConfiguration("corporateActionCache", 32)
      .memoryStoreEvictionPolicy(MemoryStoreEvictionPolicy.LRU)
      .eternal(false)
    singletonManager.addCache(new Cache(corporateActionCacheConfiguration))

    val quarterlyReportHistoryCacheConfiguration = new CacheConfiguration("quarterlyReportHistoryCache", 32)
      .memoryStoreEvictionPolicy(MemoryStoreEvictionPolicy.LRU)
      .eternal(false)
    singletonManager.addCache(new Cache(quarterlyReportHistoryCacheConfiguration))
  }

  def cleanupCache() {
    CacheManager.getInstance().shutdown()
  }

  class RuntimeConfig(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("tradesim 1.0.0 (c) 2013 David K Ellis")
    banner("""Usage: tradesim [--setup-db | --trial-count N]
             |Options:
             |""".stripMargin)

    val setupDb = opt[Boolean](default = Some(false), descr = "Setup the DB tables", noshort = true)
    val trialCount = opt[Int](descr = "Specify the number of trials to run", short = 'n')
  }

  def main(args: Array[String]) {
    setupCaches()

//    MongoAdapter.withAdapter("mongodb://localhost") {
    SlickAdapter.withAdapter("jdbc:postgresql:tradesim", "org.postgresql.Driver") {

      val config = new RuntimeConfig(args)
      if (config.setupDb()) {
        Adapter.threadLocalAdapter.createDb()
      } else if (config.trialCount.isSupplied && config.trialCount() > 0) {
        println(s"run ${config.trialCount()} buy-and-hold trials")
        buyandhold.runSingleTrial()
      }

    }

    cleanupCache()
  }

}