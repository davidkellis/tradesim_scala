package dke.tradesim

import scala.slick.session.{Database}
import net.sf.ehcache.{Cache, CacheManager}
import net.sf.ehcache.config.{CacheConfiguration}
import net.sf.ehcache.store.MemoryStoreEvictionPolicy
import dke.tradesim.strategies.buyandhold
import dke.tradesim.db.{SlickAdapter}

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

  case class RuntimeConfig(var trialCount: Int, var setupDb: Boolean)

  def main(args: Array[String]) {
    setupCaches()

//    MongoAdapter.withAdapter("mongodb://localhost") {
    SlickAdapter.withAdapter("jdbc:postgresql:tradesim", "org.postgresql.Driver") {

      val config = RuntimeConfig(0, false)
      val parser = new scopt.mutable.OptionParser("tradesim", "1.0") {
        intOpt("n", "trialCount", "trialCount is the number of trials to run per simulation", { v: Int => config.trialCount = v })

//        opt("o", "output", "<file>", "output is a string property", { v: String => config.bar = v })

        booleanOpt("setup-db", "Sets up the database", { v: Boolean => config.setupDb = v })

//        keyValueOpt("l", "lib", "<libname>", "<filename>", "load library <libname>",
//          {(key: String, value: String) => { config.libname = key; config.libfile = value } })
//
//        arg("<singlefile>", "<singlefile> is an argument", { v: String => config.whatnot = v })

        // arglist("<file>...", "arglist allows variable number of arguments",
        //   { v: String => config.files = (v :: config.files).reverse })
      }
      if (parser.parse(args)) {
        config match {
          case RuntimeConfig(_, true) =>
            println("setup db")
          case RuntimeConfig(trialCount, _) =>
            println(s"run $trialCount trials")
          //      buyandhold.runSingleTrial()
        }
      } else {
        // arguments are bad, usage message will have been displayed
      }
    }

    cleanupCache()
  }

}