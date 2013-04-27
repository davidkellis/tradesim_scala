package dke.tradesim

import scala.slick.session.Database
import net.sf.ehcache.{Cache, CacheManager}
import net.sf.ehcache.config.{CacheConfiguration}
import net.sf.ehcache.store.MemoryStoreEvictionPolicy
import dke.tradesim.strategies.buyandhold

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

  }

  def cleanupCache() {
    CacheManager.getInstance().shutdown()
  }

  def main(args: Array[String]) {
    setupCaches()

    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
      // The session is never named explicitly. It is bound to the current
      // thread as the threadLocalSession that we imported
      buyandhold.runSingleTrial()
    }

    cleanupCache()
  }

}