package dke.tradesim.stats

import scala.collection.LinearSeq
import spire.implicits._

object sample {
  // returns the sample correlation coefficient (Pearson's r coefficient)
  def correlation(xs: Seq[BigDecimal], ys: Seq[BigDecimal]): BigDecimal = {
    val pairs = xs.zip(ys)
    val onlineCorrelation = new OnlineRegression
    pairs.foreach(pair => onlineCorrelation.push(pair._1, pair._2))
    onlineCorrelation.correlation
  }

  def mean(xs: IndexedSeq[BigDecimal]): BigDecimal = sum(xs) / xs.length

  def mean(xs: LinearSeq[BigDecimal]): BigDecimal = {
    def onlineMean(xs: LinearSeq[BigDecimal], sum: BigDecimal, length: Long): BigDecimal = {
      if (xs.isEmpty) sum / length
      else onlineMean(xs.tail, sum + xs.head, length + 1)
    }
    onlineMean(xs, 0, 0)
  }

  case class OlsResult(slope: BigDecimal, intercept: BigDecimal)
  def ols(xs: Seq[BigDecimal], ys: Seq[BigDecimal]): OlsResult = {
    val pairs = xs.zip(ys)
    val onlineRegression = new OnlineRegression
    pairs.foreach(pair => onlineRegression.push(pair._1, pair._2))
    OlsResult(onlineRegression.slope, onlineRegression.intercept)
  }

  // copied from http://www.johndcook.com/running_regression.html
  class OnlineRegression {
    val xStats = new OnlineVariance
    val yStats = new OnlineVariance
    var S_xy: BigDecimal = 0
    var n: Long = 0

    def push(x: BigDecimal, y: BigDecimal) {
      S_xy += (xStats.mean - x) * (yStats.mean - y) * n / (n + 1)

      xStats.push(x)
      yStats.push(y)
      n += 1
    }

    def slope: BigDecimal = {
      val S_xx = xStats.variance * (n - 1)
      S_xy / S_xx
    }

    def intercept: BigDecimal = yStats.mean - slope * xStats.mean

    def correlation: BigDecimal = {
      val t = xStats.stdDev * yStats.stdDev
      S_xy / ((n - 1) * t)
    }
  }

  // copied from http://www.johndcook.com/standard_deviation.html
  class OnlineVariance {
    var k: Long = 0
    var m_k: BigDecimal = 0
    var s_k: BigDecimal = 0

    // invariant:
    // m_k = m_kMinus1 + (x_k - m_kMinus1) / k
    // s_k = s_kMinus1 + (x_k - m_kMinus1) * (x_k - m_k)
    def push(x: BigDecimal) {
      k += 1

      // See Knuth TAOCP vol 2, 3rd edition, page 232
      if (k == 1) {
        m_k = x
        s_k = 0
      } else {
        val m_kPlus1 = m_k + (x - m_k) / k
        val s_kPlus1 = s_k + (x - m_k) * (x - m_kPlus1)
        m_k = m_kPlus1
        s_k = s_kPlus1
      }
    }

    def n: Long = k

    def mean: BigDecimal = if (k > 0) m_k else 0

    def variance: BigDecimal = if (k > 1) s_k / (k - 1) else 0

    def stdDev: BigDecimal = variance.sqrt
  }

  def sum(xs: Seq[BigDecimal]): BigDecimal = xs.fold(BigDecimal(0))(_ + _)

  def variance(xs: Seq[BigDecimal]): BigDecimal = {
    // onlineVariance based on http://www.johndcook.com/standard_deviation.html
    def onlineVariance(xs: Seq[BigDecimal], m_k: BigDecimal, s_k: BigDecimal, k: Long): BigDecimal = {
      if (xs.isEmpty) {
        if (k > 1) s_k / (k - 1)
        else 0
      } else {
        val kPlus1 = k + 1
        val x_kPlus1 = xs.head
        val m_kPlus1 = m_k + (x_kPlus1 - m_k) / kPlus1
        val s_kPlus1 = s_k + (x_kPlus1 - m_k) * (x_kPlus1 - m_kPlus1)
        onlineVariance(xs.tail, m_kPlus1, s_kPlus1, kPlus1)
      }
    }
    if (xs.isEmpty) 0
    else onlineVariance(xs.tail, xs.head, 0, 1)
  }
}
