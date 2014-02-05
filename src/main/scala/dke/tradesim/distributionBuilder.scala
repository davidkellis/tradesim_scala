package dke.tradesim

import scala.collection.immutable.TreeMap

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{GetResult, StaticQuery => Q}

import dke.tradesim.datetimeUtils.{datetime, date, days, timestamp, interspersedDateSeries, currentTime}
import dke.tradesim.database.Tables._
import dke.tradesim.seq.{treeMap}
import dke.tradesim.stats.sample

import Database.dynamicSession

object distributionBuilder {
  abstract class TrialSampleAttribute(val name: String, val trialAttributeExtractorFn: TrialsRow => Option[BigDecimal])
  object Yield extends TrialSampleAttribute("yield", _.`yield`)
  object MaximumFavorableExcursion extends TrialSampleAttribute("mfe", _.mfe)
  object MaximumAdverseExcursion extends TrialSampleAttribute("mae", _.mae)

  def buildMissingTrialSamples(trialSampleAttribute: TrialSampleAttribute) {
    val today = date(currentTime())
    trialSets.foreach { trialSetsRow =>
      val trialsByEndTime = treeMap(trials(trialSetsRow.id), (_:TrialsRow).endTime)

      val recentTrialSampleRow = mostRecentTrialSample(trialSetsRow.id, trialSampleAttribute)
      val mostRecentSampleEndTime = recentTrialSampleRow.map(trialSamplesRow => datetime(trialSamplesRow.endTime))
                                                        .orElse(trialsByEndTime.headOption.map(pair => datetime(pair._2.endTime)))
      mostRecentSampleEndTime.map { mostRecentSampleEndTime =>
        val missingEndDates = interspersedDateSeries(date(mostRecentSampleEndTime).plus(days(1)), today, days(1))

        missingEndDates.foreach { endDate =>
          val subsetOfTrials = trialsByEndTime.until(timestamp(endDate))
          if (!subsetOfTrials.isEmpty)
            createTrialSample(trialSetsRow.id, trialSampleAttribute, subsetOfTrials)
        }
      }
    }
  }

//  implicit val getTrialSetsRowResult = GetResult(r => TrialSetsRow(r.nextInt, r.nextBigDecimalOption, r.nextBigDecimalOption, r.nextBigDecimalOption, r.nextStringOption, r.nextInt))
//  def trialSetsMissingTrialSamples(): Seq[TrialSetsRow] = {
//    val sql = s"""
//        |select tsets.*
//        |from trial_sets tsets
//        |left outer join trial_samples tsamples on tsets.id = tsamples.trial_set_id
//        |group by tsets.id
//        |having count(tsamples.id) == 0
//      """.stripMargin
//    Q.queryNA[TrialSetsRow](sql).list
//  }

  def mostRecentTrialSample(trialSetId: Int, trialSampleAttribute: TrialSampleAttribute): Option[TrialSamplesRow] = {
    TrialSamples.filter(_.trialSetId === trialSetId)
                .filter(_.attribute === trialSampleAttribute.name)
                .sortBy(_.endTime.desc)
                .firstOption
  }

  // it is assumed that trialsOrderedByEndTime is ordered by endTime
  def createTrialSample(trialSetId: Int, trialSampleAttribute: TrialSampleAttribute, trialsByEndTime: TreeMap[Long, TrialsRow]): TrialSamplesRow = {
    val oldestTrial = trialsByEndTime.head._2
    val mostRecentTrial = trialsByEndTime.head._2
    val distribution = Array[Byte]()    // todo, encode the distribution with the DIST distribution format
    val trialCount = trialsByEndTime.size

    val trialsRows = trialsByEndTime.values.toSeq
    val onlineVariance = new sample.OnlineVariance
    onlineVariance.pushAll(trialsRows.flatMap(trialsRow => trialSampleAttribute.trialAttributeExtractorFn(trialsRow)))

    val average = onlineVariance.mean
    val min = onlineVariance.min
    val max = onlineVariance.max
    val percentile10 = Some(BigDecimal(1))      // todo, compute the percentiles
    val percentile20 = Some(BigDecimal(1))
    val percentile30 = Some(BigDecimal(1))
    val percentile40 = Some(BigDecimal(1))
    val percentile50 = Some(BigDecimal(1))
    val percentile60 = Some(BigDecimal(1))
    val percentile70 = Some(BigDecimal(1))
    val percentile80 = Some(BigDecimal(1))
    val percentile90 = Some(BigDecimal(1))

    val row = TrialSamplesRow(0, trialSampleAttribute.name, oldestTrial.startTime, mostRecentTrial.endTime, true, distribution, Some(trialCount), Some(average), min, max, percentile10, percentile20, percentile30, percentile40, percentile50, percentile60, percentile70, percentile80, percentile90, trialSetId)
    val trialSampleId = (TrialSamples returning TrialSamples.map(_.id)).insert(row)
    row
  }

  def trialSets(): Seq[TrialSetsRow] = {
    TrialSets.list()
  }

  def trialSamples(trialSetId: Int): Seq[TrialSamplesRow] = {
    TrialSamples.filter(_.trialSetId === trialSetId).list
  }

  def trials(trialSetId: Int): Seq[TrialsRow] = {
    Trials.filter(_.trialSetId === trialSetId).list
  }
}