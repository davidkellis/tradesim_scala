package dke.tradesim

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{GetResult, StaticQuery => Q}

import dke.tradesim.database.Tables._

import Database.dynamicSession

object distributionBuilder {
  def buildMissingTrialSamples() {
    trialSets.foreach { trialSetsRow =>
      val trialsRows = trials(trialSetsRow)
      val trialSamplesRows = trialSamples(trialSetsRow)
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

  def trialSets(): Seq[TrialSetsRow] = {
    TrialSets.list()
  }

  def trialSamples(trialSet: TrialSetsRow): Seq[TrialSamplesRow] = {
    TrialSamples.filter(_.trialSetId === trialSet.id).list
  }

  def trials(trialSet: TrialSetsRow): Seq[TrialsRow] = {
    Trials.filter(_.trialSetId === trialSet.id).list
  }
}