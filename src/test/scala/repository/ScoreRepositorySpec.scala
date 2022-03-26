package repository

import doobie.specs2.IOChecker
import org.specs2.mutable.Specification

import ScoreRepository._
import model.db.ScoreTableRowWithoutId

class AnalysisTestSpec
    extends Specification
    with IOChecker
    with RepoSpecSupport {

  implicit val instance = "score"

  val aa =
    ScoreTableRowWithoutId(
      isDeuce = false,
      playerWithAdvantage = None,
      playerThatWon = None,
      p1Score = 0,
      p2Score = 0
    )

  check(ScoreRepository.getScoreSql(1))
  check(ScoreRepository.updateSql(1, aa))
  check(ScoreRepository.insertSql(aa))

}
