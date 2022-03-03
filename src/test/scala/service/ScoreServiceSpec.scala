package service

import org.specs2.mutable._
import scala.util.chaining._
import model.Score._


class ScoreSpec extends Specification {
  "Tennis Scoring" should {
    "start" in {
      val g =
        ScoreService.startGame()
      g shouldEqual(NormalScoring(ScoreLove, ScoreLove))
    }

    "P1 Scores" in {
      val g =
        ScoreService.startGame()
        .pipe(ScoreService.score(P1))

      g shouldEqual(NormalScoring(Score15, ScoreLove))
    }

    "P1 Scores Twice" in {
      val g =
        ScoreService.startGame()
        .pipe(ScoreService.score(P1))
        .pipe(ScoreService.score(P1))

      g shouldEqual(NormalScoring(Score30, ScoreLove))
    }

    "1,2,1" in {
      val g =
        ScoreService.startGame()
        .pipe(ScoreService.score(P1))
        .pipe(ScoreService.score(P2))
        .pipe(ScoreService.score(P1))

      g shouldEqual(NormalScoring(Score30, Score15))
    }

    "both scores 3 times" in {
      val g =
        ScoreService.startGame()
        .pipe(ScoreService.score(P1))
        .pipe(ScoreService.score(P2))
        .pipe(ScoreService.score(P1))
        .pipe(ScoreService.score(P2))
        .pipe(ScoreService.score(P1))
        .pipe(ScoreService.score(P2))

      g shouldEqual(Deuce)
    }

    "both scores 3 times, P1" in {
      val g = Deuce.pipe(ScoreService.score(P1))

      g shouldEqual(Advantage(P1))
    }

    "Advantage P1, P2" in {
      val g = Advantage(P1).pipe(ScoreService.score(P2))

      g shouldEqual(Deuce)
    }

    "Advantage P1, P1" in {
      val beforeWin = Advantage(P1)
      val g = beforeWin.pipe(ScoreService.score(P1))
      g shouldEqual(Win(P1))
    }

    "Advantage P2, P2" in {
      val beforeWin = Advantage(P2)
      val g = beforeWin.pipe(ScoreService.score(P2))

      g shouldEqual(Win(P2))
    }

    "P1 Wins" in {
      val beforeWin =
        ScoreService.startGame()
        .pipe(ScoreService.score(P1))
        .pipe(ScoreService.score(P1))
        .pipe(ScoreService.score(P1))

      val g =
        beforeWin
        .pipe(ScoreService.score(P1))

      g shouldEqual(Win(P1))
    }

    "P2 Wins" in {
      val beforeWin =
        ScoreService.startGame()
        .pipe(ScoreService.score(P2))
        .pipe(ScoreService.score(P2))
        .pipe(ScoreService.score(P2))

      val g =
        beforeWin
        .pipe(ScoreService.score(P2))

      g shouldEqual(Win(P2))
    }
  }
}

