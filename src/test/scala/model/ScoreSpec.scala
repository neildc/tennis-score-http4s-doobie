package model

import org.specs2.mutable._
import scala.util.chaining._


class ScoreSpec extends Specification {
  "Tennis Scoring" should {
    "start" in {
      val g = model.startGame()
      g shouldEqual(model.NormalScoring(ScoreLove, ScoreLove))
    }

    "P1 Scores" in {
      val g = model
        .startGame()
        .pipe(model.score(model.P1))

      g shouldEqual(model.NormalScoring(Score15, ScoreLove))
    }

    "P1 Scores Twice" in {
      val g = model
        .startGame()
        .pipe(model.score(model.P1))
        .pipe(model.score(model.P1))

      g shouldEqual(model.NormalScoring(Score30, ScoreLove))
    }

    "1,2,1" in {
      val g = model
        .startGame()
        .pipe(model.score(model.P1))
        .pipe(model.score(model.P2))
        .pipe(model.score(model.P1))

      g shouldEqual(model.NormalScoring(Score30, Score15))
    }

    "both scores 3 times" in {
      val g = model
        .startGame()
        .pipe(model.score(model.P1))
        .pipe(model.score(model.P2))
        .pipe(model.score(model.P1))
        .pipe(model.score(model.P2))
        .pipe(model.score(model.P1))
        .pipe(model.score(model.P2))

      g shouldEqual(model.Deuce)
    }

    "both scores 3 times, P1" in {
      val g = model.Deuce.pipe(model.score(model.P1))

      g shouldEqual(model.Advantage(model.P1))
    }

    "Advantage P1, P2" in {
      val g = model.Advantage(model.P1).pipe(model.score(model.P2))

      g shouldEqual(model.Deuce)
    }

    "Advantage P1, P1" in {
      val beforeWin = model.Advantage(model.P1)
      val g = beforeWin.pipe(model.score(model.P1))
      g shouldEqual(model.Win(model.P1))
    }

    "Advantage P2, P2" in {
      val beforeWin = model.Advantage(model.P2)
      val g = beforeWin.pipe(model.score(model.P2))

      g shouldEqual(model.Win(model.P2))
    }

    "P1 Wins" in {
      val beforeWin = model
        .startGame()
        .pipe(model.score(model.P1))
        .pipe(model.score(model.P1))
        .pipe(model.score(model.P1))

      val g =
        beforeWin
        .pipe(model.score(model.P1))

      g shouldEqual(model.Win(model.P1))
    }

    "P2 Wins" in {
      val beforeWin = model
        .startGame()
        .pipe(model.score(model.P2))
        .pipe(model.score(model.P2))
        .pipe(model.score(model.P2))

      val g =
        beforeWin
        .pipe(model.score(model.P2))

      g shouldEqual(model.Win(model.P2))
    }
  }
}

