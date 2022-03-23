package service

import model._

object ScoreService {
  def startGame(): State = NormalScoring((ScoreLove, ScoreLove))

  def score(player: Player)(state: State): State = {
    (player, state) match {
      case (P1, NormalScoring((Score30, Score40))) =>
        Deuce

      case (P2, NormalScoring((Score40, Score30))) =>
        Deuce

      case (P2, NormalScoring((_, Score40))) =>
        Win(P2)

      case (P1, NormalScoring((Score40, _))) =>
        Win(P1)

      case (P1, NormalScoring((p1Score, p2Score))) =>
        NormalScoring((incrementScore(p1Score), p2Score))

      case (P2, NormalScoring((p1Score, p2Score))) =>
        NormalScoring((p1Score, incrementScore(p2Score)))

      case (playerThatScored, Deuce) =>
        Advantage(playerThatScored)

      case (playerThatScored, Advantage(playerWithAdvantage)) =>
        if (playerThatScored == playerWithAdvantage) {
          Win(playerWithAdvantage)
        } else {
          Deuce
        }
      // exhaustive

      // case (P1, State.Win(_)) =>
      //   startGame()

      // case (P2, State.Win(_)) =>
      //   startGame()

      case (_, Win(_)) =>
        startGame()
    }
  }

  private def incrementScore(score: Score): Score = {
    score match {
      case ScoreLove => Score15
      case Score15   => Score30
      case Score30   => Score40
      case Score40   => Score40 // TODO: Harmless but shit
    }
  }
}
