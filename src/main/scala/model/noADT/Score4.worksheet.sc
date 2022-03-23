sealed trait PlayerEnum
case object P1 extends PlayerEnum
case object P2 extends PlayerEnum

val WIN_SCORE = 3

sealed trait StateEnum
case object NormalScoring extends StateEnum
case object Deuce extends StateEnum
case object Advantage extends StateEnum
case object Win extends StateEnum

sealed trait ScoreEnum
case object ScoreLove extends ScoreEnum
case object Score15 extends ScoreEnum
case object Score30 extends ScoreEnum
case object Score40 extends ScoreEnum

def incrementScore(score: ScoreEnum): ScoreEnum = {
  score match {
    case ScoreLove => Score15
    case Score15   => Score30
    case Score30   => Score40
    case Score40   => Score40 // TODO: Harmless but shit
  }
}

def shouldTransitionToDeuce(state: State4): Boolean =
  ((state.p1Score == Score40 && state.p2Score == Score30)
    || (state.p1Score == Score30) && state.p2Score == Score40)

case class State4(
    state: StateEnum,
    playerWithAdvantage: Option[PlayerEnum], // required when state == Advantage
    playerThatWon: Option[PlayerEnum], // required when state == Won
    p1Score: ScoreEnum,
    p2Score: ScoreEnum
)

val s4: State4 =
  State4(
    state = NormalScoring,
    playerWithAdvantage = None,
    playerThatWon = None,
    p1Score = ScoreLove,
    p2Score = ScoreLove
  )

def score3(player: PlayerEnum, state: State4): State4 = {
  if (shouldTransitionToDeuce(state))
    s4.copy(state = Deuce)
  else
    (player, state.state) match {
      case (P1, NormalScoring) =>
        if (state.p1Score == Score40)
          s4.copy(state = Win, playerThatWon = Some(P1))
        else
          state.copy(p1Score = incrementScore(state.p1Score))

      case (P2, NormalScoring) =>
        if (state.p2Score == Score40)
          s4.copy(state = Win, playerThatWon = Some(P2))
        else
          state.copy(p2Score = incrementScore(state.p2Score))

      case (p, Deuce) =>
        s4.copy(state = Advantage, playerWithAdvantage = Some(p))

      case (p, Advantage) => {
        state.playerWithAdvantage match {
          case None => ???
          case Some(pAdvantage) =>
            if (p == pAdvantage)
              s4.copy(state = Win, playerThatWon = Some(p))
            else
              s4.copy(state = Deuce)

        }
      }

      case (_, Win) => s4
    }
}

object Main {
  def main(args: Array[String]) = {
    println("ok")
  }
}
