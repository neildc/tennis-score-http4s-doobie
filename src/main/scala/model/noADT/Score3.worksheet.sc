sealed trait PlayerEnum
case object P1 extends PlayerEnum
case object P2 extends PlayerEnum

val WIN_SCORE = 3

sealed trait StateEnum
case object NormalScoring extends StateEnum
case object Deuce extends StateEnum
case object Advantage extends StateEnum
case object Win extends StateEnum

def shouldTransitionToDeuce(state: State3): Boolean =
  ((state.p1Score == WIN_SCORE && state.p2Score == (WIN_SCORE - 1))
    || (state.p1Score == (WIN_SCORE - 1) && state.p2Score == WIN_SCORE))

case class State3(
    state: StateEnum,
    playerWithAdvantage: Option[PlayerEnum], // required when state == Advantage
    playerThatWon: Option[PlayerEnum], // required when state == Won
    p1Score: Int,
    p2Score: Int
)

val s3: State3 =
  State3(
    state = NormalScoring,
    playerWithAdvantage = None,
    playerThatWon = None,
    p1Score = 0,
    p2Score = 0
  )

def score3(player: PlayerEnum, state: State3): Either[String, State3] = {
  if (shouldTransitionToDeuce(state))
    Right(s3.copy(state = Deuce))
  else
    (player, state.state) match {
      case (P1, NormalScoring) =>
        if (state.p1Score == WIN_SCORE)
          Right(s3.copy(state = Win, playerThatWon = Some(P1)))
        else
          Right(state.copy(p1Score = state.p1Score + 1))

      case (P2, NormalScoring) =>
        if (state.p2Score == WIN_SCORE)
          Right(s3.copy(state = Win, playerThatWon = Some(P2)))
        else
          Right(state.copy(p2Score = state.p2Score + 1))

      case (p, Deuce) =>
        Right(s3.copy(state = Advantage, playerWithAdvantage = Some(p)))

      case (p, Advantage) => {
        state.playerWithAdvantage match {
          case None => 
            Left("In the Advantage state, but state.playerWithAdvantage is None")

          case Some(pAdvantage) =>
            if (p == pAdvantage)
              Right(s3.copy(state = Win, playerThatWon = Some(p)))
            else
              Right(s3.copy(state = Deuce))
        }
      }

      case (p, Win) => Right(s3)
    }
}

object Main {
  def main(args: Array[String]) = {
    println("ok")
  }
}
