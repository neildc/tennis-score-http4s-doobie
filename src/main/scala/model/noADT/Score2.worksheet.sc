val p1 = 1
val p2 = 2
val WIN_SCORE = 3

sealed trait StateEnum
case object NormalScoring extends StateEnum
case object Deuce extends StateEnum
case object Advantage extends StateEnum
case object Win extends StateEnum


def shouldTransitionToDeuce(state: State2): Boolean =
    ((state.p1Score == WIN_SCORE && state.p2Score == (WIN_SCORE - 1))
  || (state.p1Score == (WIN_SCORE - 1) && state.p2Score == WIN_SCORE))

case class State2(
    state: StateEnum,
    playerWithAdvantage: Option[Int], // required when state == Advantage
    playerThatWon: Option[Int], // required when state == Won
    p1Score: Int,
    p2Score: Int
)

val s2: State2 =
  State2(
    state = NormalScoring,
    playerWithAdvantage = None,
    playerThatWon = None,
    p1Score = 0,
    p2Score = 0
  )

def score2(player: Int, state: State2): Either[String, State2] = {
  if (player != p1 && player != p2) {
    Left(s"Expected player $p1 or $p2")
  }

  if (shouldTransitionToDeuce(state))
      Right(s2.copy(state = Deuce))

    else
    (player, state.state) match {
      case (1, NormalScoring) =>
        if (state.p1Score == WIN_SCORE)
          Right(s2.copy(state = Win, playerThatWon = Some(p1)))
        else
          Right(state.copy(p1Score = state.p1Score + 1))

      case (2, NormalScoring) =>
        if (state.p2Score == WIN_SCORE)
          Right(s2.copy(state = Win, playerThatWon = Some(p2)))
        else
          Right(state.copy(p2Score = state.p2Score + 1))

      case (p, NormalScoring) =>
        Left(s"Expected player $p1 or $p2")

      case (p, Deuce) =>
        Right(s2.copy(state = Advantage, playerWithAdvantage = Some(p)))

      case (p, Advantage) => {
        state.playerWithAdvantage match {
          case None =>
            Left("In the Advantage state, but state.playerWithAdvantage is None")

          case Some(pAdvantage) =>
            if (p == pAdvantage)
              Right(s2.copy(state = Win, playerWithAdvantage = Some(pAdvantage)))
            else
              Right(s2.copy(state = Deuce))

        }
      }
      case (p, Win) => Right(s2)
    }
}

object Main {
  def main(args: Array[String]) = {
    println("ok")
  }
}
