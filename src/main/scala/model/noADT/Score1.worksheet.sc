case class State1(
    isDeuce: Boolean,
    playerWithAdvantage: Option[Int],
    playerThatWon: Option[Int],
    p1Score: Int,
    p2Score: Int
)

val s1: State1 =
  State1(
    isDeuce = false,
    playerWithAdvantage = None,
    playerThatWon = None,
    p1Score = 0,
    p2Score = 0
  )

val p1 = 1
val p2 = 2
val WIN_SCORE = 3

def shouldTransitionToDeuce(state: State1): Boolean =
  ((state.p1Score ==  WIN_SCORE       && state.p2Score == (WIN_SCORE - 1))
|| (state.p1Score == (WIN_SCORE - 1)  && state.p2Score ==  WIN_SCORE))

def score1(player: Int, state: State1): Either[String, State1] = {
  if (player != p1 && player != p2) {
    Left(s"Expected player $p1 or $p2")
  }

  if (shouldTransitionToDeuce(state))
    Right(s1.copy(isDeuce = true))

  else if (state.isDeuce)
    Right(s1.copy(playerWithAdvantage = Some(player)))

  else
    state.playerWithAdvantage match {
      case Some(pAdvantage) => {
        if (player == pAdvantage)
          Right(s1.copy(playerThatWon = Some(player)))
        else
          Right(s1.copy(isDeuce = true))
      }

      case None => {
        if (player == p1)
          if (state.p1Score == WIN_SCORE)
            Right(s1.copy(playerThatWon = Some(p1)))
          else
            Right(state.copy(p1Score = state.p1Score + 1))

        else if (player == p2)
          if (state.p2Score == WIN_SCORE)
            Right(s1.copy(playerThatWon = Some(p2)))
          else
            Right(state.copy(p2Score = state.p2Score + 1))

        else
          Left(s"Expected player $p1 or $p2")
      }
    }
}

object Main {
  def main(args: Array[String]) = {
    println("ok")
  }
}

println(score1(1, s1.copy(isDeuce = true, playerWithAdvantage = Some(1))))
// println(score1(3, s1.copy(isDeuce = true)))
