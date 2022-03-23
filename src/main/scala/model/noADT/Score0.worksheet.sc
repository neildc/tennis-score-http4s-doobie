case class State0(
    isDeuce: Boolean,
    playerWithAdvantage: Option[Int],
    playerThatWon: Option[Int],
    p1Score: Int,
    p2Score: Int
)

val s0: State0 =
  State0(
    isDeuce = false,
    playerWithAdvantage = None,
    playerThatWon = None,
    p1Score = 0,
    p2Score = 0
  )

val p1 = 1
val p2 = 2
val WIN_SCORE = 3

def shouldTransitionToDeuce(state: State0): Boolean =
  ((state.p1Score ==  WIN_SCORE      && state.p2Score == (WIN_SCORE - 1))
|| (state.p1Score == (WIN_SCORE - 1) && state.p2Score ==  WIN_SCORE))

def score0(player: Int, state: State0): State0 = {
  if (shouldTransitionToDeuce(state))
    s0.copy(isDeuce = true)
  else if (state.isDeuce)
    s0.copy(playerWithAdvantage = Some(player))
  else
    state.playerWithAdvantage match {
      case Some(pAdvantage) => {
        if (player == pAdvantage)
          s0.copy(playerThatWon = Some(player))
        else
          s0.copy(isDeuce = true)
      }

      case None => {
        if (player == p1)
          if (state.p1Score == WIN_SCORE)
            s0.copy(playerThatWon = Some(p1))
          else
            state.copy(p1Score = state.p1Score + 1)
        else
          if (state.p2Score == WIN_SCORE)
            s0.copy(playerThatWon = Some(p2))
          else
            state.copy(p2Score = state.p2Score + 1)
      }
    }
}

object Main {
  def main(args: Array[String]) = {
    println("ok")
  }
}

// println(score1(1, s0.copy(isDeuce = true, playerWithAdvantage = Some(1))))
// println(score0(3, s0.copy(isDeuce = true)))
