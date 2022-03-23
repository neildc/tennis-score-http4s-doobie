println("ok")

case class State1(
    isDeuce: Boolean,
    playerWithAdvantage: Option[Int],
    playerThatWon: Option[Int],
    p1Score: Int,
    p2Score: Int
)

sealed trait StateEnum
case object NormalScoring extends StateEnum
case object Deuce extends StateEnum
case object Advantage extends StateEnum
case object Win extends StateEnum

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

val p1 = 1
val p2 = 2
val WIN_SCORE = 3

def score2(player: Int)(state: State2): State2 = {
  if (player != p1 || player != p2) {
    ???
  }

  if ((state.p1Score == WIN_SCORE       && state.p2Score == (WIN_SCORE - 1))
  ||  (state.p1Score == (WIN_SCORE - 1) && state.p2Score == WIN_SCORE )) {
    s2.copy(state = Deuce)
  } else {
    (player, state.state) match {
      case (1, NormalScoring) =>
        if (state.p1Score == WIN_SCORE)
          s2.copy(state = Win, playerThatWon = Some(p1))
        else
          state.copy(p1Score = state.p1Score + 1)

      case (2, NormalScoring) =>
        if (state.p2Score == WIN_SCORE)
          s2.copy(state = Win, playerThatWon = Some(p2))
        else
          state.copy(p2Score = state.p2Score + 1)

      case (p, NormalScoring) => ???

      case (p, Deuce) =>
        s2.copy(state = Advantage, playerWithAdvantage = Some(p))

      case (p, Advantage) => {
        state.playerWithAdvantage match {
          case None => ???
          case Some(pAdvantage) =>
            if (p == pAdvantage)
              s2.copy(state = Win, playerWithAdvantage = Some(pAdvantage))
            else
              s2.copy(state = Deuce)

        }
      }
      case (p, Win) => s2
    }
  }
}

sealed trait PlayerEnum
case object P1 extends PlayerEnum
case object P2 extends PlayerEnum

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

def score3(player: PlayerEnum)(state: State3): State3 = {
  if ((state.p1Score == WIN_SCORE       && state.p2Score == (WIN_SCORE - 1))
  ||  (state.p1Score == (WIN_SCORE - 1) && state.p2Score == WIN_SCORE )) {
    s3.copy(state = Deuce)
  } else {
    (player, state.state) match {
      case (P1, NormalScoring) =>
        if (state.p1Score == 3)
          s3.copy(state = Win, playerThatWon = Some(P1))
        else
          state.copy(p1Score = state.p1Score + 1)

      case (P2, NormalScoring) =>
        if (state.p2Score == 3)
          s3.copy(state = Win, playerThatWon = Some(P2))
        else
          state.copy(p2Score = state.p2Score + 1)

      case (p, Deuce) =>
        s3.copy(state = Advantage, playerWithAdvantage = Some(p))

      case (p, Advantage) => {
        state.playerWithAdvantage match {
          case None => ???
          case Some(pAdvantage) =>
            if (p == pAdvantage)
              s3.copy(state = Win, playerThatWon = Some(p))
            else
              s3.copy(state = Deuce)

        }
      }

      case (p, Win) => s3
    }
  }
}

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

def score3(player: PlayerEnum)(state: State4): State4 = {
  if ( (state.p1Score == Score30 && state.p2Score == Score40)
    || (state.p1Score == Score40 && state.p2Score == Score30)) {
    s4.copy(state = Deuce)
  } else {
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
}
