package object model {

  case object ScoreNotFoundError

  sealed trait Score
  final case object ScoreLove extends Score
  final case object Score15 extends Score
  final case object Score30 extends Score
  final case object Score40 extends Score

  def scoreToInt(score: Score): Int =
    score match {
      case ScoreLove => 0
      case Score15   => 1
      case Score30   => 2
      case Score40   => 3
    }

  def intToScore(i: Int): Option[Score] =
    i match {
      case 0 => Some(ScoreLove)
      case 1 => Some(Score15)
      case 2 => Some(Score30)
      case 3 => Some(Score40)
      case _ => None
    }

  sealed abstract class Player
  final case object P1 extends Player
  final case object P2 extends Player

  def intToPlayer(i: Int): Option[Player] =
    i match {
      case 1 => Some(P1)
      case 2 => Some(P2)
      case _ => None
    }

  def playerToInt(player: Player): Int =
    player match {
      case P1 => 1
      case P2 => 2
    }

// State -> ADT (Algebraic Data Type)
  sealed trait State
  final case class NormalScoring(score: (Score, Score)) extends State
  final case object Deuce extends State
  final case class Advantage(player: Player) extends State
// TODO: What was the score when they won?
  final case class Win(player: Player) extends State

  def startGame(): State = NormalScoring((ScoreLove, ScoreLove))

  // score expanded

  /*
  def player1Score(state: State): State = score(P1, state)
  def player2Score(state: State): State = score(P2, state)

  def player1Score: State => State = score(P1)
  def player2Score: State => State = score(P2)
   */

  def score(player: Player)(state: State): State =
    // { throw new RuntimeException("asASD")}
    // { val arr = Array(1,2); arr(5); }
    {
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
