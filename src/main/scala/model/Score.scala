package model

// State -> ADT (Algebraic Data Type)
sealed trait State
final case class NormalScoring(score: (Score, Score)) extends State
final case object Deuce extends State
final case class Advantage(player: Player) extends State
// TODO: What was the score when they won?
final case class Win(player: Player) extends State


sealed trait Score
final case object ScoreLove extends Score
final case object Score15 extends Score
final case object Score30 extends Score
final case object Score40 extends Score

object Score {

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
}

sealed trait Player
final case object P1 extends Player
final case object P2 extends Player

object Player {

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
}
