package model

import io.circe.{Decoder}
import cats.syntax.functor._
import io.circe.syntax._
import io.circe._, io.circe.generic.semiauto._

// State -> ADT (Algebraic Data Type)
sealed trait State
final case class NormalScoring(score: (Score, Score)) extends State
final case object Deuce extends State
final case class Advantage(player: Player) extends State
// TODO: What was the score when they won?
final case class Win(player: Player) extends State

object State {
  implicit val decodeNormalScoring: Decoder[NormalScoring] =
    deriveDecoder

  implicit val decodeAdvantage = new Decoder[Advantage] {
    final def apply(c: HCursor): Decoder.Result[Advantage] =
      for {
        p <- c.downField("advantage").as[Player]
      } yield {
        new Advantage(p)
      }
  }

  implicit val decodeWin= new Decoder[Win] {
      final def apply(c: HCursor): Decoder.Result[Win] =
        for {
          p <- c.downField("win").as[Player]
        } yield {
          new Win(p)
        }
    }
  val decodeDeuce: Decoder[State] = Decoder[String].emap(s =>
     s match {
      case "deuce" => Right(Deuce)
       case _ => Left("expected: deuce")
     }
  )

  implicit val decodeState: Decoder[State] = {
    def decodeOneOf[A](decoders: List[Decoder[A]]) = decoders.reduceLeft(_ or _)

    decodeOneOf(
      List[Decoder[State]](
          Decoder[NormalScoring].widen,
          Decoder[Advantage].widen,
          Decoder[Win].widen,
          decodeDeuce
      )
    )
  }

}


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
  implicit val decodeScore: Decoder[Score] = Decoder[String].emap {
    case "love" => Right(ScoreLove)
    case "15" => Right(Score15)
    case "30" => Right(Score30)
    case "40" => Right(Score40)
    case other => Left(s"invalid score: $other")

  }
}

sealed trait Player
final case object P1 extends Player
final case object P2 extends Player

object Player {
  implicit val decodePlayer: Decoder[Player] = Decoder[String].emap {
      case "p1" => Right(P1)
      case "p2" => Right(P2)
      case other => Left(s"invalid player: $other")
  }


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
