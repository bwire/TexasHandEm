import Utils._
import Orderings._

class Game(val board: Board, val hands: Seq[Hand]) {
  override def toString: String = s"Game: Board [$board], Hands [${hands.map(_.name).mkString(" ")}]"

  def handCombs(hand: Hand): (Hand, Seq[Seq[Card]]) = {
    val all: Seq[Card] = board.cards ++ hand.cards
    val combs = (for {
      c1 <- all
      c2 <- all
      c3 <- all
      c4 <- all
      c5 <- all
    } yield List(c1, c2, c3, c4, c5))
      .map(_.distinct)
      .filter(_.length == 5)
      .map(_.sorted)
      .distinct
    (hand, combs)
  }

  def yieldResult(): Option[String] = {
    val bestHands =
      hands
      .map(hand => handCombs(hand))
      .map(pair => (pair._1, pair._2.map(HandValue(_)).max))
      .sortBy(_._2)

    val resultGroups = bestHands
      .groupBy(_._2)
      .map(p1 => (p1._1, p1._2.map(p2 => p2._1).mkString("=")))

    Some(bestHands.map(_._2).map(resultGroups(_)).distinct.mkString(" "))
  }
}

object Game {
  def apply(b: String, n: Int, h: Seq[String]): Option[Game] = {
    def uniqueSet(board: Board, hands: Seq[Hand]): Option[Boolean] = {
      val allCards: Seq[Card] = hands.foldLeft(board.cards) {
        (a, h) => a ++ h.cards
      }
      if (allCards.length == allCards.distinct.length) Some(true) else None
    }

    for {
      board <- Board(b)
      hands <- traverse(h map (Hand(_)))
      _ <- if (hands.length == h.length) Some(true) else None
      _ <- uniqueSet(board, hands)
    } yield new Game(board, hands)
  }
}