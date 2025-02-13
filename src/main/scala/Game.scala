import Utils._

class Game(val board: Board, val hands: Seq[Hand]) {
  override def toString: String = s"Game: Board [$board], Hands [${hands.map(_.name).mkString(" ")}]"

  def handCombs(hand: Hand): (Hand, Seq[Seq[Card]]) = {
    val combs = (board.cards ++ hand.cards)
      .combinations(5)
      .toSeq
    (hand, combs)
  }

  def yieldResult(): Option[String] = {
    val bestHands: Seq[(Hand, HandValue)] =
      hands
      .map(hand => handCombs(hand))
      .map(pair => (pair._1, pair._2.map(HandValue(_)).max))
      .sortBy(_._2)

    val resultGroups: Map[HandValue, String] =
      bestHands
      .groupBy(_._2)
      .map(p1 => (p1._1, p1._2.map(p2 => p2._1.name).sorted.mkString("=")))

    Some(bestHands.map(_._2).map(resultGroups(_)).distinct.mkString(" "))
  }
}

object Game {
  def apply(b: String, h: Seq[String]): Option[Game] = {
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
      _ <- if (hands.length <= 256) Some(true) else None
      _ <- uniqueSet(board, hands)
    } yield new Game(board, hands)
  }
}