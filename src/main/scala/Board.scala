class Board(val cards: Seq[Card]) {
  override def toString: String = cards.map(_.toString).mkString(" ")
}

object Board {
  import Utils._
  def apply(s: String): Option[Board] = {
    val opts = for {
      (c1, r1) <- chopCard(s)
      (c2, r2) <- chopCard(r1)
      (c3, r3) <- chopCard(r2)
      (c4, r4) <- chopCard(r3)
      (c5, r5) <- chopCard(r4)
      _ <- checkEmptyTail(r5)
    } yield List (c1, c2, c3, c4, c5) map (Card(_))
    val optLst = for {
      lst <- opts
      cards <- traverse(lst)
    } yield cards
    optLst.map(new Board(_))
  }
}
