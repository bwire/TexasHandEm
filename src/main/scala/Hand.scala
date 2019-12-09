case class Hand(name: String, cards: Seq[Card]) {
  override def toString: String = name
}

object Hand {
  import Utils._
  def apply(s: String): Option[Hand] = {
    val opts = for {
      (c1, r1) <- chopCard(s)
      (c2, r2) <- chopCard(r1)
      _ <- checkEmptyTail(r2)
    } yield List (c1, c2) map (Card(_))
    val optLst = for {
      lst <- opts
      cards <- traverse(lst)
    } yield cards
    optLst.map(new Hand(s, _))
  }
}
