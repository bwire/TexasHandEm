case class Card(name: String, rank: Rank, suit: Suit) {
  override def toString: String = name
}

object Card {
  def apply(is: String): Option[Card] =
    is.toCharArray match {
      case Array(c1, c2) => for {
        r <- Rank(c1)
        s <- Suit(c2)
      } yield Card(is, r, s)
      case _ => None
    }
}
