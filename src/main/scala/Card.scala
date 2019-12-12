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

  implicit object Ordering extends Ordering[Card] {
    def compare(x: Card, y: Card): Int =
      x.rank.toint compare y.rank.toint match {
        case 0 => x.suit.toint compare y.suit.toint
        case v => v
      }
  }
}
