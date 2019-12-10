object Orderings {
  implicit object SuitOrdering extends Ordering[Suit] {
    def compare(x: Suit, y: Suit): Int = x.toint compare y.toint
  }

  implicit object RankOrdering extends Ordering[Rank] {
    // descending intentionally
    def compare(x: Rank, y: Rank): Int = y.toint compare x.toint
  }

  implicit object CardOrdering extends Ordering[Card] {
    def compare(x: Card, y: Card): Int =
      x.rank.toint compare y.rank.toint match {
        case 0 => x.suit.toint compare y.suit.toint
        case v => v
      }
  }

  implicit object HandValueOrdering extends Ordering[HandValue] {
    def compare(x: HandValue, y: HandValue): Int =
      x.toint compare y.toint match {
        case 0 => (x.ranks).zip(y.ranks).foldLeft(0) {
          (acc, pair) => if (acc != 0) acc else pair._1.toint compare pair._2.toint
        }
        case r => r
      }
  }
}
