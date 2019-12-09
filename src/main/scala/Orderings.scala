object Orderings {
  def compareRankSequences(s1: Seq[Rank], s2: Seq[Rank]): Int = {
    s1.zip(s2).foldLeft(0) {
      (acc, pair) => if (acc != 0) acc else pair._1.toint compare pair._2.toint
    }
  }

  implicit object SuitOrdering extends Ordering[Suit] {
    def compare(x: Suit, y: Suit): Int = x compare y
  }

  implicit object RankOrdering extends Ordering[Rank] {
    // descending intentionally
    def compare(x: Rank, y: Rank): Int = y compare x
  }

  implicit object CardOrdering extends Ordering[Card] {
    def compare(x: Card, y: Card): Int =
      x.rank compare y.rank match {
        case 0 => x.suit compare y.suit
        case v => v
      }
  }

  implicit object HandValueOrdering extends Ordering[HandValue] {
    def compare(x: HandValue, y: HandValue): Int =
      x.toint compare y.toint match {
        case 0 => compareRankSequences(x.ranks, y.ranks)
        case r => r
      }
  }
}
