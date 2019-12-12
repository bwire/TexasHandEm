sealed trait Rank {
  def toint: Int =
    this match {
      case Two => 1
      case Three => 2
      case Four => 3
      case Five => 4
      case Six => 5
      case Seven => 6
      case Eight => 7
      case Nine => 8
      case Ten => 9
      case Jack => 10
      case Queen => 11
      case King => 12
      case Ace => 13
    }
}

case object Ace extends Rank
case object King extends Rank
case object Queen extends Rank
case object Jack extends Rank
case object Ten extends Rank
case object Nine extends Rank
case object Eight extends Rank
case object Seven extends Rank
case object Six extends Rank
case object Five extends Rank
case object Four extends Rank
case object Three extends Rank
case object Two extends Rank

object Rank {
  def apply(c: Char): Option[Rank] =
    c match {
      case 'A' => Some(Ace)
      case 'K' => Some(King)
      case 'Q' => Some(Queen)
      case 'J' => Some(Jack)
      case 'T' => Some(Ten)
      case '9' => Some(Nine)
      case '8' => Some(Eight)
      case '7' => Some(Seven)
      case '6' => Some(Six)
      case '5' => Some(Five)
      case '4' => Some(Four)
      case '3' => Some(Three)
      case '2' => Some(Two)
      case _ => None
    }

  // descending intentionally
  implicit val ordering: Ordering[Rank] = (x: Rank, y: Rank) => y.toint compare x.toint
}