sealed trait Suit {
  def toint: Int =
    this match {
      case Spades => 1
      case Clubs => 2
      case Diamonds => 3
      case Hearts => 4
    }
}
case object Hearts extends Suit
case object Diamonds extends Suit
case object Clubs extends Suit
case object Spades extends Suit

object Suit {
  def apply(c: Char): Option[Suit] =
    c match {
      case 'h' => Some(Hearts)
      case 'd' => Some(Diamonds)
      case 'c' => Some(Clubs)
      case 's' => Some(Spades)
      case _ => None
  }

  implicit val ordering: Ordering[Suit] = (x: Suit, y: Suit) => x.toint compare y.toint
}
