
sealed trait HandValue {
  def ranks: Seq[Rank]
  def toint: Int =
    this match {
      case HighCard(_) => 1
      case TwoOfKinds(_) => 2
      case TwoPairs(_) => 3
      case ThreeOfKinds(_) => 4
      case Straight(_) => 5
      case Flush(_) => 6
      case FullHouse(_) => 7
      case FourOfKinds(_) => 8
      case StraightFlush(_) => 9
    }
}

case class StraightFlush(ranks: Seq[Rank]) extends HandValue
case class FourOfKinds(ranks: Seq[Rank]) extends HandValue
case class FullHouse(ranks: Seq[Rank]) extends HandValue
case class Flush(ranks: Seq[Rank]) extends HandValue
case class Straight(ranks: Seq[Rank]) extends HandValue
case class ThreeOfKinds(ranks: Seq[Rank]) extends HandValue
case class TwoPairs(ranks: Seq[Rank]) extends HandValue
case class TwoOfKinds(ranks: Seq[Rank]) extends HandValue
case class HighCard(ranks: Seq[Rank]) extends HandValue

object HandValue {
  def isStraight(cards: Seq[Card]): Boolean = {
    val ranks = cards.map(_.rank).sorted
    val hasAce = ranks.head == Ace
    val numbers = ranks map (_.toint)

    if (hasAce && (ranks.contains(King) || ranks.contains(Two)))
      numbers.tail.toSet == numbers.tail.head.to(numbers.tail.head - 3).by(-1).toSet
    else
      numbers.toSet == numbers.head.to(numbers.head - 4).by(-1).toSet
  }

  def hasOneSuit(cards: Seq[Card]): Boolean =
    cards.groupBy(_.suit).toSeq.length == 1

  def makeStraightFlush(data: Seq[Card]): StraightFlush =
    StraightFlush(Seq(data.minBy(_.rank).rank))

  def makeFourOfKinds(data: Seq[(Card, Int)]): FourOfKinds =
    data.map(_._1.rank) match {
      case Seq(kickerRank, fourRank) => FourOfKinds(Seq(fourRank, kickerRank))
    }

  def makeFullHouse(data: Seq[(Card, Int)]): FullHouse =
    data.map(_._1.rank) match {
      case Seq(rank2, rank3) => FullHouse(Seq(rank3, rank2))
    }

  def makeFlush(data: Seq[Card]): Flush = Flush(data.map(_.rank).sorted)

  def makeStraight(data: Seq[Card]): Straight = {
    data.sortBy(_.rank).map(_.rank) match {
      case Seq(f, s, _*) => Straight(Seq(if (f == Ace || s != King) s else f))
    }
  }

  def makeThreeOfKinds(data: Seq[(Card, Int)]): ThreeOfKinds = {
    data.map(_._1.rank) match {
      case Seq(r1, r2, rankMain) => ThreeOfKinds(Seq(rankMain) ++ Seq(r1, r2).sorted)
    }
  }

  def makeTwoPairs(data: Seq[(Card, Int)]): TwoPairs = {
    data.map(_._1.rank) match {
      case Seq(cardRank, pr1, pr2) => TwoPairs(Seq(pr1, pr2).sorted ++ Seq(cardRank))
    }
  }

  def makeTwoOfKinds(data: Seq[(Card, Int)]): TwoOfKinds = {
    data.map(_._1.rank) match {
      case Seq(pr, r1, r2, r3) => TwoOfKinds(Seq(pr) ++ Seq(r1, r2, r3).sorted)
    }
  }

  def makeHighCard(data: Seq[Card]): HighCard = HighCard(data.map(_.rank).sorted)

  def dispatch2GroupsValue(data: Seq[(Card, Int)]): HandValue = {
    // there exactly two elements in the sequence so it's safe
    if (data.head._2 == 1)
      makeFourOfKinds(data)
    else
      makeFullHouse(data)
  }

  def dispatch3GroupsValue(data: Seq[(Card, Int)]): HandValue = {
    // there exactly three elements in the sequence so it's safe
    if (data.tail.head._2 == 1)
      makeThreeOfKinds(data)
    else
      makeTwoPairs(data)
  }

  def dispatch5GroupsValue(data: Seq[(Card, Int)]): HandValue = {
    val cards = data.map(pair => pair._1).sortBy(_.rank.toint)
    if (hasOneSuit(cards)) {
      if (isStraight(cards)) makeStraightFlush(cards) else makeFlush(cards)
    } else {
      if (isStraight(cards)) makeStraight(cards) else makeHighCard(cards)
    }
  }

  def apply(cards: Seq[Card]): HandValue = {
    val data: Seq[(Card, Int)] = cards
      .groupBy(_.rank)
      .toSeq
      .map(pair => (pair._2.head, pair._2.length))
      .sortBy(_._2)

    data.length match {
      case 2 => dispatch2GroupsValue(data)
      case 3 => dispatch3GroupsValue(data)
      case 4 => makeTwoOfKinds(data)
      case _ => dispatch5GroupsValue(data)
    }
  }

  implicit val ordering: Ordering[HandValue] = (x: HandValue, y: HandValue) =>
    x.toint compare y.toint match {
      case 0 => x.ranks.zip(y.ranks).foldLeft(0) {
        (acc, pair) => if (acc != 0) acc else pair._1.toint compare pair._2.toint
      }
      case r => r
    }
}