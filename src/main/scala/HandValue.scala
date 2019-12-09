import Orderings._

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

  def compare(that: HandValue): Int = compareRankSequences(this.ranks, that.ranks)
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
    val numbers = cards.map(_.rank.toint).sorted
    val start = numbers.head
    numbers.toSet == (start to start + 4).toSet
  }

  def isStraightWithAce(cards: Seq[Card]): Boolean = {
    val ranks = cards.map(_.rank).sorted
    val hasAce = ranks.head == Ace
    val numbers = ranks map (_.toint)

    if (hasAce)
      numbers.tail.toSet == numbers.tail.head.to(numbers.tail.head - 3).by(-1).toSet
    else
      numbers.toSet == numbers.head.to(numbers.head - 4).by(-1).toSet
  }

  def hasOneSuit(cards: Seq[Card]): Boolean =
    cards.groupBy(_.suit).toSeq.length == 1


  def makeStraightFlush(data: Seq[Card]): StraightFlush =
    StraightFlush(Seq(data.minBy(_.rank).rank))

  def makeFourOfKinds(data: Seq[(Card, Int)]): FourOfKinds = {
    val kickerRank = data.head._1.rank
    val fourRank = data.tail.head._1.rank
    FourOfKinds(Seq(fourRank, kickerRank))
  }

  def makeFullHouse(data: Seq[(Card, Int)]): FullHouse = {
    val rank2 = data.head._1.rank
    val rank3 = data.tail.head._1.rank
    FullHouse(Seq(rank3, rank2))
  }

  def makeFlush(data: Seq[Card]): Flush = Flush(data.map(_.rank).sorted)

  def makeStraight(data: Seq[Card]): Straight = {
    val cards = data.sortBy(_.rank)
    val rank = if (cards.head.rank == Ace && cards.tail.head.rank != King)
      cards.tail.head.rank
    else
      cards.head.rank
    Straight(Seq(rank))
  }

  def makeThreeOfKinds(data: Seq[(Card, Int)]): ThreeOfKinds = {
    val rankMain = data.tail.tail.head._1.rank
    val ranks = Seq(data.head, data.tail.head).map(_._1.rank).sorted
    val rank1 = ranks.head
    val rank2 = ranks.tail.head
    ThreeOfKinds(Seq(rankMain, rank1, rank2))
  }

  def makeTwoPairs(data: Seq[(Card, Int)]): TwoPairs = {
    val cardRank = data.head._1.rank
    val ranks = data.tail.map(_._1.rank).sorted
    val rank1 = ranks.head
    val rank2 = ranks.tail.head
    TwoPairs(Seq(rank1, rank2, cardRank))
  }

  def makeTwoOfKinds(data: Seq[(Card, Int)]): TwoOfKinds = {
    val pairRank = data.last._1.rank
    val ranks = Seq(data.head, data.tail.head, data.tail.tail.head).map(_._1.rank).sorted
    TwoOfKinds(Seq(pairRank) ++ ranks)
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
      if (isStraightWithAce(cards)) makeStraight(cards) else makeHighCard(cards)
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
}