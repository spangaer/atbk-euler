package eu.atbk.euler.p005

import scala.collection.immutable.{ Seq => ISeq }
import eu.atbk.euler.util.MyIO

object P0054 {
  def main(args: Array[String]) {
    //    println(Rules.HighCard)
    println(Rules.values)

    val test = ISeq(
      "5H 5C 6S 7S KD 2C 3S 8S 8D TD", // Pair of Fives       Pair of Eights       Player 2
      "5D 8C 9S JS AC 2C 5C 7D 8S QH", // Highest card Ace    Highest card Queen   Player 1
      "2D 9C AS AH AC 3D 6D 7D TD QD", // Three Aces          Flush with Diamonds  Player 2
      "4D 6S 9H QH QC 3D 6D 7H QD QS", // Pair of Queens + 9  Pair of Queens + 7   Player 1
      "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D") // Full House (4)      Full House (3)       Player 1

    test
      .map(hands => parseHands(hands))
      .map { case (a, b) => (Rules.score(a), Rules.score(b)) }
      .foreach { play => println(s"$play\t${winnerString(play)}") }

    println("")

    val plays = MyIO.fileLines("data/p054_poker.txt").filterNot(_.isEmpty())
      .map(hands => parseHands(hands))
      .map { case (a, b) => (Rules.score(a), Rules.score(b)) }

    val wins = plays
      .map { play =>
        println(s"$play\t${winnerString(play)}")
        play
      }
      .count { case (a, b) => a > b }

    println(wins)
  }

  def winnerString(play: (RuleApplicaton, RuleApplicaton)): String = {
    if (play._1 > play._2) {
      "Player 1"
    } else {
      "Player 2"
    }
  }

  private def parseHands(hands: String): (ISeq[Card], ISeq[Card]) = {
    val elements = hands.split(" ").filterNot(_.isEmpty()).to[ISeq]
    val (one, two) = elements.splitAt(5)
    (parseHand(one), parseHand(two))
  }

  private def parseHand(hand: ISeq[String]): ISeq[Card] = {
    hand.map(s => Card(CardValues.withNameAsCardValue("" + s(0)), CardSymbols.withName("" + s(1))))
  }

  object CardSymbols extends Enumeration {
    type CardSymbol = Value
    val H, C, S, D = Value
  }

  object CardValues extends Enumeration {
    sealed class CardValue(order: Int, name: String) extends Val(order, name) {

      def next(previous: Option[CardValue]): Option[CardValue] = {
        Some(CardValues.apply(order + 1).asInstanceOf[CardValue])
      }
    }

    val V2 = new CardValue(2, "2")
    val V3 = new CardValue(3, "3")
    val V4 = new CardValue(4, "4")
    val V5 = new CardValue(5, "5")
    val V6 = new CardValue(6, "6")
    val V7 = new CardValue(7, "7")
    val V8 = new CardValue(8, "8")
    val V9 = new CardValue(9, "9")
    val VT = new CardValue(10, "T")
    val VJ = new CardValue(11, "J")
    val VQ = new CardValue(12, "Q")
    val VK = new CardValue(13, "K")
    val VA = new CardValue(14, "A") {
      override def next(previous: Option[CardValue]): Option[CardValue] = {
        previous match {
          case None => Some(V2)
          case Some(_) => None
        }
      }
    }

    implicit val ordering = Ordering.by[CardValue, Value](_.asInstanceOf[Value])
    val seqSordering = Ordering.Implicits.seqDerivedOrdering[Seq, CardValue]

    def withNameAsCardValue(s: String): CardValue = super.withName(s).asInstanceOf[CardValue]

  }

  case class Card(value: CardValues.CardValue, symbol: CardSymbols.CardSymbol) extends Ordered[Card] {
    override def compare(other: Card) = value.compare(other.value)
  }

  case class RuleApplicaton(rule: Rules.Rule, inRuleScore: ISeq[CardValues.CardValue], highCards: ISeq[CardValues.CardValue]) extends Ordered[RuleApplicaton] {

    override def compare(other: RuleApplicaton) = {
      var out = rule.compare(other.rule)
      if (out != 0) {
        out
      } else {
        val myScoreSeq = inRuleScore ++ (highCards.sorted(CardValues.ordering.reverse))
        val otherScoreSeq = other.inRuleScore ++ (other.highCards.sorted(CardValues.ordering.reverse))

        CardValues.seqSordering.compare(myScoreSeq, otherScoreSeq)
      }
    }
  }

  object Rules extends Enumeration {
    sealed abstract class Rule(order: Int, name: String) extends Val(order, name) {
      def apply(hand: ISeq[Card]): Option[RuleApplicaton]
    }

    val HighCard = new Rule(0, "HighCard") {
      override def apply(hand) = Some(RuleApplicaton(this, Nil, hand.map(_.value)))
    }

    val OnePair = new Rule(1, "OnePair") {
      override def apply(hand) = {
        val a = hand.dropRight(1)
        val b = hand.drop(1)

        val matcher = a.zip(b)

        // select first matching pair, in case there are more, rule will be overruled
        matcher.find { case (ca, cb) => ca.value == cb.value }
          .map {
            case (ca, cb) =>
              val ruleScore = ISeq(ca.value)
              val highCards = hand.filterNot(c => c == ca || c == cb)
              RuleApplicaton(this, ruleScore, highCards.map(_.value))
          }
      }
    }

    val TwoPair = new Rule(2, "TwoPair") {
      override def apply(hand) = {
        // Two pair is same as one pair succeeding twice
        // If the same rank occurs more it will be overrules by higher rule

        OnePair(hand)
          .flatMap {
            case RuleApplicaton(_, lowerPair, _) =>
              val remainder = hand.filterNot(_.value == lowerPair.head)
              OnePair(remainder).map {
                case RuleApplicaton(_, higherPair, highCard) =>
                  RuleApplicaton(this, higherPair ++ lowerPair, highCard)
              }
          }
      }
    }

    val ThreeOfAKind = new Rule(3, "ThreeOfAKind") {
      override def apply(hand) = {
        val a = hand.dropRight(2)
        val b = hand.drop(1).dropRight(1)
        val c = hand.drop(2)

        val matcher = a.zip(b).zip(c).map { case ((a, b), c) => (a, b, c) }

        // select first matching pair, in case there are more, rule will be overruled
        matcher.find { case (ca, cb, cc) => ca.value == cb.value && cb.value == cc.value }
          .map {
            case (ca, cb, cc) =>
              val ruleScore = ISeq(ca.value)
              val highCards = hand.filterNot(c => c == ca || c == cb || c == cc)
              RuleApplicaton(this, ruleScore, highCards.map(_.value))
          }
      }
    }

    val Straight = new Rule(4, "Straight") {
      override def apply(hand) = {
        // if sequential or last card is ACE and when put in front, sequential
        val isStraight = if (sequential(hand)) {
          Some(hand)
        } else if (hand.last.value == CardValues.VA) {
          val otherSeq = hand.last +: hand.dropRight(1)
          if (sequential(otherSeq)) {
            Some(otherSeq)
          } else {
            None
          }
        } else {
          None
        }

        isStraight
          .map { hand => // only highest value of straight needed
            RuleApplicaton(this, ISeq(hand.last.value), Nil)
          }
      }

      def sequential(hand: ISeq[Card]): Boolean = {
        val a = hand.dropRight(1)
        val b = hand.drop(1)

        val matcher = a.zip(b)

        matcher.foldLeft((None: Option[CardValues.CardValue], true)) {
          case ((_, false), _) =>
            // when false don't care anymore
            (None, false)
          case ((previousCard, _), (currentCard, nextCard)) =>
            (Some(currentCard.value), currentCard.value.next(previousCard) == Some(nextCard.value))
        }._2
      }
    }

    val Flush = new Rule(5, "Flush") {
      override def apply(hand) = {
        val a = hand.dropRight(1)
        val b = hand.drop(1)

        val matcher = a.zip(b)

        val flush = matcher.forall { case (ca, cb) => ca.symbol == cb.symbol }

        if (flush) {
          Some(RuleApplicaton(this, hand.map(_.value).sorted(CardValues.ordering.reverse), Nil))
        } else
          None
      }
    }

    val FullHouse = new Rule(6, "FullHouse") {
      override def apply(hand) = {
        // Full House is same as three of a kind + one pair
        // If the same rank occurs more it will be overrules by higher rule

        ThreeOfAKind(hand)
          .flatMap {
            case RuleApplicaton(_, three, _) =>
              val remainder = hand.filterNot(_.value == three.head)
              OnePair(remainder).map {
                case RuleApplicaton(_, two, highCard) =>
                  RuleApplicaton(this, three ++ two, Nil)
              }
          }

      }
    }

    val FourOfAKind = new Rule(7, "FourOfAKind") {
      override def apply(hand) = {
        // Full House is same as three of a kind and a remainder that is 1 after filtering
        // If the same rank occurs more it will be overrules by higher rule

        ThreeOfAKind(hand)
          .flatMap {
            case RuleApplicaton(_, three, _) =>
              val remainder = hand.filterNot(_.value == three.head)

              if (remainder.size == 1) {
                Some(RuleApplicaton(this, three, remainder.map(_.value)))
              } else {
                None
              }
          }
      }
    }

    val StraightFlush = new Rule(8, "StraightFlush") {
      override def apply(hand) = {
        // is a flush that is also a straight, straight score counts

        Flush(hand)
          .flatMap {
            case _ =>
              Straight(hand).map(_.copy(rule = this))
          }
      }
    }

    def score(hand: ISeq[Card]) =
      super.values
        .toSeq
        .map(_.asInstanceOf[Rule](hand.sorted)).filter(_.isDefined).map(_.get).max

  }
}

