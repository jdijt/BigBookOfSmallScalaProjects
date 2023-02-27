package eu.derfniw.project04Blackjack

import cats.effect.*
import cats.effect.std.Random

enum Suit(val icon: Char):
  case Hearts   extends Suit(0x2665)
  case Diamonds extends Suit(0x2666)
  case Clubs    extends Suit(0x2663)
  case Spades   extends Suit(0x2660)

enum Rank(val values: List[Int], val display: String):
  case Ace            extends Rank(List(1, 11), "A")
  case King           extends Rank(List(10), "K")
  case Queen          extends Rank(List(10), "Q")
  case Jack           extends Rank(List(10), "J")
  case Number(v: Int) extends Rank(List(v), v.toString)

case class Card(suit: Suit, rank: Rank)

/** Due to cards potentially having multiple values (Ace) cannot just make them easily summable by
  * implementing Numeric.
  *
  * Every time an Ace is encountered the number of possible combinations doubles, this method just
  * calculates all and then pics the score closest to 21, preferring one that <= 21.
  */
extension (l: Seq[Card])
  def sumCards: Int =
    val possibleValues = l.foldLeft(List(0)) { (currentSums, card) =>
      for cs <- currentSums; v <- card.rank.values yield cs + v
    }
    if possibleValues.forall(_ > 21) then possibleValues.min
    else possibleValues.filterNot(_ > 21).max

type Deck = List[Card]

object Deck:
  import Rank.*
  import Suit.*

  private val allCardsSorted =
    for
      suit <- List(Hearts, Diamonds, Clubs, Spades)
      rank <- (2 to 10).map(v => Number(v)) ++ List(Ace, King, Queen, Jack)
    yield Card(suit, rank)

  def shuffledDeck: IO[Deck] =
    val generator = Random.javaUtilConcurrentThreadLocalRandom[IO]
    for cards <- generator.shuffleList(allCardsSorted)
    yield cards
end Deck
