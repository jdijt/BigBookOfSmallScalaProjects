package eu.derfniw.project04Blackjack

import cats.effect.IO
import cats.effect.std.Random

enum Suit(val icon: Char):
  case Hearts   extends Suit(0x2665)
  case Diamonds extends Suit(0x2666)
  case Clubs    extends Suit(0x2663)
  case Spades   extends Suit(0x2660)

enum Rank(val values: List[Int], val disp: String):
  case Ace            extends Rank(List(1, 11), "A")
  case King           extends Rank(List(10), "K")
  case Queen          extends Rank(List(10), "Q")
  case Jack           extends Rank(List(10), "J")
  case Number(v: Int) extends Rank(List(v), v.toString())

case class Card(suit: Suit, rank: Rank, isOpen: Boolean):
  def open: Card = this.copy(isOpen = true)

class Deck(cards: List[Card]):
  def take(n: Int): (List[Card], Deck) = (cards.take(n), Deck(cards.drop(n)))

object Deck:
  import Rank.*
  import Suit.*

  private val allCardsSorted =
    for
      suit <- List(Hearts, Diamonds, Clubs, Spades)
      rank <- (2 to 10).map(v => Number(v)) ++ List(Ace, King, Queen, Jack)
    yield Card(suit, rank, false)

  def shuffledDeck: IO[Deck] =
    val generator = Random.javaUtilConcurrentThreadLocalRandom[IO]
    for cards <- generator.shuffleList(allCardsSorted) yield Deck(cards)
end Deck
