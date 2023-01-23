package eu.derfniw.project04Blackjack

import cats.effect.IO
import cats.effect.IOApp

class BlackJack(deck: Deck)

extension (cards: List[Card])
  def cardsSum: Int =
    val potentialSums = cards.foldLeft(List.empty[Int]) { (sums, card) =>
      for
        v <- card.rank.values
        s <- sums
      yield v + s
    }
    

object BlackJack:
  def apply(startingDeck: Deck): BlackJack =
    val (dealerCards, deckRem1) = startingDeck.take(2)
    val (playerCards, deckRem2) = deckRem1.take(2)
    BlackJack(deckRem2, 5000, dealerCards.updated(1, dealerCards(1).open), playerCards.map(_.open))
