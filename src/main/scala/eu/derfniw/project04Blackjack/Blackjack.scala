package eu.derfniw.project04Blackjack

import cats.effect.IO
import cats.effect.IOApp

case class Blackjack(deck: Deck, money: Int, dealerCards: List[Card], playerCards: List[Card])

object Blackjack:
  def apply(deck: Deck): Blackjack = 
    val (dealerCards, rem1) = deck.splitAt(2)
    val (playerCards, remainder) = rem1.splitAt(2)
    Blackjack(remainder, 5000, dealerCards.updated(1, dealerCards(1).open), playerCards.map(_.open))

