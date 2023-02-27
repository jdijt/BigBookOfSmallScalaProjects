package eu.derfniw.project04Blackjack

import eu.derfniw.utils.readValidValue

import cats.*
import cats.data.StateT
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.std.Console
import cats.implicits.*

case class BlackJack(
    playerMoney: Int,
    currentBet: Int,
    playerHand: List[Card],
    dealerHand: List[Card],
    deck: List[Card]
)

object BlackJack:

  val initialState: BlackJack = BlackJack(5000, 0, List(), List(), List())

  def renewDeck: GameState[Unit] = StateT { st =>
    for newDeck <- Deck.shuffledDeck
    yield (st.copy(deck = newDeck, playerHand = List(), dealerHand = List(), currentBet = 0), ())
  }
  def drawCard: GameState[Card] = StateT { st =>
    (st.copy(deck = st.deck.tail), st.deck.head).pure[IO]
  }
  def addPlayerCard(c: Card): GameState[Int] = StateT { st =>
    val newState = st.copy(playerHand = st.playerHand :+ c)
    (newState, newState.playerHand.sumCards).pure[IO]
  }
  def addDealerCard(c: Card): GameState[Int] = StateT { st =>
    val newState = st.copy(dealerHand = st.dealerHand :+ c)
    (newState, newState.dealerHand.sumCards).pure[IO]
  }
  def incrementPlayerBet(bet: Int): GameState[Unit] = StateT.modify { st =>
    st.copy(playerMoney = st.playerMoney - bet, currentBet = st.currentBet + bet)
  }

  def doubleBet: GameState[Unit] = for
    st <- StateT.get[IO, BlackJack]
    _  <- incrementPlayerBet(st.currentBet)
  yield ()

  def dealInitialHands: GameState[Unit] =
    for
      dealerHand <- List(drawCard, drawCard).sequence
      playerHand <- List(drawCard, drawCard).sequence
      _          <- dealerHand.traverse(addDealerCard)
      _          <- playerHand.traverse(addPlayerCard)
    yield ()

  def printState(showDealer: Boolean)(using c: Console[IO]): GameState[Unit] = StateT.inspectF {
    st =>
      for
        _ <- c.println(s"Bet: ${st.currentBet}")
        _ <- c.println("")
        _ <- c.println(if showDealer then s"Dealer: ${st.dealerHand.sumCards}" else s"Dealer: ???")
        _ <- st.dealerHand.showCard(showDealer).traverse(c.println)
        _ <- c.println("")
        _ <- c.println(s"Player: ${st.playerHand.sumCards}")
        _ <- st.playerHand.showCard(true).traverse(c.println)
        _ <- c.println("")
      yield ()
  }

end BlackJack
