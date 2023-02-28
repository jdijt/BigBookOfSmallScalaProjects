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
  def drawPlayerCard: GameState[Int] = StateT { st =>
    val newState = st.copy(playerHand = st.playerHand :+ st.deck.head, deck = st.deck.tail)
    (newState, newState.playerHand.sumCards).pure[IO]
  }
  def drawDealerCard: GameState[Int] = StateT { st =>
    val newState = st.copy(dealerHand = st.dealerHand :+ st.deck.head, deck = st.deck.tail)
    (newState, newState.dealerHand.sumCards).pure[IO]
  }

  def playerScore: GameState[Int] = StateT.inspect(_.playerHand.sumCards)

  def dealerScore: GameState[Int] = StateT.inspect(_.dealerHand.sumCards)

  def incrementPlayerBet(bet: Int): GameState[Unit] = StateT.modify { st =>
    st.copy(playerMoney = st.playerMoney - bet, currentBet = st.currentBet + bet)
  }

  def doubleBet: GameState[Unit] = for
    st <- StateT.get[IO, BlackJack]
    _  <- incrementPlayerBet(st.currentBet)
  yield ()

  def dealInitialHands: GameState[Unit] =
    for
      _ <- List(drawDealerCard, drawDealerCard).sequence
      _ <- List(drawPlayerCard, drawPlayerCard).sequence
    yield ()

  def payOutBet(factor: Double): GameState[Unit] = StateT.modify { st =>
    val earnings = (st.currentBet * factor).toInt
    st.copy(playerMoney = st.playerMoney + earnings, currentBet = 0)
  }

end BlackJack
