package eu.derfniw.project04Blackjack

import cats.*
import cats.data.StateT
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits.*

case class BlackJack(
    playerMoney: Int,
    currentBet: Int,
    playerHand: List[Card],
    dealerHand: List[Card],
    deck: List[Card]
)

object BlackJack extends IOApp.Simple:

  type GameState[A] = StateT[IO, BlackJack, A]

  val initialState: BlackJack = BlackJack(5000, 0, List(), List(), List())

  def run: IO[Unit] = for
    _  <- IO.println(Text.rules)
    fs <- runGame.runS(initialState)
    _  <- IO.println(s"You completed the game with ${fs.playerMoney} money in the end!")
  yield ()

  def runGame: GameState[Unit] =
    for
      _        <- renewDeck
      continue <- initialBet
      _ <-
        if !continue then StateT.liftF(IO.unit)
        else
          for
            _ <- dealInitialHands
            _ <- playerActions()
            // _ <- dealerActions()
            // _ <- announceWinLose()
            _ <- runGame
          yield ()
    yield ()

  def renewDeck: GameState[Unit] = StateT { st =>
    for newDeck <- Deck.shuffledDeck
    yield (st.copy(deck = newDeck, playerHand = List(), dealerHand = List(), currentBet = 0), ())
  }
  def drawCard: GameState[Card] = StateT { st =>
    IO.pure(st.copy(deck = st.deck.tail), st.deck.head)
  }
  def addPlayerCard(c: Card): GameState[Unit] = StateT { st =>
    IO.pure(st.copy(playerHand = st.playerHand :+ c), ())
  }
  def addDealerCard(c: Card): GameState[Unit] = StateT { st =>
    IO.pure(st.copy(dealerHand = st.dealerHand :+ c), ())
  }

  def initialBet: GameState[Boolean] =
    def getValidBetInput(st: BlackJack): IO[Option[Int]] = for
      _ <- IO.print(">")
      l <- IO.readLine
      res <- l.toIntOption match
               case Some(i) if i > 0 && i <= st.playerMoney => IO.pure(Some(i))
               case None if l == "QUIT"                     => IO.pure(None)
               case Some(i) if i > st.playerMoney =>
                 IO.println("You cannot bet more than you have!") >> getValidBetInput(st)
               case Some(i) => IO.println("Bet must be greater than 0") >> getValidBetInput(st)
               case None    => IO.println("Invalid number") >> getValidBetInput(st)
    yield res

    StateT { st =>
      for
        _  <- IO.println(s"How much do you bet? (1-${st.playerMoney}, or QUIT)")
        in <- getValidBetInput(st)
        newState = in match
                     case None      => st
                     case Some(bet) => st.copy(playerMoney = st.playerMoney - bet, currentBet = bet)
        continue = in.nonEmpty
      yield (newState, continue)
    }
  end initialBet

  def dealInitialHands: GameState[Unit] =
    for
      dealerHand <- List(drawCard, drawCard).sequence
      playerHand <- List(drawCard, drawCard).sequence
      _          <- dealerHand.traverse(addDealerCard)
      _          <- playerHand.traverse(addPlayerCard)
    yield ()

  enum PlayerAction:
    case Hit
    case Stand
    case Double

  def playerActions(first: Boolean = true): GameState[Unit] =
    def readPlayerAction: IO[PlayerAction] =
      import PlayerAction.*
      for
        _  <- IO.print(s"(H)it, (S)tand" + (if first then ", (D)ouble down" else ""))
        in <- IO.print(">") >> IO.readLine
        action <- in.map(_.toLower) match
                    case "h"          => Hit.pure[IO]
                    case "s"          => Stand.pure[IO]
                    case "d" if first => Double.pure[IO]
                    case _            => IO.println(s"""Invalid input: "$in"""") >> readPlayerAction
      yield action
    end readPlayerAction

    for
      _      <- printState(false)
      action <- StateT.liftF(readPlayerAction)
      _ <- action match
             case PlayerAction.Hit =>
               for
                 card <- drawCard
                 _    <- addPlayerCard(card)
                 _    <- StateT((st: BlackJack) => if st.playerHand.sumCards > 21 then (st, IO.unit) else )
               yield ()
             case PlayerAction.Stand  => StateT.liftF(IO.unit)
             case PlayerAction.Double => ???
    yield ()
    end for
  end playerActions

  def dealerActions(): GameState[Unit] = ???

  def announceWinLose(): GameState[Unit] = ???

  def printState(showDealer: Boolean): GameState[Unit] = StateT { st =>
    for
      _ <- IO.println(s"Bet: ${st.currentBet}")
      _ <- IO.println("")
      _ <- IO.println(if showDealer then s"Dealer: ${st.dealerHand.sumCards}" else s"Dealer: ???")
      _ <- st.dealerHand.showCard(showDealer).traverse(IO.println)
      _ <- IO.println("")
      _ <- IO.println(s"Player: ${st.playerHand.sumCards}")
      _ <- st.playerHand.showCard(true).traverse(IO.println)
      _ <- IO.println("")
    yield (st, ())
  }
end BlackJack
