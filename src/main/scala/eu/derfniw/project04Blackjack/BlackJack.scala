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
            _           <- dealInitialHands
            playerScore <- playerActions()
            dealerScore <- dealerActions()
            _           <- processWinLose(playerScore, dealerScore)
            _           <- runGame
          yield ()
    yield ()

  def renewDeck: GameState[Unit] = StateT { st =>
    for newDeck <- Deck.shuffledDeck
    yield (st.copy(deck = newDeck, playerHand = List(), dealerHand = List(), currentBet = 0), ())
  }
  def drawCard: GameState[Card] = StateT { st =>
    IO.pure(st.copy(deck = st.deck.tail), st.deck.head)
  }
  def addPlayerCard(c: Card): GameState[Int] = StateT { st =>
    val newState = st.copy(playerHand = st.playerHand :+ c)
    IO.pure(newState, newState.playerHand.sumCards)
  }
  def addDealerCard(c: Card): GameState[Int] = StateT { st =>
    val newState = st.copy(dealerHand = st.dealerHand :+ c)
    IO.pure(newState, newState.dealerHand.sumCards)
  }

  def initialBet: GameState[Boolean] =
    def getValidBetInput(st: BlackJack): IO[Option[Int]] = for
      _ <- IO.print("> ")
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
      if st.playerMoney == 0 then IO.println("You're broke!") >> IO.pure(st, false)
      else
        for
          _  <- IO.println(s"How much do you bet? (1-${st.playerMoney}, or QUIT)")
          in <- getValidBetInput(st)
          newState = in match
                       case None => st
                       case Some(bet) =>
                         st.copy(playerMoney = st.playerMoney - bet, currentBet = bet)
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

  def playerActions(first: Boolean = true): GameState[Int] =
    def readPlayerAction: IO[PlayerAction] =
      import PlayerAction.*
      for
        _  <- IO.println(s"(H)it, (S)tand" + (if first then ", (D)ouble down" else ""))
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
      newScore <- action match
                    case PlayerAction.Hit =>
                      for
                        card     <- drawCard
                        newScore <- addPlayerCard(card)
                      yield newScore
                    case PlayerAction.Stand =>
                      StateT.inspect[IO, BlackJack, Int](_.playerHand.sumCards)
                    case PlayerAction.Double =>
                      for
                        _ <- StateT.modify[IO, BlackJack](st =>
                               st.copy(
                                 playerMoney = st.playerMoney - st.currentBet,
                                 currentBet = st.currentBet * 2
                               )
                             )
                        cards    <- List(drawCard, drawCard).sequence
                        newScore <- cards.traverse(addPlayerCard).map(_.last)
                      yield newScore
      finalScore <-
        if newScore > 21 then
          StateT.liftF(IO.println("You went bust!, dealer's turn now") >> IO.pure(newScore))
        else if newScore == 21 then 
          StateT.liftF(IO.println("Blackjack!, dealer's turn!") >> IO.pure(newScore))
        else if action == PlayerAction.Stand then StateT.liftF(IO.pure(newScore))
        else playerActions(false)
    yield finalScore
    end for
  end playerActions

  def dealerActions(): GameState[Int] =
    for
      _ <- printState(true)
      _ <- StateT.liftF(IO.println("Press enter to let dealer continue...") >> IO.readLine)
      newScore <- StateT
                    .inspect[IO, BlackJack, Int]((st: BlackJack) => st.dealerHand.sumCards)
                    .flatMap(score =>
                      if score > 17 then StateT.liftF(score.pure)
                      else
                        for
                          card       <- drawCard
                          _          <- addDealerCard(card)
                          finalScore <- dealerActions()
                        yield finalScore
                    )
    yield newScore

  def processWinLose(playerScore: Int, dealerScore: Int): GameState[Unit] =
    if playerScore > 21 && dealerScore > 21 then
      StateT.liftF(IO.println("Double bust, dealer wins."))
    else if playerScore > 21 then
      StateT.liftF(IO.println("Player Busted, dealer wins"))
    // at this point: player <=21, dealer can be anything.
    else if playerScore == dealerScore then
      StateT.liftF(IO.println("Tie, no payout")).modify((st:BlackJack) => st.copy(playerMoney = st.playerMoney + st.currentBet))
    else if playerScore > dealerScore && playerScore == 21 then // Implies dealer < 21, together with prev.
      StateT.liftF(IO.println("Player wins with blackjack, 3:2 payout!"))
        .modify((st: BlackJack) => st.copy(playerMoney = (st.playerMoney + 2.5 * st.currentBet).toInt)) //1x bet + 1.5 times bet payout
    else if playerScore > dealerScore || dealerScore > 21 then // Here this is not the case, so we check for dealer bust
      StateT
        .liftF(IO.println("Player wins!"))
        .modify((st: BlackJack) => st.copy(playerMoney = st.playerMoney + 2 * st.currentBet))
    else
      StateT
        .liftF(IO.println("Dealer wins!"))

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