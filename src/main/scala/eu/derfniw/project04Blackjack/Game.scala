package eu.derfniw.project04Blackjack

import eu.derfniw.utils.readValidValue

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.std.Console
import cats.effect.syntax.all.*
import cats.syntax.all.*

type GameState[A] = StateT[IO, BlackJack, A]

object Game extends IOApp.Simple:
  import BlackJack.*

  override def run: IO[Unit] = runGame

  def runGame(using c: Console[IO]): IO[Unit] = for
    _  <- c.println(Text.rules)
    fs <- gameLoop.runS(initialState)
    _  <- c.println(s"You completed the game with ${fs.playerMoney} money in the end!")
  yield ()

  def gameLoop: GameState[Unit] =
    for
      _   <- renewDeck
      bet <- initialBet
      _ <- bet match
             case None => StateT.pure[IO, BlackJack, Unit](())
             case Some(bet) =>
               for
                 _           <- incrementPlayerBet(bet)
                 _           <- dealInitialHands
                 playerScore <- playerActions()
                 dealerScore <- dealerActions
                 _           <- processWinLose(playerScore, dealerScore)
                 _           <- gameLoop
               yield ()
    yield ()

  def initialBet(using c: Console[IO]): GameState[Option[Int]] =
    StateT.inspectF { st =>
      if st.playerMoney == 0 then c.println("You're broke!") >> None.pure[IO]
      else
        readValidValue(
          s"How much do you bet? (1-${st.playerMoney}, or QUIT)",
          parseBetInput(st)
        )
    }
  end initialBet

  def parseBetInput(st: BlackJack)(in: String): Either[Option[Int], String] =
    in.toIntOption match
      case Some(i) if i > 0 & i <= st.playerMoney => Left(Some(i))
      case None if in == "QUIT"                   => Left(None)
      case Some(i) if i > st.playerMoney          => Right("You cannot bet more than you have!")
      case Some(_)                                => Right("Bet must be greater than 0")
      case None                                   => Right(s"Invalid number: $in")

  def parsePlayerAction(first: Boolean)(in: String): Either[PlayerAction, String] =
    in.toLowerCase match
      case "h"          => Left(PlayerAction.Hit)
      case "s"          => Left(PlayerAction.Stand)
      case "d" if first => Left(PlayerAction.Double)
      case _            => Right(s"""Invalid input: "$in"""")

  enum PlayerAction:
    case Hit
    case Stand
    case Double

  /** Processes the effects of a players action on the state And returns the players new score.
    *
    * @param action
    * @return
    */
  def processPlayerAction(action: PlayerAction): GameState[Int] = action match
    case PlayerAction.Hit =>
      for
        card     <- drawCard
        newScore <- addPlayerCard(card)
      yield newScore
    case PlayerAction.Stand => StateT.inspect(_.playerHand.sumCards)
    case PlayerAction.Double =>
      for
        _        <- doubleBet
        cards    <- List(drawCard, drawCard).sequence
        newScore <- cards.traverse(addPlayerCard).map(_.last)
      yield newScore

  def playerActions(first: Boolean = true)(using c: Console[IO]): GameState[Int] =
    val prompt = s"(H): Hit, (S): Stand" + (if first then ", (D): Double down" else "")
    for
      _        <- printState(false)
      action   <- StateT.liftF(readValidValue(prompt, parsePlayerAction(first)))
      newScore <- processPlayerAction(action)
      finalScore <-
        if newScore > 21 then
          StateT.liftF(c.println("You went bust!, dealer's turn now") >> newScore.pure[IO])
        else if newScore == 21 then
          StateT.liftF(c.println("Blackjack!, dealer's turn!") >> newScore.pure[IO])
        else if action == PlayerAction.Stand then StateT.liftF(newScore.pure[IO])
        else playerActions(false)
    yield finalScore
    end for
  end playerActions

  def dealerActions(using c: Console[IO]): GameState[Int] =
    for
      _ <- BlackJack.printState(true)
      _ <- StateT.liftF(c.println("Press enter to let dealer continue...") >> c.readLine)
      newScore <- StateT
                    .inspect[IO, BlackJack, Int]((st: BlackJack) => st.dealerHand.sumCards)
                    .flatMap(score =>
                      if score > 17 then StateT.liftF(score.pure)
                      else
                        for
                          card       <- drawCard
                          _          <- addDealerCard(card)
                          finalScore <- dealerActions
                        yield finalScore
                    )
    yield newScore

  def processWinLose(playerScore: Int, dealerScore: Int)(using c: Console[IO]): GameState[Unit] =
    if playerScore > 21 && dealerScore > 21 then
      StateT.liftF(c.println("Double bust, dealer wins."))
    else if playerScore > 21 then StateT.liftF(c.println("Player Busted, dealer wins"))
    // at this point: player <=21, dealer can be anything.
    else if playerScore == dealerScore then
      StateT
        .liftF(c.println("Tie, no payout"))
        .modify((st: BlackJack) => st.copy(playerMoney = st.playerMoney + st.currentBet))
    else if playerScore > dealerScore && playerScore == 21 then // Implies dealer < 21, together with prev.
      StateT
        .liftF(c.println("Player wins with blackjack, 3:2 payout!"))
        .modify((st: BlackJack) =>
          st.copy(playerMoney = (st.playerMoney + 2.5 * st.currentBet).toInt)
        ) // 1x bet + 1.5 times bet payout
    else if playerScore > dealerScore || dealerScore > 21 then // Here this is not the case, so we check for dealer bust
      StateT
        .liftF(c.println("Player wins!"))
        .modify((st: BlackJack) => st.copy(playerMoney = st.playerMoney + 2 * st.currentBet))
    else
      StateT
        .liftF(c.println("Dealer wins!"))
end Game
