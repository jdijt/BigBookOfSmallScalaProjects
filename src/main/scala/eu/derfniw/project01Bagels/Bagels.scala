package eu.derfniw.project01Bagels

import scala.annotation.tailrec

import cats.*
import cats.data.StateT
import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.Random
import cats.implicits.*

object Strings:
  val success = "You got it!"
  val welcomeText =
    s"""|I am thinking of a 3-digit number. Try to guess what it is.
      |Here are some clues:
      |When I say:    That means:
      |  Pico         One digit is correct but in the wrong position.
      |  Fermi        One digit is correct and in the right position.
      |  Bagels       No digit is correct.""".stripMargin
  val playAgain                           = "Do you want to play again? (yes or no)"
  val thanks                              = "Thanks for playing!"
  def invalidInputMsg(in: String): String = s"I do not understand $in. Please try again."

  val numberPickedText =
    s"""|I have thought up a number.
        | You have ${GameState.defaultGuesses} guesses to get it.""".stripMargin
  def outOfGuesses(secret: String) = s"You're out of guesses, the number was $secret."
  def guessMsg(in: Int)            = s"Guesses left: $in"
end Strings

case class GameState(secret: String, guesses: Int):
  def decrementGuesses = this.copy(guesses = guesses - 1)

object GameState:
  val defaultGuesses = 10

  def apply(secret: String): GameState = new GameState(secret, defaultGuesses)

object Bagels extends IOApp.Simple:
  import Strings.*

  def run: IO[Unit] =
    val c = summon[Console[IO]]
    for
      generator <- Random.scalaUtilRandom[IO]
      _         <- c.println(welcomeText)
      _         <- mainLoop(generator)
    yield ()

  def mainLoop(rand: Random[IO])(using c: Console[IO]): IO[Unit] = for
    secret <- rand.betweenInt(1, 1000).map(i => f"$i%03d")
    _      <- c.println(numberPickedText)
    again  <- singleGame.runA(GameState(secret))
    _      <- if again then mainLoop(rand) else c.println(thanks)
  yield ()

  def singleGame(using c: Console[IO]): StateT[IO, GameState, Boolean] = for
    guess <- StateT.apply[IO, GameState, String](st =>
               askValidGuess(st.guesses).map(guess => (st.decrementGuesses, guess))
             )
    state <- StateT.get[IO, GameState]
    again <- if guess == state.secret then StateT.liftF(c.println(success) >> askPlayAgain)
             else if state.guesses == 0 then
               StateT.liftF(c.println(outOfGuesses(state.secret)) >> askPlayAgain)
             else
               val clues = getClues(state.secret, guess)
               StateT.liftF(c.println(clues.mkString(" "))) >> singleGame
  yield again

  def askPlayAgain(using c: Console[IO]): IO[Boolean] = for
    _        <- c.println(playAgain)
    response <- c.readLine
    result <- response.toLowerCase() match
                case "yes" => IO.pure(true)
                case "no"  => IO.pure(false)
                case _     => c.println(invalidInputMsg(response)) >> askPlayAgain
  yield result

  def askValidGuess(guesses: Int)(using c: Console[IO]): IO[String] = for
    _    <- c.println(guessMsg(guesses))
    read <- c.readLine
    valid <- if read.length == 3 && read.forall(_.isDigit) then read.pure[IO]
             else c.println(invalidInputMsg(read)) >> askValidGuess(guesses)
  yield valid

  def getClues(secret: String, guess: String): List[String] =
    val clues = guess.toList.zip(secret).collect {
      case (g, a) if g == a             => "Fermi"
      case (g, _) if secret.contains(g) => "Pico"
    }
    if clues.length == 0 then List("Bagels") else clues
end Bagels
