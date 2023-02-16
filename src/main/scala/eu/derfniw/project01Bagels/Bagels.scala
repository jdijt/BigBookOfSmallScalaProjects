package eu.derfniw.project01Bagels

import eu.derfniw.utils.readValidValue

import scala.annotation.tailrec

import cats.*
import cats.data.StateT
import cats.effect.*
import cats.effect.std.Console
import cats.effect.std.Random
import cats.implicits.*

object Strings:
  val success: String = "You got it!"
  val welcomeText: String =
    s"""|I am thinking of a 3-digit number. Try to guess what it is.
      |Here are some clues:
      |When I say:    That means:
      |  Pico         One digit is correct but in the wrong position.
      |  Fermi        One digit is correct and in the right position.
      |  Bagels       No digit is correct.""".stripMargin
  val playAgain: String                   = "Do you want to play again? (yes or no)"
  val thanks: String                      = "Thanks for playing!"
  def invalidInputMsg(in: String): String = s"I do not understand $in. Please try again."

  val numberPickedText: String =
    s"""|I have thought up a number.
        | You have ${GameState.defaultGuesses} guesses to get it.""".stripMargin
  def outOfGuesses(secret: String) = s"You're out of guesses, the number was $secret."
  def guessMsg(in: Int)            = s"Guesses left: $in"
end Strings

case class GameState(secret: String, guesses: Int):
  def decrementGuesses: GameState = this.copy(guesses = guesses - 1)

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

  def singleGame(using c: Console[IO]): StateT[IO, GameState, Boolean] =
    for
      guess <- StateT.apply[IO, GameState, String](st =>
                 readValidValue(guessMsg(st.guesses), parseValidGuess).map(guess =>
                   (st.decrementGuesses, guess)
                 )
               )
      state <- StateT.get[IO, GameState]
      again <-
        if guess == state.secret then
          StateT.liftF(c.println(success) >> readValidValue(playAgain, parsePlayAgain))
        else if state.guesses == 0 then
          StateT.liftF(
            c.println(outOfGuesses(state.secret)) >> readValidValue(
              playAgain,
              parsePlayAgain
            )
          )
        else
          val clues = getClues(state.secret, guess)
          StateT.liftF(c.println(clues.mkString(" "))) >> singleGame
    yield again
    end for
  end singleGame

  def parsePlayAgain(in: String): Either[Boolean, String] = in.toLowerCase() match
    case "yes" => Left(true)
    case "no"  => Left(false)
    case _     => Right(invalidInputMsg(in))

  def parseValidGuess(in: String): Either[String, String] =
    if in.length == 3 && in.forall(_.isDigit) then Left(in)
    else Right(invalidInputMsg(in))

  def getClues(secret: String, guess: String): List[String] =
    val clues = guess.toList.zip(secret).collect {
      case (g, a) if g == a             => "Fermi"
      case (g, _) if secret.contains(g) => "Pico"
    }
    if clues.isEmpty then List("Bagels") else clues
end Bagels
