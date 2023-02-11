package eu.derfniw.project01Bagels

import scala.annotation.tailrec

import cats.effect.*
import cats.effect.IO
import cats.effect.std.Console
import cats.effect.std.Random

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

  def numberPickedText(guesses: Int) =
    s"""|I have thought up a number.
        | You have $guesses guesses to get it.""".stripMargin
  def outOfGuesses(secret: String) = s"You're out of guesses, the number was $secret."
  def guessMsg(in: Int)            = s"Guess #$in"
end Strings

class SingleGame(secret: String, guesses: Int)(using c: Console[IO]):
  import Strings.*

  def start: IO[Unit] = for
    _ <- c.println(numberPickedText(guesses))
    _ <- loop(1)
  yield ()

  private def getClues(guess: String): String =
    val clues = guess.zip(secret).collect {
      case (g, a) if g == a             => "Fermi"
      case (g, _) if secret.contains(g) => "Pico"
    }
    if clues.length == 0 then "Bagels" else clues.mkString(" ")

  private def loop(currentGuess: Int): IO[Unit] =
    if currentGuess > guesses then c.println(outOfGuesses(secret))
    else
      for
        _     <- c.println(guessMsg(currentGuess))
        input <- c.readLine
        _ <-
          if input == secret then c.println(success)
          else if input.length == 3 && input.forall(_.isDigit) then
            c.println(getClues(input)) >> loop(currentGuess + 1)
          else c.println(invalidInputMsg(input)) >> loop(currentGuess)
      yield ()
end SingleGame

class MultiGameRunner(rand: Random[IO])(using c: Console[IO]):
  import Strings.*

  val guesses = 10

  val run: IO[Unit] = for
    _      <- c.println(welcomeText)
    secret <- rand.betweenInt(1, 1000).map(i => f"$i%03d")
    _      <- SingleGame(secret, guesses).start
    again  <- askPlayAgain
    _      <- if again then run else c.println(thanks)
  yield ()

  val askPlayAgain: IO[Boolean] = for
    _        <- c.println(playAgain)
    response <- c.readLine
    result <- response match
                case "yes" => IO.pure(true)
                case "no"  => IO.pure(false)
                case _     => c.println(invalidInputMsg(response)) >> askPlayAgain
  yield result
end MultiGameRunner

object Bagels extends IOApp.Simple:
  def run: IO[Unit] = for
    generator <- Random.scalaUtilRandom[IO]
    _         <- MultiGameRunner(generator).run
  yield ()

end Bagels
