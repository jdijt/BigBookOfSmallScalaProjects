package eu.derfniw.project01Bagels

import cats.effect.*
import cats.effect.IO
import cats.effect.std.Console
import cats.effect.std.Random

val digits  = 3
val guesses = 10
val welcomeText =
  s"""|I am thinking of a $digits-digit number. Try to guess what it is.
      |Here are some clues:
      |When I say: That means:
      | Pico One digit is correct but in the wrong position.
      | Fermi One digit is correct and in the right position.
      | Bagels No digit is correct.""".stripMargin
val numberPickedText =
  s"""|I have thought up a number.
      | You have $guesses guesses to get it.
      |""".stripMargin
val playAgain  = "Do you want to play again? (yes or no)"
val success    = "You got it!"
val thanks     = "Thanks for playing!"
val notANumber = "This is not a number, try again!"

def guess(in: Int)                   = s"Guess #$in"
def invalidInput(in: String): String = s"I do not understand $in. Please say yes or no."
def outOfGuesses(in: String): String = s"You're out of guesses, the number was $in."

def compareValues(guess: String, secret: String): String =
  val clues = guess.zip(secret).collect {
    case (g, a) if g == a             => "Fermi"
    case (g, _) if secret.contains(g) => "Pico"
  }
  if clues.length == 0 then "Bagels" else clues.mkString(" ")

/**
  * Handles a guess.
  * Must know how to complete a game when we run out of guesses or when the guess is correct.
  */
def handleGuess(currentGuess: Int, secretNumber: String, completer: IO[Unit]): IO[Unit] =
  if currentGuess > guesses then Console[IO].println(outOfGuesses(secretNumber)) *> completer
  else
    for
      _     <- Console[IO].println(guess(currentGuess))
      input <- Console[IO].readLine
      _ <-
        if input == secretNumber then Console[IO].println(success) *> completer
        else if input.forall(_.isDigit) then
          Console[IO].println(compareValues(input, secretNumber)) *> handleGuess(
            currentGuess + 1,
            secretNumber,
            completer
          )
        else Console[IO].println(notANumber) *> handleGuess(currentGuess, secretNumber, completer)
    yield ()

/**
  * Completes the game and asks for a restart, must know how to start a game.
  */
def gameCompleted(starter: IO[Unit]): IO[Unit] = for
  _        <- Console[IO].println(playAgain)
  response <- Console[IO].readLine
  _ <- response match
         case "no"  => Console[IO].println(thanks) *> IO.unit
         case "yes" => starter
         case _ =>
           Console[IO].println(invalidInput(response)) *> gameCompleted(starter)
yield ()

/**
  * Generates a secret with the supplied generator & kicks off the guessing.
  */
def startGame(rand: Random[IO]): IO[Unit] = for
  secret <- rand.betweenInt(100, 1000).map(_.toString)
  _      <- Console[IO].println(numberPickedText)
  _      <- handleGuess(1, secret, gameCompleted(startGame(rand)))
yield ()

object Bagels extends IOApp.Simple:
  def run: IO[Unit] = for
    _         <- Console[IO].println(welcomeText)
    generator <- Random.scalaUtilRandom[IO]
    _         <- startGame(generator)
  yield ()
