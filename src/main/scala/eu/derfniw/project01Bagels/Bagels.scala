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

def guessMsg(in: Int)                   = s"Guess #$in"
def invalidInputMsg(in: String): String = s"I do not understand $in. Please say yes or no."
def outOfGuessesMsg(in: String): String = s"You're out of guesses, the number was $in."

def compareValues(guess: String, secret: String): String =
  val clues = guess.zip(secret).collect {
    case (g, a) if g == a             => "Fermi"
    case (g, _) if secret.contains(g) => "Pico"
  }
  if clues.length == 0 then "Bagels" else clues.mkString(" ")

class Bagels(generator: Random[IO], secret: Ref[IO, String]):
  /** Generates a secret with the supplied generator & kicks off the guessing.
    */
  def start: IO[Unit] = for
    newSecret <- generator.betweenInt(100, 1000).map(_.toString)
    _         <- secret.set(newSecret)
    _         <- Console[IO].println(numberPickedText)
    _         <- gameLoop(1)
  yield ()

  /** Completes the game and asks for a restart, must know how to start a game.
    */
  def complete: IO[Unit] = for
    _        <- Console[IO].println(playAgain)
    response <- Console[IO].readLine
    _ <- response match
           case "no"  => Console[IO].println(thanks) *> IO.unit
           case "yes" => start
           case _ =>
             Console[IO].println(invalidInputMsg(response)) *> complete
  yield ()

  def gameLoop(currentGuess: Int): IO[Unit] = for
    secretNumber <- secret.get
    _ <- if currentGuess > guesses then outOfGuesses(secretNumber)
         else handleGuess(currentGuess, secretNumber)
  yield ()

  def outOfGuesses(secretNum: String): IO[Unit] =
    Console[IO].println(outOfGuessesMsg(secretNum)) *> complete

  def handleGuess(currentGuess: Int, secretNumber: String): IO[Unit] = for
    _     <- Console[IO].println(guessMsg(currentGuess))
    input <- Console[IO].readLine
    _ <-
      if input == secretNumber then Console[IO].println(success) *> complete
      else if input.forall(_.isDigit) then
        Console[IO].println(compareValues(input, secretNumber)) *> gameLoop(
          currentGuess + 1
        )
      else Console[IO].println(notANumber) *> handleGuess(currentGuess, secretNumber)
  yield ()
end Bagels

object Bagels extends IOApp.Simple:
  def run: IO[Unit] = for
    _         <- Console[IO].println(welcomeText)
    generator <- Random.scalaUtilRandom[IO]
    secretRef <- Ref[IO].of("000")
    _         <- Bagels(generator, secretRef).start
  yield ()
