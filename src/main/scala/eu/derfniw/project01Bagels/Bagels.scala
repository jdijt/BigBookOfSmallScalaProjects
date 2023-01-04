package eu.derfniw.project01Bagels

import cats.effect.*
import cats.effect.IO
import cats.effect.std.Console
import cats.effect.std.Random

val digits  = 3
val guesses = 10

class SingleGame(secret: String):
  private val numberPickedText = s"""|I have thought up a number.
                                     | You have $guesses guesses to get it.""".stripMargin
  private val outOfGuesses: String = s"You're out of guesses, the number was $secret."
  private val notANumber           = "This is not a number, try again!"
  private val success: String      = "You got it!"
  private def guessMsg(in: Int)    = s"Guess #$in"

  /** Generates a secret with the supplied generator & kicks off the guessing.
    */
  def start: IO[Unit] = for
    _ <- Console[IO].println(numberPickedText)
    _ <- loop(1)
  yield ()

  private def getClues(guess: String): String =
    val clues = guess.zip(secret).collect {
      case (g, a) if g == a             => "Fermi"
      case (g, _) if secret.contains(g) => "Pico"
    }
    if clues.length == 0 then "Bagels" else clues.mkString(" ")

  private def loop(currentGuess: Int): IO[Unit] =
    if currentGuess > guesses then Console[IO].println(outOfGuesses)
    else
      for
        _     <- Console[IO].println(guessMsg(currentGuess))
        input <- Console[IO].readLine
        _ <-
          if input == secret then Console[IO].println(success)
          else if input.length == 3 && input.forall(_.isDigit) then
            Console[IO].println(getClues(input)) *> loop(currentGuess + 1)
          else Console[IO].println(notANumber) *> loop(currentGuess)
      yield ()
end SingleGame

object Bagels extends IOApp.Simple:
  val welcomeText =
    s"""|I am thinking of a $digits-digit number. Try to guess what it is.
        |Here are some clues:
        |When I say: That means:
        | Pico One digit is correct but in the wrong position.
        | Fermi One digit is correct and in the right position.
        | Bagels No digit is correct.""".stripMargin
  val playAgain                           = "Do you want to play again? (yes or no)"
  val thanks                              = "Thanks for playing!"
  def invalidInputMsg(in: String): String = s"I do not understand $in. Please say yes or no."

  def run: IO[Unit] = for
    _         <- Console[IO].println(welcomeText)
    generator <- Random.scalaUtilRandom[IO]
    _         <- runGame(generator)
  yield ()

  def runGame(rand: Random[IO]): IO[Unit] = for
    secret  <- rand.betweenInt(0, 1000).map(i => f"$i%03d")
    _       <- SingleGame(secret).start
    restart <- askPlayAgain
    _       <- if restart then runGame(rand) else Console[IO].println(thanks)
  yield ()

  def askPlayAgain: IO[Boolean] = for
    _        <- Console[IO].println(playAgain)
    response <- Console[IO].readLine
    result <- response match
                case "yes" => IO.pure(true)
                case "no"  => IO.pure(false)
                case _     => Console[IO].println(invalidInputMsg(response)) *> askPlayAgain
  yield result
end Bagels
