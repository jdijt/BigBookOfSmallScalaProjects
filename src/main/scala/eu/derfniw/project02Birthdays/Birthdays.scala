package eu.derfniw.project02Birthdays

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.implicits.*
import cats.effect.std.Random
import cats.syntax.all.*
import java.time.LocalDate

object Strings:
  val question: String     = "How many birthdays shall I generate?"
  val confirmBegin: String = "Press enter to begin"

  def start(birthdays: Int): String = s"Generating $birthdays random birthdays 10.000 times..."

  def result(birthdays: Int, matches: Int): String =
    s"""|Out of 10.000 simulations of $birthdays people there was a matching birthday in that group $matches times.
        |
        |This means that $birthdays people had a ${matches / 100.0} % chance of having a matching birthday in their group.
        |""".stripMargin
end Strings

def generateBirthdays(count: Int, year: Int)(using generator: Random[IO]): IO[Vector[LocalDate]] =
  require(count >= 0, "Count must be > 0")
  val yearLength = LocalDate.ofYearDay(year, 1).lengthOfYear()
  Vector
    .unfold(count) { c =>
      if c <= 0 then None
      else
        val day = generator.betweenInt(1, yearLength + 1).map(day => LocalDate.ofYearDay(year, day))
        Some((day, c - 1))
    }
    .sequence
end generateBirthdays

def hasDoubleDate(l: Seq[LocalDate]): Boolean = l.toSet.size != l.size

def runSimulations(simulationCount: Int, birthdayCount: Int): IO[Vector[Boolean]] =
  require(simulationCount >= 0, "Need a positive number of simulations to run.")
  import Strings.*
  given Random[IO] = Random.javaUtilConcurrentThreadLocalRandom[IO]
  Vector
    .unfold(simulationCount) { c =>
      if c <= 0 then None
      else
        val simResult = generateBirthdays(birthdayCount, LocalDate.now().getYear).map(hasDoubleDate)
        Some(simResult, c - 1)
    }
    .parSequence
end runSimulations

object App extends IOApp.Simple:

  def run: IO[Unit] =
    for
      _       <- IO.println(Strings.question)
      num     <- IO.readLine.map(_.toInt)
      _       <- IO.println(Strings.start(num))
      _       <- IO.println(Strings.confirmBegin)
      _       <- IO.readLine
      results <- runSimulations(10000, num)
      _       <- IO.println(Strings.result(num, results.count(_ == true)))
      _       <- IO.println("Run again? (y or n)")
      answer  <- IO.readLine
      _       <- if answer == "y" || answer == "yes" then run else IO.println("Ok, bye!")
    yield ()
    end for
  end run
end App
