package eu.derfniw.project02Birthdays

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.implicits.*
import cats.effect.std.Random
import cats.syntax.all.*
import java.time.LocalDate

object Strings:
  val question     = "How many birthdays shall I generate?"
  val confirmBegin = "Press enter to begin"

  def start(birthdays: Int) = s"Generating $birthdays random birthdays 100.000 times..."

  def simulationProgress(current: Int) = s"Generated $current simulations."

  def result(birthdays: Int, matches: Int) =
    s"""|Out of 100.000 simulations of $birthdays people there was a matching birthday in that group $matches times.
        |
        |This means that $birthdays people had a ${matches / 1000.0} % chance of having a matching birthday in their group.
        |""".stripMargin
end Strings

def generateBirthdays(count: Int)(using generator: Random[IO]): IO[List[LocalDate]] =
  if count == 0 then Nil.pure
  else
    for day <- generator.betweenInt(1, 366); tail <- generateBirthdays(count - 1)
    yield LocalDate.ofYearDay(2023, day) :: tail

def hasDoubleDate(l: List[LocalDate]): Boolean = l.toSet.size != l.size

def runSimulations(simulationCount: Int, birthdayCount: Int): IO[List[Boolean]] =
  import Strings.*
  val generator = Random.javaUtilConcurrentThreadLocalRandom[IO]
  List
    .range(0, simulationCount)
    .map(sim =>
      for
        dates <- List
                   .range(0, birthdayCount)
                   .map(_ =>
                     for day <- generator.betweenInt(1, 366)
                     yield LocalDate.ofYearDay(2023, day)
                   )
                   .sequence
        _ <- if sim % 10000 == 0 then IO.println(Strings.simulationProgress(sim)) else IO.unit
      yield hasDoubleDate(dates)
    )
    .sequence
end runSimulations

object App extends IOApp.Simple:

  def run: IO[Unit] =
    for
      _       <- IO.println(Strings.question)
      num     <- IO.readLine.map(_.toInt)
      _       <- IO.println(Strings.start(num))
      _       <- IO.println(Strings.confirmBegin)
      _       <- IO.readLine
      results <- runSimulations(100000, num)
      _       <- IO.println(Strings.result(num, results.count(_ == true)))
      _       <- IO.println("Run again? (y or n)")
      answer  <- IO.readLine
      _       <- if answer == "y" || answer == "yes" then run else IO.println("Ok, bye!")
    yield ()
    end for
  end run
end App
