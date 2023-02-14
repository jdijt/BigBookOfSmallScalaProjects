package eu.derfniw.project01Bagels

import eu.derfniw.utils.TestConsole

import cats.*
import cats.effect.*
import cats.implicits.*
import concurrent.duration.*
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Test.Parameters
import org.scalacheck.effect.PropF

class BagelsTest extends CatsEffectSuite with ScalaCheckEffectSuite:
  import eu.derfniw.project01Bagels.Bagels.*

  override def munitTimeout: Duration = 5.seconds

  object AskPlayAgainGens:
    import Gen.*
    // Yes and no in random casing
    // There is probably more elegant ways of solving this.
    val yes         = oneOf("yes", "YES", "Yes", "yEs", "yeS", "YEs", "YeS", "yES")
    val no          = oneOf("no", "NO", "nO", "No")
    val validAnswer = oneOf(yes, no)

    val inputWithValidAnswer =
      listOf(frequency((1, validAnswer), (10, Gen.alphaStr)))
        .suchThat(_.exists(s => s.equalsIgnoreCase("yes") || s.equalsIgnoreCase("no")))
  end AskPlayAgainGens

  test("AskPlayAgain should return true for all casings of yes") {
    PropF.forAllF(AskPlayAgainGens.yes) { in =>
      TestConsole.make(in).flatMap(c => askPlayAgain(using c)).map(r => assertEquals(r, true))
    }
  }
  test("AskPlayAgain should return false for all casings of no") {
    PropF.forAllF(AskPlayAgainGens.no) { in =>
      TestConsole.make(in).flatMap(c => askPlayAgain(using c)).map(r => assertEquals(r, false))
    }
  }
  test("AskPlayAgain should terminate if input contains a valid answer") {
    PropF.forAllF(AskPlayAgainGens.inputWithValidAnswer) { in =>
      TestConsole
        .make(in*)
        .flatMap(c => IO.race(askPlayAgain(using c), IO.sleep(1.seconds) >> IO.unit))
        .map {
          case Left(_)  => ()
          case Right(_) => assert(false, "askPlayAgain blocked with valid input")
        }
    }
  }

  object AskValidGuessGens:
    import Gen.*
    val invalidGuess = alphaStr
    // Should always be length 3, but theoretically can be < 3, we need 3 always.
    // Hence the suchThat
    val validGuess   = listOfN(3, numChar).map(_.mkString).suchThat(_.length == 3)
    val validGuesses = listOf(validGuess).suchThat(_.length >= 1)

    val listWithValidGuess = listOf(frequency((1, validGuess), (10, invalidGuess)))
      .suchThat(_.exists(s => s.length == 3 && s.forall(_.isDigit))) // Implicit length >= 1

  test("AskValidGuess should return first valid response.") {
    PropF.forAllF(AskValidGuessGens.validGuesses) { guesses =>
      TestConsole
        .make(guesses*)
        .flatMap(c => askValidGuess(0)(using c))
        .map(result => assertEquals(result, guesses.head))
    }
  }

  test("AskValidGuess should terminate if input contains a valid guess") {
    PropF.forAllF(AskValidGuessGens.listWithValidGuess) { guesses =>
      TestConsole
        .make(guesses*)
        .flatMap(c => IO.race(askValidGuess(0)(using c), IO.sleep(1.seconds) >> IO.unit))
        .map {
          case Left(_)  => ()
          case Right(_) => assert(false, "askValidGuess blocked with valid input")
        }
    }
  }

end BagelsTest
