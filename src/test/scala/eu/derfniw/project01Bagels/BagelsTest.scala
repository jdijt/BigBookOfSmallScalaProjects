package eu.derfniw.project01Bagels

import eu.derfniw.testutils.TestConsole

import cats.*
import cats.effect.*
import cats.implicits.*
import concurrent.duration.*
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.scalacheck.Test.Parameters
import org.scalacheck.effect.PropF

class BagelsTest extends CatsEffectSuite with ScalaCheckEffectSuite:
  import eu.derfniw.project01Bagels.Bagels.*

  override def munitTimeout: Duration = 5.seconds

  object Gens:
    import Gen.*
    // Should always be length 3, but theoretically can be < 3, we need 3 always.
    // Hence the suchThat
    val validGuess: Gen[String] = listOfN(3, numChar).map(_.mkString).suchThat(_.length == 3)

    val playAgainAnswer: Gen[String] =
      oneOf("yes", "Yes", "yEs", "yeS", "YEs", "YeS", "yES", "YES", "no", "nO", "No", "NO")

  property("parseValidGuess should only accept input with length 3") {
    forAll(Gen.numStr) { num =>
      parseValidGuess(num) match
        case Left(n) if n.length == 3 => assertEquals(n, num)
        case Left(_)                  => assert(false, "Accepted input with length not 3")
        case Right(_)                 => assert(num.length != 3)
    }
  }

  property("parseValidGuess should fail on all non-numeric strings") {
    forAll { (s: String) =>
      (!s.forall(_.isDigit)) ==> assert(parseValidGuess(s).isRight)
    }
  }

  property("parseValidGuess should return the value of a valid guess") {
    forAll(Gens.validGuess)(n => assertEquals(parseValidGuess(n), Left(n)))
  }

  test("parsePlayAgain should return true for yes, and false for no") {
    assertEquals(parsePlayAgain("yes"), Left(true))
    assertEquals(parsePlayAgain("no"), Left(false))
  }

  property("parsePlayAgain should accept valid answers in any casing combination") {
    forAll(Gens.playAgainAnswer)(in => assert(parsePlayAgain(in).isLeft))
  }

  property("parsePlayAgain should not accept any other values") {
    forAll { (in: String) =>
      (in.toLowerCase != "yes" && in.toLowerCase != "no") ==> {
        assert(parsePlayAgain(in).isRight)
      }
    }
  }

  property("getClues should return 1 to 3 hints") {
    forAll(Gens.validGuess, Gens.validGuess) { (secret, guess) =>
      val hints = getClues(secret, guess)
      assert(hints.nonEmpty && hints.length <= 3, s"Hints returned wrong size list: ${hints.size}")
    }
  }

  property("getClues should only return \"Bagels\" when nothing matches.") {
    forAll(Gens.validGuess, Gens.validGuess) { (secret, guess) =>
      (secret.toSet & guess.toSet).isEmpty ==> {
        val hints = getClues(secret, guess)
        assertEquals(hints.head, "Bagels")
      }
    }
  }

  property("getClues should return at least as many hints as matching numbers") {
    // Multiple in case of duplicate matches :)
    forAll(Gens.validGuess, Gens.validGuess) { (secret, guess) =>
      val intersected = secret.toSet & guess.toSet
      intersected.nonEmpty ==> {
        val hints = getClues(secret, guess)
        assert(hints.size >= intersected.size, "Should return as many hints as matching numbers.")
      }
    }
  }

  test("SingleGame should terminate when out of guesses") {
    // This game terminates after 1 bad guess, or it should at least.
    val testState = new GameState("123", 1)
    val input     = List("456")

    for
      console  <- TestConsole.make(input)
      endState <- singleGame(using console).runS(testState)
      stdOut   <- console.outLines
    yield
      assertEquals(endState.guesses, 0, "Should be no guesses left")
      assertEquals(stdOut.last, Strings.outOfGuesses("123"))
  }

  test("SingleGame should terminate on correct guess") {
    val testState = new GameState("123", 2)
    val input     = List("123")

    for
      console  <- TestConsole.make(input)
      endState <- singleGame(using console).runS(testState)
      stdOut   <- console.outLines
    yield
      assertEquals(endState.guesses, 1, "Should be one guess left")
      assertEquals(stdOut.last, Strings.success)
  }
end BagelsTest
