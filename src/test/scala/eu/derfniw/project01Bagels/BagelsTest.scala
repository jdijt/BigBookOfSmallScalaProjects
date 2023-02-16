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

end BagelsTest
