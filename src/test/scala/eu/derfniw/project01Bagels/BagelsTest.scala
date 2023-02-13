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

  override def scalaCheckTestParameters = super.scalaCheckTestParameters.withMinSuccessfulTests(100)

  object AskPlayAgainGens:
    import Gen.*
    // Yes and no in random casing
    // There is probably more elegant ways of solving this.
    val yes         = oneOf("yes", "YES", "Yes", "yEs", "yeS", "YEs", "YeS", "yES")
    val no          = oneOf("no", "NO", "nO", "No")
    val validAnswer = oneOf(yes, no)

    val inputWithValidAnswer =
      listOf(frequency((1, validAnswer), (20, arbitrary[String])))
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
          case Right(_) => assert(false, "askPlayAgain timed out")
        }
    }
  }

end BagelsTest
