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

  val validPlayAgainAnswer = Gen.oneOf("Yes", "yes", "YES", "No", "no", "NO")
  test("AskPlayAgain should return the correct result for a given valid input") {
    TestConsole.make("Yes").flatMap(c => askPlayAgain(using c)).map(r => assertEquals(r, true))
    TestConsole.make("No").flatMap(c => askPlayAgain(using c)).map(r => assertEquals(r, false))
  }
  test("AskPlayAgain should terminate if input contains a valid answer") {
    PropF.forAllF(arbitrary[Seq[String]], validPlayAgainAnswer) { (prefix, last) =>
      val expected = last.toLowerCase() == "yes"
      TestConsole
        .make((prefix :+ last)*)
        .flatMap(c => askPlayAgain(using c))
        .map(r => assertEquals(r, expected))
    }
  }

end BagelsTest
