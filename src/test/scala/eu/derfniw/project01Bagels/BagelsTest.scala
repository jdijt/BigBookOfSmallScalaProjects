package eu.derfniw.project01Bagels

import eu.derfniw.utils.TestConsole

import cats.*
import cats.implicits.*
import munit.Assertions
import munit.CatsEffectSuite
import munit.ScalaCheckSuite

class BagelsTest extends CatsEffectSuite:
  import eu.derfniw.project01Bagels.Bagels.*

  test("AskPlayAgain basic cases") {
    val testCases = Vector(
      (TestConsole.make(Seq("Yes")), true),
      (TestConsole.make(Seq("No")), false),
      (TestConsole.make(Seq("No", "yes")), false),
      (TestConsole.make(Seq("yes", "No")), false)
    )
    testCases.traverse { (console, result) =>
      for
        c <- console
        _ <- assertIO[Boolean, Boolean](askPlayAgain(using c), result)
      yield ()
    }
  }
end BagelsTest
