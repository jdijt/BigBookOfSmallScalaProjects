package eu.derfniw.utils

import eu.derfniw.testutils.TestConsole

import cats.*
import cats.effect.*
import cats.syntax.all.*
import munit.CatsEffectSuite

class ReadValidValueTest extends CatsEffectSuite:

  test(
    "ReadValidValue should return first valid value from the console and not read any further."
  ) {
    val testInput = (1 to 20).map(_.show)
    val validator = (in: String) => if in.toInt >= 10 then Left(in.toInt) else Right("Not yet!")

    for
      c         <- TestConsole.make(testInput)
      res       <- readValidValue[IO, Int]("testPrompt", validator)(using c)
      outLines  <- c.outLines
      linesLeft <- c.leftToRead
    yield
      assertEquals(res, 10, "Expected first valid value: 10")

      val firstLine = outLines.head
      assertEquals(firstLine, "testPrompt", "Expected testPrompt as first output.")

      val errorCount = outLines.count(_ == "Not yet!")
      assertEquals(errorCount, 9, "expected 9x \"Not yet!\" on stdout")

      assertEquals(linesLeft, (11 to 20).map(_.show), "Should not read beyond first valid value")
    end for
  }

end ReadValidValueTest
