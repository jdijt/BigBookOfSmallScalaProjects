package eu.derfniw.project02Birthdays

import cats.effect.IO
import cats.effect.std.Random
import java.time.LocalDate
import munit.{CatsEffectSuite, ScalaCheckEffectSuite, ScalaCheckSuite}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.effect.PropF
import org.scalacheck.{Arbitrary, Gen}

class BirthdaysTest extends CatsEffectSuite with ScalaCheckEffectSuite:

  property("hasDoubleDate detects no duplicate when there is none") {
    forAll { (dates: Set[LocalDate]) =>
      assertEquals(hasDoubleDate(dates.toSeq), false)
    }
  }

  property("hasDoubleDate detects duplicate when there is one") {
    forAll { (dates: Seq[LocalDate]) =>
      dates.nonEmpty ==> {
        assertEquals(hasDoubleDate(dates :+ dates.head), true)
      }
    }
  }

  test("generateBirthDays generation count and year correct") {
    given Random[IO] = Random.javaUtilConcurrentThreadLocalRandom[IO]
    val countGen     = Gen.choose(0, 1000)
    val yearGen      = Gen.choose(LocalDate.MIN.getYear, LocalDate.MAX.getYear)
    PropF.forAllF(countGen, yearGen) { (count: Int, year: Int) =>
      generateBirthdays(count, year).map { dates =>
        assertEquals(dates.length, count)
        assert(dates.forall(_.getYear == year))
      }
    }
  }

  test("generateBirthDays rejects invalid input") {
    given Random[IO] = Random.javaUtilConcurrentThreadLocalRandom[IO]
    intercept[IllegalArgumentException] {
      generateBirthdays(-1, 2000)
    }
  }

  test("runSimulations runs the expected number of simulations") {
    val countGen = Gen.choose(0, 200) // Not too high, keep it fast..
    PropF.forAllF(countGen, countGen) { (simulations, birthdays) =>
      runSimulations(simulations, birthdays).map(res => assertEquals(res.size, simulations))
    }
  }
end BirthdaysTest
