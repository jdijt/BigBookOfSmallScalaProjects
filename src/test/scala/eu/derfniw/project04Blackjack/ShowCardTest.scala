package eu.derfniw.project04Blackjack

import eu.derfniw.project04Blackjack.CardGenerators.given

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class ShowCardTest extends ScalaCheckSuite:

  property("cards printed closed should only contain \"{_,|, ,#}\"") {
    val allowed = Set(' ', '|', '_', '#')
    forAll { (c: Card) =>
      val badChars = c.showCard(false).flatMap(_.filterNot(allowed))
      assert(badChars.isEmpty)
    }
  }

  property("cards printed open should not contain \"#\" ") {
    forAll { (c: Card) =>
      assert(!c.showCard(true).flatten.contains('#'))
    }
  }

  property("printed cards should be rectangular") {
    forAll { (c: Card, open: Boolean) =>
      val lines = c.showCard(open)
      assertEquals(lines.length, 4, "Card is 4 lines high")
      assert(lines.forall(_.length == 5), "Card is 5 chars wide")
    }
  }

  property("Printed lists of cards should expand horizontally") {
    forAll { (cards: List[Card], open: Boolean) =>
      cards.nonEmpty ==> {
        val lines = cards.showCard(open)
        assertEquals(lines.length, 4)
        // 1 char separator between all cards.
        assert(lines.forall(_.length == cards.length * 5 + cards.length - 1))
      }
    }
  }
end ShowCardTest
