package eu.derfniw.project04Blackjack

import CardGenerators.*
import CardGenerators.given
import munit.{CatsEffectSuite, ScalaCheckSuite}
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class DeckTest extends CatsEffectSuite with ScalaCheckSuite:

  test("ShuffledDeck should contain one of each card") {
    Deck.shuffledDeck.map(cards => assertEquals(cards.toSet.size, 52))
  }

  property(
    "Sumcards should always return a value greater or equal than the nr of cards passed in"
  ) {
    forAll((cs: List[Card]) => assert(cs.sumCards >= cs.length))
  }

  property("Sumcards should always pick the score closest to 21") {
    forAll(Gen.resize(4, Gen.listOf(cardGen))) { cards =>
      cards.forall(_.rank != Rank.Ace) ==> {
        val score             = cards.sumCards
        val scoreWithExtraAce = cards.appended(Card(Suit.Spades, Rank.Ace)).sumCards
        val scoreDiff         = math.abs(score - scoreWithExtraAce)

        // The sum should avoid going over 21, and also when over 21 pick the score closest to 21.
        // So in all those cases the extra ace should only add one point.
        // Only if the current score is <= 10, the ace should add 11.
        // As this tests allows for only the ace that we add later, we avoid a bunch of edge cases
        // While still triggering the logic that we want to test.
        if score > 10 then assertEquals(scoreDiff, 1)
        else assertEquals(scoreDiff, 11)
      }
    }
  }

end DeckTest
