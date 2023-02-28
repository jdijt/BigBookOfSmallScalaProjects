package eu.derfniw.project04Blackjack

import org.scalacheck.{Arbitrary, Gen}

import scala.util.Random

object CardGenerators:

  private val suitGen: Gen[Suit] = Gen.oneOf(
    Suit.Hearts,
    Suit.Diamonds,
    Suit.Clubs,
    Suit.Spades
  )

  private val rankGen: Gen[Rank] = Gen.oneOf(
    Seq(
      Rank.Ace,
      Rank.King,
      Rank.Queen,
      Rank.Jack
    ) ++ (2 to 10).map(Rank.Number.apply)
  )

  val cardGen: Gen[Card] = for
    rank <- rankGen
    suit <- suitGen
  yield Card(suit, rank)

  val validDeck: Gen[List[Card]] =
    // Take the seed from ScalaCheck to shuffle
    // Deterministically based on ScalaChecks internal seed.
    Gen.long.map(seed =>
      val shuffler = scala.util.Random(seed)
      shuffler.shuffle(Deck.allCardsSorted)
    )

  given Arbitrary[Card] = Arbitrary(cardGen)
end CardGenerators
