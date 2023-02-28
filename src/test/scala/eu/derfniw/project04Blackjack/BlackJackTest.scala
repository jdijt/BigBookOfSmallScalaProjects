package eu.derfniw.project04Blackjack

import CardGenerators.*
import cats.effect.IO
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.scalacheck.effect.PropF.*

class BlackJackTest extends CatsEffectSuite with ScalaCheckEffectSuite:

  private object Gens:
    def stateGen: Gen[BlackJack] = for
      fullDeck        <- validDeck
      playerCardCount <- Gen.chooseNum(1, 10)
      dealerCardCount <- Gen.chooseNum(1, 10)
      currentBet      <- Gen.posNum[Int]
      playerMoney     <- Gen.posNum[Int]
    yield BlackJack(
      playerMoney = playerMoney,
      currentBet = currentBet,
      playerHand = fullDeck.slice(0, playerCardCount),
      dealerHand = fullDeck.slice(playerCardCount, playerCardCount + dealerCardCount),
      deck = fullDeck.drop(playerCardCount + dealerCardCount)
    )
  end Gens

  test("RenewDeck should provide a clean state.") {
    PropF.forAllF(Gens.stateGen) { startingState =>
      BlackJack.renewDeck.runS(startingState).map { renewedState =>
        assertEquals(renewedState.playerMoney, startingState.playerMoney)
        assertEquals(renewedState.currentBet, 0)
        assertEquals(renewedState.playerHand, Nil)
        assertEquals(renewedState.dealerHand, Nil)
        assertEquals(renewedState.deck.toSet.size, 52)
      }
    }
  }

  test("AddPlayerCard should append card to player hand & keep state valid") {
    PropF.forAllF(Gens.stateGen.suchThat(_.deck.nonEmpty)) { startingState =>
      BlackJack.drawPlayerCard.runS(startingState).map { endState =>
        assertEquals(endState.playerHand.length, startingState.playerHand.length + 1)
        assertEquals(
          endState.playerHand.length + endState.dealerHand.length + endState.deck.length,
          52,
          "Total number of cards in the game must be 1 deck"
        )
      }
    }
  }

  test("AddDealerCard should append card to dealer hand & keep state valid") {
    PropF.forAllF(Gens.stateGen.suchThat(_.deck.nonEmpty)) { startingState =>
      BlackJack.drawDealerCard.runS(startingState).map { endState =>
        assertEquals(endState.dealerHand.length, startingState.dealerHand.length + 1)
        assertEquals(
          endState.playerHand.length + endState.dealerHand.length + endState.deck.length,
          52,
          "Total number of cards in the game must be 1 deck"
        )
      }
    }
  }
end BlackJackTest
