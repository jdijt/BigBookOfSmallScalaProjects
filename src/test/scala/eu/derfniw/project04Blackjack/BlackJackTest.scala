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

  def assertStateInvariant(st: BlackJack): Unit =
    val allCards        = st.playerHand ++ st.dealerHand ++ st.deck
    val uniqueCardCount = allCards.toSet.size
    assertEquals(uniqueCardCount, 52, "There is exactly one whole deck in play")

  test("RenewTable should provide a valid clean state.") {
    PropF.forAllF(Gens.stateGen) { startingState =>
      BlackJack.renewTable.runS(startingState).map { renewedState =>
        assertEquals(renewedState.playerMoney, startingState.playerMoney)
        assertEquals(renewedState.currentBet, 0)
        assertEquals(renewedState.playerHand.size, 2)
        assertEquals(renewedState.dealerHand.size, 2)
        assertStateInvariant(renewedState)
      }
    }
  }

  test("DrawPlayerCard should append card to player hand & keep state valid") {
    PropF.forAllF(Gens.stateGen.suchThat(_.deck.nonEmpty)) { startingState =>
      BlackJack.drawPlayerCard.runS(startingState).map { endState =>
        assertEquals(endState.playerHand.length, startingState.playerHand.length + 1)
        assertStateInvariant(endState)
      }
    }
  }

  test("DrawDealerCard should append card to dealer hand & keep state valid") {
    PropF.forAllF(Gens.stateGen.suchThat(_.deck.nonEmpty)) { startingState =>
      BlackJack.drawDealerCard.runS(startingState).map { endState =>
        assertEquals(endState.dealerHand.length, startingState.dealerHand.length + 1)
        assertStateInvariant(endState)
      }
    }
  }

  test("Playerscore should not change state & calculate correct score.") {
    PropF.forAllF(Gens.stateGen) { st =>
      BlackJack.playerScore.run(st).map { (endState, score) =>
        assert(endState == st, "State should be unchanged")
        // The minimum score of a card is 1
        // Similarly the maximum score is 11, so it must be less than 11 * size
        // Below only fails if a score is impossible given the card count.
        // More in depth testing of sumCards is done in eu.derfniw.project04Blackjack.DeckTest
        assert(score >= st.playerHand.size && score <= (st.playerHand.size * 11))
        assertStateInvariant(endState)
      }
    }
  }
  test("Dealerscore should not change state & calculate correct score") {
    PropF.forAllF(Gens.stateGen) { st =>
      BlackJack.dealerScore.run(st).map { (endState, score) =>
        assert(endState == st, "State should be unchanged")
        assert(score >= st.dealerHand.size && score <= (st.dealerHand.size * 11))
      }
    }
  }
  test("Incrementplayerbet should not alter the total amount of money the player has") {
    PropF.forAllF(Gens.stateGen, Gen.posNum[Int]) { (st, bet) =>
      BlackJack.incrementPlayerBet(bet).runS(st).map { endState =>
        assertEquals(endState.currentBet + endState.playerMoney, st.currentBet + st.playerMoney)
        assertStateInvariant(st)
      }
    }
  }
  test("Doublebet should double the bet & not alter the total amount of money the player has") {
    PropF.forAllF(Gens.stateGen) { st =>
      BlackJack.doubleBet.runS(st).map { endState =>
        assertEquals(endState.currentBet, 2 * st.currentBet)
        assertEquals(endState.currentBet + endState.playerMoney, st.currentBet + st.playerMoney)
        assertStateInvariant(endState)
      }
    }
  }
  test("PayOutBet should increase the amount of money in the game if bet > 0"){
    PropF.forAllF(Gens.stateGen.suchThat(_.currentBet > 0), Gen.posNum[Double].suchThat(_ > 1)){ (st, factor) =>
      BlackJack.payOutBet(factor).runS(st).map{ endstate =>
        assertEquals(endstate.currentBet, 0)
        assert(endstate.playerMoney > st.playerMoney + st.currentBet)
        assertStateInvariant(st)
      }
    }
  }

end BlackJackTest
