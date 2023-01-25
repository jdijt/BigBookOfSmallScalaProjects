package eu.derfniw.project04Blackjack

import cats.effect.IO
import cats.effect.IOApp

object Blackjack extends IOApp.Simple:

  def run: IO[Unit] = for
    _ <- IO.println(Text.rules)
    _ <- game(5000)
  yield ()

  def game(playerMoney: Int): IO[Unit] = for
    (bet, score) <- playerActions(playerMoney)
    score <- dealerActions()
  yield ()

  def playerActions(playerMoney: Int): IO[(Int, Int)] = ???

  def dealerActions(): IO[Int] = ???