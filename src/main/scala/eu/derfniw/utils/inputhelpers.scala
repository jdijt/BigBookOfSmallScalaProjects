package eu.derfniw.utils

import cats.*
import cats.effect.kernel.Sync
import cats.effect.std.Console
import cats.implicits.*

def readValidValue[F[_], A](prompt: String, checker: String => Either[A, String])(using
    C: Console[F],
    F: Sync[F]
): F[A] =
  def readLoop: F[A] = for
    _    <- C.print("> ")
    read <- C.readLine
    res <- checker(read) match
             case Left(v)    => F.pure(v)
             case Right(err) => C.println(err) >> readLoop
  yield res

  for
    _   <- C.println(prompt)
    res <- readLoop
  yield res
end readValidValue
