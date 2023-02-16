package eu.derfniw.testutils

import eu.derfniw.project01Bagels.Strings.success

import cats.*
import cats.effect.*
import cats.effect.std.Console
import cats.implicits.*
import java.nio.charset.Charset
import junit.framework.Test

class TestConsole private (
    linesToRead: Ref[IO, Vector[String]],
    stdOutLines: Ref[IO, String],
    stdErrLines: Ref[IO, String]
) extends Console[IO]:

  override def error[A](a: A)(using s: Show[A]): IO[Unit] =
    stdErrLines.modify(old => (old + s.show(a), ()))

  override def errorln[A](a: A)(using s: Show[A]): IO[Unit] = error(s.show(a) + "\n")

  override def print[A](a: A)(using s: Show[A]): IO[Unit] =
    stdOutLines.modify(old => (old + s.show(a), ()))

  override def println[A](a: A)(using s: Show[A]): IO[Unit] = print(s.show(a) + "\n")

  override def readLineWithCharset(charset: Charset): IO[String] = for
    linesAccess <- linesToRead.access
    (lines, modifier) = linesAccess
    line <- lines match
              case l +: ls =>
                modifier(ls).flatMap(success =>
                  if success then l.pure[IO] else readLineWithCharset(charset)
                )
              case _ => IO.never[String]
  yield line

  def outLines: IO[Seq[String]] = stdOutLines.get.map(_.split("\n"))
  def errLines: IO[Seq[String]] = stdErrLines.get.map(_.split("\n"))
end TestConsole

object TestConsole:

  def make(linesToRead: String*): IO[Console[IO]] = for
    readRef   <- IO.ref(linesToRead.toVector)
    stdOutRef <- IO.ref("")
    stdErrRef <- IO.ref("")
  yield new TestConsole(readRef, stdOutRef, stdErrRef)
