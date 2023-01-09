package eu.derfniw.project03Bitmap

import cats.effect.IO
import cats.effect.IOApp

object BitmapApp extends IOApp.Simple:

  val map = """|....................................................................
               |   **************   *  *** **  *      ******************************
               |  ********************* ** ** *  * ****************************** *
               | **      *****************       ******************************
               |          *************          **  * **** ** ************** *
               |           *********            *******   **************** * *
               |            ********           ***************************  *
               |   *        * **** ***         *************** ******  ** *
               |               ****  *         ***************   *** ***  *
               |                 ******         *************    **   **  *
               |                 ********        *************    *  ** ***
               |                   ********         ********          * *** ****
               |                   *********         ******  *        **** ** * **
               |                   *********         ****** * *           *** *   *
               |                     ******          ***** **             *****   *
               |                     *****            **** *            ********
               |                    *****             ****              *********
               |                    ****              **                 *******   *
               |                    ***                                       *    *
               |                    **     *                    *
               |....................................................................""".stripMargin

  def applyBitMap(message: String): String = map
    .split("\n")
    .map { line =>
      line.zipWithIndex.map {
        case (' ', _) => ' '
        case (_, idx) => message(idx % message.length())
      }.mkString
    }
    .mkString("\n")

  def run: IO[Unit] = for
    _   <- IO.println("Enter the message to display with the bitmap") >> IO.print("> ")
    msg <- IO.readLine
    _   <- IO.println(applyBitMap(msg))
  yield ()
end BitmapApp
