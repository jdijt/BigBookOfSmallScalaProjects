package eu.derfniw.project03Bitmap

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class BitmapTest extends ScalaCheckSuite {

  property("Result should only contain ' ', '\n' and characters from input string"){
    forAll{(in: String) =>
      in.nonEmpty ==> {
        val result = Bitmap.applyBitMap(in)
        result.forall(c => c == ' ' || c == '\n' || in.contains(c))
      }
    }
  }
}
