package eu.derfniw.project04Blackjack

trait ShowLines[A]:
  extension (a: A) def showLines(isOpen: Boolean): List[String]

extension (s: String)
  def leftPadTo(length: Int, filler: Char) =
    val numToPrepend = math.max(0, length - s.length())
    (filler.toString() * numToPrepend) + s

given ShowLines[Card] with
  extension (c: Card)
    def showLines(isOpen: Boolean): List[String] =
      if isOpen then
        List(
          s" ___  ",
          s"|${c.rank.disp.padTo(2, ' ')} | ",
          s"| ${c.suit.icon} | ",
          s"|_${c.rank.disp.leftPadTo(2, '_')}| "
        )
      else
        List(
          s" ___  ",
          s"|## | ",
          s"|###| ",
          s"|_##| "
        )
end given

given [A: ShowLines]: ShowLines[List[A]] with
  extension (s: List[A])
    def showLines(isOpen: Boolean): List[String] =
      if s.isEmpty then Nil
      else
        // If not isOpen we'll only show the last element of the list open.
        val chunks = s.zipWithIndex.map((a, idx) => a.showLines(isOpen || idx == s.size - 1))
        // We take the length of the first element as the height of the output!
        val height = chunks.head.length
        // Potentially inefficient due to "get", but we'll only use it for very short lists so probably fine.
        (0 until height).map(i => chunks.map(_(i)).mkString(" ")).toList
end given
