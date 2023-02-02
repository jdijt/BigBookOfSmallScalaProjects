package eu.derfniw.project04Blackjack

/** Typeclass for printing the cards. A bit weird because its not that generic (i.e.: only applies
  * to Cards and collections of those). But imo still cleaner than adding all that printing logic to
  * the classes itself, or making some: "displayCards(hand: List[Card])"
  */
trait ShowCard[A]:
  extension (a: A) def showCard(isOpen: Boolean): List[String]

/** Don't download left-pad from the internet kids ;) .
  */
extension (s: String)
  def leftPadTo(length: Int, filler: Char) =
    val numToPrepend = math.max(0, length - s.length())
    (filler.toString() * numToPrepend) + s

given ShowCard[Card] with
  extension (c: Card)
    def showCard(isOpen: Boolean): List[String] =
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

given [A: ShowCard]: ShowCard[List[A]] with
  extension (s: List[A])
    def showCard(isOpen: Boolean): List[String] =
      if s.isEmpty then Nil
      else
        // If not isOpen we'll only show the last element of the list open.
        val chunks = s.zipWithIndex.map((a, idx) => a.showCard(isOpen || idx == s.size - 1))
        // We take the length of the first element as the height of the output!
        val height = chunks.head.length
        // Potentially inefficient due to "get", but we'll only use it for very short lists so probably fine.
        (0 until height).map(i => chunks.map(_(i)).mkString(" ")).toList
end given
