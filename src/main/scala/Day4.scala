import scala.annotation.tailrec

object Day4 {

  def part1(r: Range): Int = {
    def sameAdjacent(i: Int): Boolean = {

      @tailrec
      def iterate(s: String, last: Char): Boolean = {
        if (s.isEmpty) false
        else if (last == s.head) true
        else iterate(s.tail, s.head)
      }

      val s = i.toString
      if (s.isEmpty) false
      else iterate(s.tail, s.head)
    }

    def neverDecrease(i: Int): Boolean = i.toString.sorted == i.toString

    r.filter(sameAdjacent)
      .filter(neverDecrease)
      .size
  }
}
