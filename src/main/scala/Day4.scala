object Day4 extends App {

  def neverDecrease(s: String): Boolean = s.sorted == s

  def part1(r: Range): Int = {

    def has2Adjacent(s: String): Boolean = s.groupBy(identity).values.exists(_.length >= 2)

    r
      .map(_.toString)
      .filter(neverDecrease)
      .filter(has2Adjacent)
      .size
  }

  def part2(r: Range): Int = {
    def neverDecrease(s: String): Boolean = s.sorted == s

    def hasExactly2Adjacent(s: String): Boolean = s.groupBy(identity).values.exists(_.length == 2)

    r
      .map(_.toString)
      .filter(neverDecrease)
      .filter(hasExactly2Adjacent)
      .size
  }
}
