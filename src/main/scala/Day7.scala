object Day7 extends App {
  (0 to 4)

  def combinations(s: Set[Int], acc: List[Int] = Nil): Set[List[Int]] = {
    if (s.isEmpty) Set(acc)
    else s.flatMap(i => combinations(s - i, i :: acc))
  }



}
