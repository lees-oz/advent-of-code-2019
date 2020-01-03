import scala.annotation.tailrec
object Day6 extends App {
  @tailrec
  def run(code: List[String], acc: Map[String, String] = Map()): Map[String, String] = {
    if(code.isEmpty) acc
    else {
      val orbits = code.head.split("[)]")
      run(code.tail, acc.updated(orbits.tail.head, orbits.head))
    }
  }

  @tailrec
  def collect(orbit: String, tree: Map[String, String], acc: List[String] = Nil): List[String] = {
    if (!tree.contains(orbit)) acc
    else {
      val orbitedTo = tree(orbit)
      collect(orbitedTo, tree, orbitedTo :: acc)
    }
  }

  def part1(code: List[String]): Int = {
    val tree: Map[String, String] = run(code)
    tree.map {
      case (from, _) => collect(from, tree)
    }.map(_.size).sum
  }

  def part2(code: List[String]): Int = {
    val tree: Map[String, String] = run(code)
    val your = collect("YOU", tree)
    val santa = collect("SAN", tree)
    your.size + santa.size - your.intersect(santa).size * 2
  }
}
