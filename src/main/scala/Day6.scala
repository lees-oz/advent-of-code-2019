import scala.annotation.tailrec
object Day6 extends App {
  val code = List("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L")

  @tailrec
  def run(code: List[String], acc: Map[String, String] = Map()): Map[String, String] = {
    if(code.isEmpty) acc
    else {
      val orbits = code.head.split("[)]")
      run(code.tail, acc.updated(orbits.tail.head, orbits.head))
    }
  }

  @tailrec
  def count(orbit: String, tree: Map[String, String], acc: Int = 0): Int = {
    if (!tree.contains(orbit)) acc
    else count(tree(orbit), tree, acc + 1)
  }

  def part1(code: List[String]): Int = {
    val tree: Map[String, String] = run(code)
    tree.map {
      case (from, _) => count(from, tree)
    }.sum
  }
}
