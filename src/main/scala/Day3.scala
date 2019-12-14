object Day3 {
  sealed trait Instruction
  case class Up(distance: Int) extends Instruction
  case class Down(distance: Int) extends Instruction
  case class Left(distance: Int) extends Instruction
  case class Right(distance: Int) extends Instruction

  sealed trait Edge
  case class Horizontal(y: Int, x: Range) extends Edge
  case class Vertical(x: Int, y: Range) extends Edge

  def cross(edge1: Edge, edge2: Edge): Option[(Int, Int)] = (edge1, edge2) match {
    case (Horizontal(hy, hx), Vertical(vx, vy)) if vy.contains(hy) && hx.contains(vx) => Some((vx, hy))
    case (Vertical(vx, vy), Horizontal(hy, hx)) if vy.contains(hy) && hx.contains(vx) => Some((vx, hy))
    case _ => None
  }

  def edges(instructions: List[Instruction]): List[Edge] =
    instructions.foldLeft((List[Edge](), (0, 0)))((acc, i) => (i, acc._2) match {
      case (Up(d), (x, y)) => (Vertical(x, y to y + d) :: acc._1, (x, y + d))
      case (Down(d), (x, y)) => (Vertical(x, y - d to y) :: acc._1, (x, y - d))
      case (Right(d), (x, y)) => (Horizontal(y, x to x + d) :: acc._1, (x + d, y))
      case (Left(d), (x, y)) => (Horizontal(y, x - d to x) :: acc._1, (x - d, y))
    })._1

  def parse(input: String): List[Instruction] = input
    .split(",")
    .map(i => (i.head, i.tail) match {
      case ('U', d) => Up(d.toInt)
      case ('D', d) => Down(d.toInt)
      case ('L', d) => Left(d.toInt)
      case ('R', d) => Right(d.toInt)
      case _ => throw new Exception("Invalid instruction")
    })
    .toList

  def manhattanDistance(p: (Int, Int)): Int = Math.abs(p._1) + Math.abs(p._2)

  def closest(wire1: String, wire2: String): Int =
    (for {
      e1 <- edges(parse(wire1))
      e2 <- edges(parse(wire2))
      crossing <- cross(e1, e2)
    } yield manhattanDistance(crossing))
      .filter(_ > 0)
      .min
}
