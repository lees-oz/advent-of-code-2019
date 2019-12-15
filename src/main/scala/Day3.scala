import scala.util.Try

object Day3 {
  sealed trait Instruction
  case class Up(distance: Int) extends Instruction
  case class Down(distance: Int) extends Instruction
  case class Left_(distance: Int) extends Instruction
  case class Right_(distance: Int) extends Instruction

  sealed trait Edge
  case class Horizontal(y: Int, x: Range) extends Edge
  case class Vertical(x: Int, y: Range) extends Edge

  implicit val instructionParser = new Parse[Instruction, String] {
    override def parse(c: Code[String]): Either[Throwable, (Instruction, Int)] = {
      Try {
        val current = c.dump.drop(c.pointer).head
        (current.head, current.tail) match {
          case ('U', d) => (Up(d.toInt), 1)
          case ('D', d) => (Down(d.toInt), 1)
          case ('L', d) => (Left_(d.toInt), 1)
          case ('R', d) => (Right_(d.toInt), 1)
          case _ => throw new Exception("Invalid instruction")
        }
      }.toEither
    }
  }

  def cross(edge1: Edge, edge2: Edge): Option[(Int, Int)] = (edge1, edge2) match {
    case (Horizontal(hy, hx), Vertical(vx, vy)) if vy.contains(hy) && hx.contains(vx) => Some((vx, hy))
    case (Vertical(vx, vy), Horizontal(hy, hx)) if vy.contains(hy) && hx.contains(vx) => Some((vx, hy))
    case _ => None
  }

  // Part 1 - get crossing of 2 wires, closest to base
  def part1(wire1: String, wire2: String): Either[Throwable, Int] = {
    type Data = (List[Edge], (Int, Int))

    val wiresMachine = new Machine[String, Instruction, Data] {
      override def execute(instruction: Instruction, data: Data): Either[Throwable, Data] =
        (instruction, data._2) match {
          case (Up(d), (x, y)) => Right((Vertical(x, y to y + d) :: data._1, (x, y + d)))
          case (Down(d), (x, y)) => Right((Vertical(x, y - d to y) :: data._1, (x, y - d)))
          case (Right_(d), (x, y)) => Right((Horizontal(y, x to x + d) :: data._1, (x + d, y)))
          case (Left_(d), (x, y)) => Right((Horizontal(y, x - d to x) :: data._1, (x - d, y)))
        }
    }

    def manhattanDistance(p: (Int, Int)): Int = Math.abs(p._1) + Math.abs(p._2)

    (for {
      e1 <- wiresMachine.run(Code(wire1.split(",").toList, 0), (Nil, (0, 0)))
      e2 <- wiresMachine.run(Code(wire2.split(",").toList, 0), (Nil, (0, 0)))
    } yield for {
      edge1 <- e1._1
      edge2 <- e2._1
      crossing <- cross(edge1, edge2)
    } yield manhattanDistance(crossing))
      .map(_.filter(_ > 0).min)
  }
}
