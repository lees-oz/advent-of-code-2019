import scala.util.Try

object Day3 extends IntCode {
  sealed trait Instruction
  case class Up(distance: Int) extends Instruction
  case class Down(distance: Int) extends Instruction
  case class Left_(distance: Int) extends Instruction
  case class Right_(distance: Int) extends Instruction

  case class Edge(from: Point, to: Instruction)
  case class EdgeWithDistance(from: Point, to: Instruction, distance: Int)

  type Point = (Int, Int)
  object Point {
    def apply(from: Point, i: Instruction): Point = {
      (from, i) match {
        case ((x, y), Up(d))     => (x, y + d)
        case ((x, y), Down(d))   => (x, y - d)
        case ((x, y), Right_(d)) => (x + d, y)
        case ((x, y), Left_(d))  => (x - d, y)
      }
    }
  }

  implicit val instructionParser = new Decoder[Instruction] {
    override def decode(
      c: List[String]
    ): Either[Throwable, (Instruction, Int)] = {
      Try {
        val current = c.head
        (current.head, current.tail) match {
          case ('U', d) => (Up(d.toInt), 1)
          case ('D', d) => (Down(d.toInt), 1)
          case ('L', d) => (Left_(d.toInt), 1)
          case ('R', d) => (Right_(d.toInt), 1)
          case _        => throw new Exception("Invalid instruction")
        }
      }.toEither
    }
  }

  def isBetween(i: Int, range: (Int, Int)): Boolean = {
    List(range._1 to range._2, range._1 to range._2 by -1)
      .exists(_.contains(i))
  }

  def cross(edge1: Edge, edge2: Edge): Option[Point] = {
    def edgePoints(e: Edge): List[Point] = {
      (e.to, e.from) match {
        case (Up(d), (x, y))     => (y to y + d).map(yy => (x, yy)).toList
        case (Down(d), (x, y))   => (y - d to y).map(yy => (x, yy)).toList
        case (Right_(d), (x, y)) => (x to x + d).map(xx => (xx, y)).toList
        case (Left_(d), (x, y))  => (x - d to x).map(xx => (xx, y)).toList
      }
    }

    edgePoints(edge1).intersect(edgePoints(edge2)).headOption
  }

  // Part 1 - get crossing of 2 wires, closest to base
  def part1(wire1: String, wire2: String): Either[Throwable, Int] = {

    type Data = (List[Edge], Point)

    implicit val wiresExecutor = new IntExecutor[Instruction, Data] {
      override def execute(instruction: Instruction,
                           data: Data): Either[Throwable, Data] = {
        val nextPoint = Point(data._2, instruction)
        Right((Edge(data._2, instruction) :: data._1, nextPoint))
      }
    }

    def manhattanDistance(p: (Int, Int)): Int = Math.abs(p._1) + Math.abs(p._2)

    (for {
      e1 <- run(Code(wire1.split(",").toList, 0), (List[Edge](), (0, 0)))
      e2 <- run(Code(wire2.split(",").toList, 0), (List[Edge](), (0, 0)))
    } yield
      for {
        edge1 <- e1._1
        edge2 <- e2._1
        crossing <- cross(edge1, edge2)
      } yield manhattanDistance(crossing))
      .map(_.filter(_ > 0).min)
  }

//  def part2(wire1: String, wire2: String): Either[Throwable, (Int, Int)] = {
//
//    type Distance = Int
//    type Wire = (Instruction, Point)
//    type Data = List[EdgeWithDistance]
//
//    val wiresMachine = new IntCode[Instruction, Data] {
//      override def execute(instruction: Instruction,
//                           data: Data): Either[Throwable, Data] =
//        (
//          instruction,
//          data.headOption.map(_.).getOrElse((0, 0)),
//          data.headOption.map(_._3).getOrElse(0)
//        ) match {
//          case (Up(d), (x, y), path) =>
//            Right((Up(d), (x, y + d), path + d) :: data)
//          case (Down(d), (x, y), path) =>
//            Right((Down(d), (x, y - d), path + d) :: data)
//          case (Right_(d), (x, y), path) =>
//            Right((Right_(d), (x + d, y), path + d) :: data)
//          case (Left_(d), (x, y), path) =>
//            Right((Left_(d), (x - d, y), path + d) :: data)
//        }
//    }
//
//    def getDistance(from: Point, intersection: Point): Int =
//      if (from._1 == intersection._1) Math.abs(from._2 - intersection._2)
//      else if (from._2 == intersection._2) Math.abs(from._1 - intersection._1)
//      else throw new Exception("Can't measure distance")
//
//    def crossWires(v1: Wire, v2: Wire): Option[Point] = {
//      None
//    }
//
//    (for {
//      wires1 <- wiresMachine.run(Code(wire1.split(",").toList, 0), Nil)
//      wires2 <- wiresMachine.run(Code(wire2.split(",").toList, 0), Nil)
//    } yield
//      for {
//        wire1 <- wires1
//        wire2 <- wires2
//        crossing <- crossWires(wire1._1, wire2._1)
//      } yield
//        (wire1, wire2, crossing) match {
//          case ((Vertical()), edge2, crossing) =>
//            (
//              wire1._3 edge -getDistance(wire1._2, crossing),
//              getDistance(edge2._2, crossing)
//            )
//        }).map(_.head)
//
//  }
}
