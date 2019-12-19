import scala.util.Try

object Day3 {
  sealed trait Instruction {
    val distance: Int
  }

  case class Up(distance: Int) extends Instruction
  case class Down(distance: Int) extends Instruction
  case class Left_(distance: Int) extends Instruction
  case class Right_(distance: Int) extends Instruction

  case class Edge(from: Point, to: Instruction)

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
        val (edges, point) = data
        val edge = Edge(point, instruction)
        val nextPoint = Point(point, instruction)
        Right((edge :: edges, nextPoint))
      }
    }

    def manhattanDistance(p: (Int, Int)): Int = Math.abs(p._1) + Math.abs(p._2)

    (for {
      e1 <- IntCode.run(Code(wire1.split(",").toList, 0), (List[Edge](), (0, 0)))
      e2 <- IntCode.run(Code(wire2.split(",").toList, 0), (List[Edge](), (0, 0)))
    } yield
      for {
        edge1 <- e1._1
        edge2 <- e2._1
        crossing <- cross(edge1, edge2)
      } yield manhattanDistance(crossing))
      .map(_.filter(_ > 0).min)
  }

  // get minimal sum of distances to first crossing of both wires
  def part2(wire1: String, wire2: String): Either[Throwable, Int] = {
    type Distance = Int
    case class EdgeWithDistance(edge: Edge, distance: Distance)

    type Data = (List[EdgeWithDistance], Point, Distance)

    implicit val wiresMachine = new IntExecutor[Instruction, Data] {
      override def execute(instruction: Instruction,
                           data: Data): Either[Throwable, Data] = {
        val (edges, point, distance) = data
        val edge = EdgeWithDistance(Edge(point, instruction), distance)
        val nextPoint = Point(point, instruction)
        Right((edge :: edges, nextPoint, distance + instruction.distance))
      }
    }

    def getDistance(from: Point, intersection: Point): Int =
      if (from._1 == intersection._1) Math.abs(from._2 - intersection._2)
      else if (from._2 == intersection._2) Math.abs(from._1 - intersection._1)
      else throw new Exception("Can't measure distance")

    val init = (List[EdgeWithDistance](), (0, 0), 0)

    (for {
      edges1 <- IntCode.run(Code(wire1.split(",").toList, 0), init)
      edges2 <- IntCode.run(Code(wire2.split(",").toList, 0), init)
    } yield for {
      edge1 <- edges1._1
      edge2 <- edges2._1
      crossing <- cross(edge1.edge, edge2.edge).filter(_ != init._2)
    } yield (
      getDistance(edge1.edge.from, crossing) + edge1.distance +
      getDistance(edge2.edge.from, crossing) + edge2.distance
    ))
      .map(_.min)
  }
}
