import scala.util.Try

object Day3 {

  import scala.annotation.tailrec

  case class Code(dump: List[String], pointer: Int)

  trait InstExecutor[I, D] {
    def execute(instruction: I, data: D): Either[Throwable, D]
  }

  trait Decoder[A] {
    def decode(s: List[String]): Either[Throwable, (A, Int)]
  }

  object Decoder {
    def apply[A](implicit d: Decoder[A]): Decoder[A] = implicitly[Decoder[A]]
  }

  /** A machine that
    * - sequentially runs through Code
    * - decodes instructions I
    * - executes these instructions, manipulating data D
    * - captures decoding and execution errors
    *
    * @returns either error or data D
   **/
  object IntCode {
    @tailrec
    final def run[I: Decoder, D](code: Code, data: D)(
      implicit executor: InstExecutor[I, D]
    ): Either[Throwable, D] = {
      if (code.pointer >= code.dump.size) Right(data)
      else {
        Decoder[I].decode(code.dump.drop(code.pointer)) match {
          case Right((instruction, size)) =>
            executor.execute(instruction, data) match {
              case Right(d) => run(Code(code.dump, code.pointer + size), d)
              case Left(e)  => Left(e)
            }
          case Left(e) => Left(e)
        }
      }
    }

  }

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

    type Data = (List[Point], Point)

    implicit val wiresExecutor = new InstExecutor[Instruction, Data] {
      override def execute(instruction: Instruction,
                           data: Data): Either[Throwable, Data] = {

        val (points, point) = data
        val newPoints = (point, instruction) match {
          case ((x, y), Up(d))     => (y to y + d).map(yy => (x, yy)).toList
          case ((x, y), Down(d))   => (y to y - d by -1).map(yy => (x, yy)).toList
          case ((x, y), Right_(d)) => (x to x + d).map(xx => (xx, y)).toList
          case ((x, y), Left_(d)) =>
            (x to x - d by -1).map(xx => (xx, y)).toList
        }

        Right(newPoints ++ points, newPoints.last)
      }
    }

    def manhattanDistance(p: Point): Int = p._1.abs + p._2.abs

    (for {
      points1 <- IntCode
        .run(Code(wire1.split(",").toList, 0), (List[Point](), (0, 0)))
        .map(_._1)

      points2 <- IntCode
        .run(Code(wire2.split(",").toList, 0), (List[Point](), (0, 0)))
        .map(_._1)
    } yield points1.intersect(points2))
      .map(_.map(manhattanDistance).filter(_ > 0).min)
  }

  // get minimal sum of distances to first crossing of both wires
  def part2(wire1: String, wire2: String): Either[Throwable, Int] = {
    type Distance = Int
    case class EdgeWithDistance(edge: Edge, distance: Distance)

    type Data = (List[EdgeWithDistance], Point, Distance)

    implicit val wiresMachine = new InstExecutor[Instruction, Data] {
      override def execute(instruction: Instruction,
                           data: Data): Either[Throwable, Data] = {
        val (edges, point, distance) = data
        val edge = EdgeWithDistance(Edge(point, instruction), distance)
        val nextPoint = Point(point, instruction)
        Right(edge :: edges, nextPoint, distance + instruction.distance)
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
    } yield
      for {
        edge1 <- edges1._1
        edge2 <- edges2._1
        crossing <- cross(edge1.edge, edge2.edge).filter(_ != init._2)
      } yield
        (
          getDistance(edge1.edge.from, crossing) + edge1.distance +
            getDistance(edge2.edge.from, crossing) + edge2.distance
        ))
      .map(_.min)
  }
}
