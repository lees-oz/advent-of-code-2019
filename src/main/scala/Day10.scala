import scala.annotation.tailrec
object Day10 extends App {
  type Point = (Int, Int)

  @tailrec
  final def getGcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else getGcd(b, a % b)

  object Space {
    def apply(map: String): Space = {
      val lines = map.split("\n").filter(_.nonEmpty)
      val asteroids: Set[Point] = lines.zipWithIndex.flatMap {
        case (s, j) => s.zipWithIndex collect {
          case (c, i) if c == '#' => (i, j)
        }
      }.toSet
      val size = (lines.head.size, lines.size)

      Space(asteroids, size)
    }
  }

  case class Space(asteroids: Set[Point], size: Point) {
    private def pointIsInside(point: Point, size: Point): Boolean = point._1 >= 0 && point._1 <= size._1 && point._2 >= 0 && point._2 <= size._2

    private def getShadowVector(pov: Point, at: Point): Point = {
      val vector = (at._1 - pov._1, at._2 - pov._2)
      val gcd = getGcd(vector._1, vector._2)
      (vector._1 / gcd, vector._2 / gcd)
    }

    private def getShadow(pov: Point, at: Point): Set[Point] = {

      @tailrec
      def traceShadow(from: Point, vector: (Int, Int), acc: Set[Point] = Set()): Set[Point] = {
        val candidate = (from._1 + vector._1, from._2 + vector._2)
        if(pointIsInside(candidate, size)) traceShadow(candidate, vector, acc + candidate)
        else acc
      }

      val shadowVector = getShadowVector(pov, at)
      traceShadow(at, shadowVector)
    }

    def getVisibleAsteroids(pov: Point): Set[Point] = {
      val otherAsteroids = asteroids - pov
      val shadows = otherAsteroids.flatMap(a => getShadow(pov, a))
      otherAsteroids.diff(shadows)
    }
  }

  def part1(map: String): Int = {
    val space = Space(map)
    space.asteroids.map(a => space.getVisibleAsteroids(a).size).max
  }

  def part2(map: String, pov: Point): Int = {
    var space = Space(map)
    type PointWithVector = (Point, Point)

//    val visible = space.getVisibleAsteroids(pov).map()
    ???
  }

  def renderSpace(base: Point, shadows: Set[Point], asteroids: Set[Point], size: Point): String = {
    (0 to size._2 - 1).flatMap(j => {
      '\n' :: (0 to size._1 - 1).map(i => {
        val p = (i, j)
        if (base == p) 'B'
        else if (shadows.contains(p) && asteroids.contains(p)) 'O'
        else if (shadows.contains(p)) 'S'
        else if (asteroids.contains(p)) '#'
        else '.'
      }
      ).toList
    }).mkString
  }
}