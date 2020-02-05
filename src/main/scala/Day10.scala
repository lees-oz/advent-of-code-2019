import scala.annotation.tailrec
object Day10 extends App {
  type Point = (Int, Int)

  def getGcd(a: Int, b: Int): Int = {
    if(a == 0) b.abs
    else if(b == 0) a.abs
    else {
      val sorted = List(a.abs, b.abs).sorted

      @tailrec
      def iterate(min: Int, max: Int, attempt: Int): Int =
        if (attempt <= 1) 1
        else if(max % attempt == 0 && min % attempt == 0) attempt
        else iterate(min, max, attempt - 1)

      iterate(sorted(0), sorted(1), sorted(0))
    }
  }

  def part1(map: String): Int = {
    val lines = map.split("\n").filter(_.nonEmpty)

    val rectangle = (lines.head.size, lines.size)

    val asteroids: Set[Point] = lines.zipWithIndex.flatMap {
      case (s, j) => s.zipWithIndex collect {
        case (c, i) if c == '#' => (i, j)
      }
    }.toSet

    def getVisibleFrom(asteroids: Set[Point], base: Point): Int = {

      def getShadow(p: Point): Set[Point] = {
        val vector = (p._1 - base._1, p._2 - base._2)
        val gcd = getGcd(vector._1, vector._2)
        val shadowVector = (vector._1 / gcd, vector._2 / gcd)

        def pointIsInRectangle(point: Point, end: Point): Boolean = {
          point._1 >= 0 && point._1 <= end._1 && point._2 >= 0 && point._2 <= end._2
        }

        @tailrec
        def traceShadow(from: Point, vector: (Int, Int), acc: Set[Point] = Set()): Set[Point] = {
          val candidate = (from._1 + vector._1, from._2 + vector._2)
          if(pointIsInRectangle(candidate, rectangle)) traceShadow(candidate, vector, acc + candidate)
          else acc
        }

        traceShadow(p, shadowVector)
      }
      val shadows = asteroids.flatMap(getShadow)
      val visible = asteroids.diff(shadows)
      visible.size
    }

    asteroids.map(a => getVisibleFrom(asteroids - a, a)).max
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