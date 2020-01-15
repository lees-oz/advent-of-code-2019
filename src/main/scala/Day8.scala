import scala.annotation.tailrec
object Day8 extends App {
  def width = 25
  def height = 6

  def part1(image: String): Int = {
    val layers = image.map(_.asDigit).grouped(width * height).toList
    val zeroCounts = layers.map(_.count(_ == 0))
    val minZerosLayer = layers.zip(zeroCounts).minBy(_._2)._1

    minZerosLayer.count(_ == 1) * minZerosLayer.count(_ == 2)
  }

  def part2(image: String): String = {
    val layers = image.map(_.asDigit).grouped(width * height).toList

    @tailrec
    def findVisible(pixels: List[Int]): Int = {
      pixels match {
        case List(0, _*) => 0
        case List(1, _*) => 1
        case List(2, _*) => findVisible(pixels.tail)
        case Nil => 2
        case p => throw new Exception(s"Unknown pixel $p")
      }
    }

    @tailrec
    def render(pixels: List[Int], acc: String = ""): String = {
      if(pixels.isEmpty) acc
      else pixels.head match {
        case 0 => render(pixels.tail, acc + " ")
        case 1 => render(pixels.tail, acc + "0")
        case 2 => render(pixels.tail, acc + " ")
      }
    }

    layers.transpose.map(findVisible).grouped(width).toList
      .map(x => render(x))
      .mkString("\n")
  }
}
