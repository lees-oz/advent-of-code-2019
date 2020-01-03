import cats.effect.IO
import day5.IntCode5
import day5.IntCode5.State
import day5.IntCode5.Code
import day5.Day5.implicits._

object Day7 extends App {

  def combinations(s: Set[Int], acc: List[Int] = Nil): Set[List[Int]] = {
    if (s.isEmpty) Set(acc)
    else s.flatMap(i => combinations(s - i, i :: acc))
  }

  def runAmps(code: IntCode5.Code, phaseSettings: List[Int], prevOut: Int = 0): IO[IntCode5.State] = {
//    for {
////      r <- IntCode5.run[List[Int]](State(code, 0), Nil, List(phaseSettings.head, prevOut)
//    } yield ???
    ???
  }

  def chain(input: List[String]): IO[Int] = {
    for {
      codes <- IO.pure((0 to 4).toSet)
      combination <- IO.pure(combinations(codes))
//      ampOutput <- for {
//
//      }
    } yield ??? //ampOutput
  }

  def part1(): Int = {
    ???
  }

}
