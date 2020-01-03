import cats.effect.IO
import day5.IntCode5
import day5.IntCode5._
import day5.Day5.implicits._

object Day7 extends {
  private def runAmps(program: List[Int]): IO[Int] = {
    def runAmp(code: IntCode5.Code, phaseSettings: List[Int], prevOut: Int = 0): IO[Int] = {
      if (phaseSettings.isEmpty) IO.pure(prevOut)
      else IntCode5.run(State(code, List(phaseSettings.head, prevOut), Nil)).flatMap(out =>
        runAmp(code, phaseSettings.tail, out.head))
    }

    import cats.implicits._
    (0 to 4).toList
      .permutations
      .toList
      .traverse(p => runAmp(Code(program, 0), p))
      .map(_.max)
  }

  def part1(program: List[Int]): Int = runAmps(program).unsafeRunSync()
}
