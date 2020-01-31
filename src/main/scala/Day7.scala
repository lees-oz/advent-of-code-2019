import cats.effect.IO
import cats.implicits._
import day5.IntCode5
import day5.IntCode5._
import day5.Day5.implicits._

object Day7 {
  private def runAmps(program: Memory): IO[Long] = {
    def runAmp(code: IntCode5.Code, phaseSettings: List[Int], prevOut: Long = 0): IO[Long] =
      if (phaseSettings.isEmpty) IO.pure(prevOut)
      else IntCode5.run(State(code, List(phaseSettings.head, prevOut), Nil)).flatMap {
        case Result(state, _) => runAmp(code, phaseSettings.tail, state.output.head)
      }

    (0 to 4).toList
      .permutations
      .toList
      .traverse(p => runAmp(Code(program, 0, 0), p))
      .map(_.max)
  }
  def part1(program: Memory): Long = runAmps(program).unsafeRunSync()

  def part2(program: Memory): Long = {
    def runAmps(code: IntCode5.Code, phaseSettings: List[Int], prevOut: Long = 0, amps: List[State] = Nil): IO[(Long, List[State])] = {
      if (phaseSettings.isEmpty) IO.pure((prevOut, amps))
      else {
        val amp = State(code, List(phaseSettings.head, prevOut), Nil)
        IntCode5.run(amp).flatMap {
          case Result(state, _) => runAmps(code, phaseSettings.tail, state.output.head, state :: amps)
        }
      }
    }

    def runAmpsLoop(amps: List[State], prevOut: Long): IO[Long] = {
      amps.foldLeft(IO.pure((List[State](), prevOut, true))) ((acc, amp) => {
        for {
          input <- acc.map(_._2)
          ampResult <- IntCode5.run(amp.copy(input = List(input)))
          amps <- acc.map(_._1)
          newAmps <- IO.pure(ampResult.state :: amps)
          output <- IO.pure(ampResult.state.output.head)
          halts <- acc.map(_._3)
        } yield (newAmps, output, halts && ampResult.status == Halted)
      }).flatMap {
        case (amps, out, false) => runAmpsLoop(amps.reverse, out)
        case (_, out, true) => IO.pure(out)
      }
    }

    (5 to 9).toList
      .permutations
      .toList
      .traverse(p => for {
        init <- runAmps(Code(program, 0, 0), p)
        runResult <- runAmpsLoop(init._2.reverse, init._1)
      } yield runResult)
      .map(_.max)
      .unsafeRunSync()

  }
}