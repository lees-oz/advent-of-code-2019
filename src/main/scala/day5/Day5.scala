package day5

import cats.effect.IO
import day5.IntCode5._

object Day5 extends App {

  sealed trait Instruction
  case object Halt extends Instruction
  case class Add(par1: Int, par2: Int, to: Int) extends Instruction
  case class Mul(par1: Int, par2: Int, to: Int) extends Instruction
  case class In(what: Int, to: Int) extends Instruction
  case class Out(what: Int) extends Instruction

  implicit val decoder: Decoder[Instruction] = (code: Code) => {
    code.opcode match {
      case 1 =>
        for {
          par1 <- MemParam(code, 0)
          par2 <- MemParam(code, 1)
          to <- ByVal(code, 2)
        } yield Add(par1, par2, to)
      case 2 =>
        for {
          par1 <- MemParam(code, 0)
          par2 <- MemParam(code, 1)
          to <- ByVal(code, 2)
        } yield Mul(par1, par2, to)
      case 3 =>
        for {
          what <- Input()
          to <- ByVal(code, 0)
        } yield In(what, to)
      case 4     => ByRef(code, 0).map(Out.apply)
      case 99    => IO.pure(Halt)
      case i @ _ => IO.raiseError(new Exception(s"Unknown instruction $i"))
    }
  }

  implicit val executor = new Executor[Instruction, ConsoleOutput] {
    override def execute(
      instruction: Instruction
    )(state: State[ConsoleOutput]): IO[ExecutionResult[ConsoleOutput]] = {
      instruction match {
        case Add(par1, par2, to) =>
          for {
            newCodeDump <- IO { state.code.dump.updated(to, par1 + par2) }
            newIp <- IO.pure(state.code.pointer + 4)
          } yield ExecutionResult(State(Code(newCodeDump, newIp), state.data))
        case Mul(par1, par2, to) =>
          for {
            newCodeDump <- IO { state.code.dump.updated(to, par1 * par2) }
            newIp <- IO.pure(state.code.pointer + 4)
          } yield ExecutionResult(State(Code(newCodeDump, newIp), state.data))
        case In(what, to) =>
          for {
            newCodeDump <- IO { state.code.dump.updated(to, what) }
            newIp <- IO.pure(state.code.pointer + 2)
          } yield ExecutionResult(State(Code(newCodeDump, newIp), state.data))
        case Out(what) =>
          for {
            _ <- IO { println(what) }
            newIp <- IO.pure(state.code.pointer + 2)
            newData <- IO.pure(state.data + what)
          } yield ExecutionResult(State(Code(state.code.dump, newIp), newData))
        case Halt =>
          IO.pure(ExecutionResult(State(state.code, state.data), true))
      }
    }
  }

  type ConsoleOutput = String

  def part1(input: List[Int]): State[ConsoleOutput] = {
    IntCode5
      .run(State(Code(input, 0), ""))
      .unsafeRunSync()
  }

  val input = List(3, 225, 1, 225, 6, 6, 1100, 1, 238, 225, 104, 0, 1102, 16,
    13, 225, 1001, 88, 68, 224, 101, -114, 224, 224, 4, 224, 1002, 223, 8, 223,
    1001, 224, 2, 224, 1, 223, 224, 223, 1101, 8, 76, 224, 101, -84, 224, 224,
    4, 224, 102, 8, 223, 223, 101, 1, 224, 224, 1, 224, 223, 223, 1101, 63, 58,
    225, 1102, 14, 56, 224, 101, -784, 224, 224, 4, 224, 102, 8, 223, 223, 101,
    4, 224, 224, 1, 223, 224, 223, 1101, 29, 46, 225, 102, 60, 187, 224, 101,
    -2340, 224, 224, 4, 224, 102, 8, 223, 223, 101, 3, 224, 224, 1, 224, 223,
    223, 1102, 60, 53, 225, 1101, 50, 52, 225, 2, 14, 218, 224, 101, -975, 224,
    224, 4, 224, 102, 8, 223, 223, 1001, 224, 3, 224, 1, 223, 224, 223, 1002,
    213, 79, 224, 101, -2291, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 2,
    224, 1, 223, 224, 223, 1, 114, 117, 224, 101, -103, 224, 224, 4, 224, 1002,
    223, 8, 223, 101, 4, 224, 224, 1, 224, 223, 223, 1101, 39, 47, 225, 101, 71,
    61, 224, 101, -134, 224, 224, 4, 224, 102, 8, 223, 223, 101, 2, 224, 224, 1,
    224, 223, 223, 1102, 29, 13, 225, 1102, 88, 75, 225, 4, 223, 99, 0, 0, 0,
    677, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1105, 0, 99999, 1105, 227, 247, 1105,
    1, 99999, 1005, 227, 99999, 1005, 0, 256, 1105, 1, 99999, 1106, 227, 99999,
    1106, 0, 265, 1105, 1, 99999, 1006, 0, 99999, 1006, 227, 274, 1105, 1,
    99999, 1105, 1, 280, 1105, 1, 99999, 1, 225, 225, 225, 1101, 294, 0, 0, 105,
    1, 0, 1105, 1, 99999, 1106, 0, 300, 1105, 1, 99999, 1, 225, 225, 225, 1101,
    314, 0, 0, 106, 0, 0, 1105, 1, 99999, 1107, 677, 677, 224, 102, 2, 223, 223,
    1006, 224, 329, 1001, 223, 1, 223, 108, 677, 677, 224, 1002, 223, 2, 223,
    1005, 224, 344, 101, 1, 223, 223, 1008, 226, 226, 224, 102, 2, 223, 223,
    1006, 224, 359, 1001, 223, 1, 223, 1107, 226, 677, 224, 102, 2, 223, 223,
    1006, 224, 374, 1001, 223, 1, 223, 8, 677, 226, 224, 102, 2, 223, 223, 1006,
    224, 389, 101, 1, 223, 223, 8, 226, 226, 224, 102, 2, 223, 223, 1006, 224,
    404, 101, 1, 223, 223, 7, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 419,
    101, 1, 223, 223, 7, 677, 226, 224, 1002, 223, 2, 223, 1005, 224, 434, 101,
    1, 223, 223, 1108, 677, 226, 224, 1002, 223, 2, 223, 1006, 224, 449, 1001,
    223, 1, 223, 108, 677, 226, 224, 1002, 223, 2, 223, 1006, 224, 464, 101, 1,
    223, 223, 1108, 226, 677, 224, 1002, 223, 2, 223, 1006, 224, 479, 101, 1,
    223, 223, 1007, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 494, 1001, 223,
    1, 223, 107, 226, 226, 224, 102, 2, 223, 223, 1005, 224, 509, 1001, 223, 1,
    223, 1008, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 524, 1001, 223, 1,
    223, 1007, 226, 226, 224, 102, 2, 223, 223, 1006, 224, 539, 101, 1, 223,
    223, 1108, 677, 677, 224, 102, 2, 223, 223, 1005, 224, 554, 1001, 223, 1,
    223, 1008, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 569, 101, 1, 223,
    223, 1107, 677, 226, 224, 1002, 223, 2, 223, 1006, 224, 584, 1001, 223, 1,
    223, 7, 226, 677, 224, 102, 2, 223, 223, 1005, 224, 599, 101, 1, 223, 223,
    108, 226, 226, 224, 1002, 223, 2, 223, 1005, 224, 614, 101, 1, 223, 223,
    107, 226, 677, 224, 1002, 223, 2, 223, 1005, 224, 629, 1001, 223, 1, 223,
    107, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 644, 101, 1, 223, 223,
    1007, 677, 226, 224, 1002, 223, 2, 223, 1006, 224, 659, 101, 1, 223, 223, 8,
    226, 677, 224, 102, 2, 223, 223, 1005, 224, 674, 1001, 223, 1, 223, 4, 223,
    99, 226)

  val res = IntCode5
    .run(State(Code(input, 0), ""))
    .unsafeRunSync()

  println(res)
}
