package day5

import cats.effect.IO
import day5.IntCode5.{ByRef, ByVal, Input, MemParam}

object Day5 extends App {

  sealed trait Instruction
  case object Halt extends Instruction
  case class Add(par1: Int, par2: Int, to: Int) extends Instruction
  case class Mul(par1: Int, par2: Int, to: Int) extends Instruction
  case class In(what: Int, to: Int) extends Instruction
  case class Out(what: Int) extends Instruction

  implicit val decoder: Decoder[Instruction] = (s: State) => {
    if (s.current.isEmpty)
      IO.raiseError(new Exception("Can't read instruction from empty memory"))
    else {
      s.opcode match {
        case 1 => for {
            par1 <- MemParam(s, 0)
            par2 <- MemParam(s, 1)
            to <- ByVal(s, 2)
          } yield Add(par1, par2, to)
        case 2 => for {
            par1 <- MemParam(s, 0)
            par2 <- MemParam(s, 1)
            to <- ByVal(s, 2)
          } yield Mul(par1, par2, to)
        case 3 => for {
            what <- Input()
            to <- ByVal(s, 0)
          } yield In(what, to)
        case 4 => ByRef(s, 0).map(Out.apply)
        case 99 => IO.pure(Halt)
        case i @ _  => IO.raiseError(new Exception(s"Unknown instruction $i"))
      }
    }
  }

  implicit val executor = new Executor[Instruction] {
    override def execute(
      instruction: Instruction
    )(state: State): IO[ExecutionResult] = {
      instruction match {
        case Add(par1, par2, to) =>
          for {
            newCodeDump <- IO { state.code.dump.updated(to, par1 + par2) }
            newIp <- IO.pure(state.code.pointer + 4)
          } yield ExecutionResult(State(Code(newCodeDump, newIp)))
        case Mul(par1, par2, to) =>
          for {
            newCodeDump <- IO { state.code.dump.updated(to, par1 * par2) }
            newIp <- IO.pure(state.code.pointer + 4)
          } yield ExecutionResult(State(Code(newCodeDump, newIp)))
        case In(what, to) =>
          for {
            newCodeDump <- IO { state.code.dump.updated(to, what) }
            newIp <- IO.pure(state.code.pointer + 2)
          } yield ExecutionResult(State(Code(newCodeDump, newIp)))
        case Out(what) =>
          for {
            _ <- IO { println(what) }
            newIp <- IO.pure(state.code.pointer + 2)
          } yield ExecutionResult(State(Code(state.code.dump, newIp)))
        case Halt => IO.pure(ExecutionResult(state, true))
      }
    }
  }


  val res = IntCode5
    .run(State(Code(List(1002, 4, 3, 4, 33), 0)))
    .unsafeRunSync()

  println(res)
}
