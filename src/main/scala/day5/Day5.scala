package day5

import cats.effect.IO
import day5.IntCode5._

object Day5 extends App {

  sealed trait Instruction
  case object Halt extends Instruction
  case class Add(par1: Long, par2: Long, to: Long) extends Instruction
  case class Mul(par1: Long, par2: Long, to: Long) extends Instruction
  case class Input(what: Option[Long], to: Long) extends Instruction
  case class Output(what: Long) extends Instruction
  case class JumpIfTrue(sub: Long, goto: Long) extends Instruction
  case class JumpIfFalse(sub: Long, goto: Long) extends Instruction
  case class LessThan(left: Long, right: Long, to: Long) extends Instruction
  case class Equals(left: Long, right: Long, to: Long) extends Instruction
  case class ShiftBase(shift: Long) extends Instruction

  object implicits {

    implicit val decoder: Decoder[Instruction] = (s: State) => {
      s.code.opcode match {
        case 1 => IO(Add(s.code.getReadPar(0), s.code.getReadPar(1), s.code.getWritePar(2)))
        case 2 => IO(Mul(s.code.getReadPar(0), s.code.getReadPar(1), s.code.getWritePar(2)))
        case 3 => IO(Input(s.input.headOption, s.code.getWritePar(0)))
        case 4 => IO(Output(s.code.getReadPar(0)))
        case 5 => IO(JumpIfTrue(s.code.getReadPar(0), s.code.getReadPar(1)))
        case 6 => IO(JumpIfFalse(s.code.getReadPar(0), s.code.getReadPar(1)))
        case 7 => IO(LessThan(s.code.getReadPar(0), s.code.getReadPar(1), s.code.getWritePar(2)))
        case 8 => IO(Equals(s.code.getReadPar(0), s.code.getReadPar(1), s.code.getWritePar(2)))
        case 9 => IO(ShiftBase(s.code.getReadPar(0)))
        case 99  => IO.pure(Halt)
        case i@_ => IO.raiseError(new Exception(s"Unknown instruction $i"))
      }
    }

    implicit val executor: Executor[Instruction] =
      (instruction: Instruction, s: State) =>
        instruction match {
          case Add(par1, par2, to) =>
            for {
              dump <- IO(s.code.dump.updated(to, par1 + par2))
              pointer <- IO.pure(s.code.pointer + 4)
            } yield Result(State(Code(dump, pointer, s.code.base), s.input, s.output))
          case Mul(par1, par2, to) =>
            for {
              dump <- IO(s.code.dump.updated(to, par1 * par2))
              pointer <- IO.pure(s.code.pointer + 4)
            } yield Result(State(Code(dump, pointer, s.code.base), s.input, s.output))
          case Input(what, to) =>
            what.map(value => for {
              dump <- IO(s.code.dump.updated(to, value))
              pointer <- IO.pure(s.code.pointer + 2)
            } yield Result(State(Code(dump, pointer, s.code.base), s.input.tail, s.output)))
              .getOrElse(IO.pure(Result(s, AwaitInput)))
          case Output(what) =>
            for {
              pointer <- IO.pure(s.code.pointer + 2)
              data <- IO.pure(what :: s.output)
            } yield Result(State(Code(s.code.dump, pointer, s.code.base), s.input, data))
          case JumpIfTrue(sub, goto) =>
            for {
              pointer <- IO.pure { if (sub != 0) goto else s.code.pointer + 3 }
            } yield Result(State(Code(s.code.dump, pointer, s.code.base), s.input, s.output))
          case JumpIfFalse(sub, goto) =>
            for {
              pointer <- IO.pure { if (sub == 0) goto else s.code.pointer + 3 }
            } yield Result(State(Code(s.code.dump, pointer, s.code.base), s.input, s.output))
          case LessThan(left, right, to) =>
            for {
              what <- IO.pure { if (left < right) 1 else 0 }
              dump <- IO.pure { s.code.dump.updated(to, what.toLong) }
            } yield Result(State(Code(dump, s.code.pointer + 4, s.code.base), s.input, s.output))
          case Equals(left, right, to) =>
            for {
              pointer <- IO.pure { s.code.pointer + 4 }
              what <- IO.pure { if (left == right) 1 else 0 }
            } yield
              Result(
                State(Code(s.code.dump.updated(to, what.toLong), pointer, s.code.base), s.input, s.output)
              )
          case ShiftBase(shift) =>
            for {
              pointer <- IO.pure { s.code.pointer + 2 }
              base <- IO.pure { s.code.base + shift }
            } yield Result(State(Code(s.code.dump, pointer, base), s.input, s.output))
          case Halt =>
            IO.pure(Result(s, Halted))
      }
  }
}
