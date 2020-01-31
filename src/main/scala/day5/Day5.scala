package day5

import cats.effect.IO
import day5.IntCode5._

object Day5 extends App {

  sealed trait Instruction
  case object Halt extends Instruction
  case class Add(par1: Long, par2: Long, to: Address) extends Instruction
  case class Mul(par1: Long, par2: Long, to: Address) extends Instruction
  case class Input(what: Option[Long], to: Address) extends Instruction
  case class Output(what: Long) extends Instruction
  case class JumpIfTrue(sub: Long, goto: Address) extends Instruction
  case class JumpIfFalse(sub: Long, goto: Address) extends Instruction
  case class LessThan(left: Long, right: Long, to: Address) extends Instruction
  case class Equals(left: Long, right: Long, to: Address) extends Instruction
  case class ShiftBase(shift: Long) extends Instruction

  object implicits {

    implicit val decoder: Decoder[Instruction] = (s: State) => {
      s.code.opcode match {
        case 1 =>
          for {
            par1 <- s.code.param(0)
            par2 <- s.code.param(1)
            to <- ByVal(s.code, 2)
          } yield Add(par1, par2, to)
        case 2 =>
          for {
            par1 <- s.code.param(0)
            par2 <- s.code.param(1)
            to <- ByVal(s.code, 2)
          } yield Mul(par1, par2, to)
        case 3 =>
          for {
            what <- IO { s.input.headOption }
            to <- ByVal(s.code, 0)
          } yield Input(what, to)
        case 4 =>
          for {
            what <- s.code.param(0)
          } yield Output(what)
        case 5 =>
          for {
            sub <- s.code.param(0)
            goto <- s.code.param(1)
          } yield JumpIfTrue(sub, goto)
        case 6 =>
          for {
            sub <- s.code.param(0)
            goto <- s.code.param(1)
          } yield JumpIfFalse(sub, goto)
        case 7 =>
          for {
            left <- s.code.param(0)
            right <- s.code.param(1)
            to <- ByVal(s.code, 2)
          } yield LessThan(left, right, to)
        case 8 =>
          for {
            left <- s.code.param(0)
            right <- s.code.param(1)
            to <- ByVal(s.code, 2)
          } yield Equals(left, right, to)
        case 9 =>
          for {
            shift <- s.code.param(0)
          } yield ShiftBase(shift)
        case 99    => IO.pure(Halt)
        case i @ _ => IO.raiseError(new Exception(s"Unknown instruction $i"))
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
            IO.pure(Result(State(s.code, s.input, s.output), Halted))
      }
  }
}
