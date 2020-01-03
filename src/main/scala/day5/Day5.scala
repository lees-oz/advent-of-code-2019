package day5

import cats.effect.IO
import day5.IntCode5._

object Day5 extends App {

  sealed trait Instruction
  case object Halt extends Instruction
  case class Add(par1: Int, par2: Int, to: Int) extends Instruction
  case class Mul(par1: Int, par2: Int, to: Int) extends Instruction
  case class Input(what: Int, to: Int) extends Instruction
  case class Output(what: Int) extends Instruction
  case class JumpIfTrue(sub: Int, goto: Int) extends Instruction
  case class JumpIfFalse(sub: Int, goto: Int) extends Instruction
  case class LessThan(left: Int, right: Int, to: Int) extends Instruction
  case class Equals(left: Int, right: Int, to: Int) extends Instruction

  object implicits {

    implicit val decoder: Decoder[Instruction] = (s: State) => {
      s.code.opcode match {
        case 1 =>
          for {
            par1 <- MemParam(s.code, 0)
            par2 <- MemParam(s.code, 1)
            to <- ByVal(s.code, 2)
          } yield Add(par1, par2, to)
        case 2 =>
          for {
            par1 <- MemParam(s.code, 0)
            par2 <- MemParam(s.code, 1)
            to <- ByVal(s.code, 2)
          } yield Mul(par1, par2, to)
        case 3 =>
          for {
            what <- IO { s.input.head }
            to <- ByVal(s.code, 0)
          } yield Input(what, to)
        case 4 => ByRef(s.code, 0).map(Output.apply)
        case 5 =>
          for {
            sub <- MemParam(s.code, 0)
            goto <- MemParam(s.code, 1)
          } yield JumpIfTrue(sub, goto)
        case 6 =>
          for {
            sub <- MemParam(s.code, 0)
            goto <- MemParam(s.code, 1)
          } yield JumpIfFalse(sub, goto)
        case 7 =>
          for {
            left <- MemParam(s.code, 0)
            right <- MemParam(s.code, 1)
            to <- ByVal(s.code, 2)
          } yield LessThan(left, right, to)
        case 8 =>
          for {
            left <- MemParam(s.code, 0)
            right <- MemParam(s.code, 1)
            to <- ByVal(s.code, 2)
          } yield Equals(left, right, to)
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
            } yield Result(State(Code(dump, pointer), s.input, s.output))
          case Mul(par1, par2, to) =>
            for {
              dump <- IO(s.code.dump.updated(to, par1 * par2))
              pointer <- IO.pure(s.code.pointer + 4)
            } yield Result(State(Code(dump, pointer), s.input, s.output))
          case Input(what, to) =>
            for {
              dump <- IO(s.code.dump.updated(to, what))
              pointer <- IO.pure(s.code.pointer + 2)
            } yield Result(State(Code(dump, pointer), s.input.tail, s.output))
          case Output(what) =>
            for {
              pointer <- IO.pure(s.code.pointer + 2)
              data <- IO.pure(what :: s.output)
            } yield Result(State(Code(s.code.dump, pointer), s.input, data))
          case JumpIfTrue(sub, goto) =>
            for {
              pointer <- IO.pure { if (sub != 0) goto else s.code.pointer + 3 }
            } yield Result(State(Code(s.code.dump, pointer), s.input, s.output))
          case JumpIfFalse(sub, goto) =>
            for {
              pointer <- IO.pure { if (sub == 0) goto else s.code.pointer + 3 }
            } yield Result(State(Code(s.code.dump, pointer), s.input, s.output))
          case LessThan(left, right, to) =>
            for {
              pointer <- IO.pure(s.code.pointer + 4)
              what <- IO.pure { if (left < right) 1 else 0 }
              dump <- IO.pure { s.code.dump.updated(to, what) }
            } yield Result(State(Code(dump, pointer), s.input, s.output))
          case Equals(left, right, to) =>
            for {
              pointer <- IO.pure { s.code.pointer + 4 }
              what <- IO.pure { if (left == right) 1 else 0 }
            } yield
              Result(
                State(Code(s.code.dump.updated(to, what), pointer), s.input, s.output)
              )
          case Halt =>
            IO.pure(Result(State(s.code, s.input, s.output), true))
      }
  }
}
