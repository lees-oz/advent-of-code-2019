import cats.data.NonEmptyList
import cats.effect.IO

object Day5 extends App {

  sealed trait Instruction
  case object Halt extends Instruction
  case class Add(par1: Param, par2: Param, to: Int) extends Instruction
  case class Mul(par1: Param, par2: Param, to: Int) extends Instruction

  sealed trait Param {
    val value: Int
  }
  case class ByVal(value: Int) extends Param
  case class ByRef(value: Int) extends Param
  object Param {
    def apply(value: Int, mode: Int): IO[Param] = {
      mode match {
        case 0 => IO.pure(ByVal(value))
        case 1 => IO.pure(ByRef(value))
        case _ => IO.raiseError(new Exception("Unknown parameter mode"))
      }
    }
  }

  def power(n: Int, p: Int): Int = {
    if (p == 0) 1
    else n * power(n, p - 1)
  }

  def eval[D](s: State[D], p: Param): IO[Int] = p match {
    case ByVal(v) => IO.pure(v)
    case ByRef(v) => IO { s.code.dump(s.code.ip) }
  }

  implicit val decoder: Decoder[Instruction] = (s: List[Int]) => {
    if (s.isEmpty)
      IO.raiseError(new Exception("Can't read instruction from empty memory"))
    else {
      val opcode = s.head % 100
      val paramModes = s.head / 100
      opcode match {
        case 1 => {
          for {
            params <- NonEmptyList
              .fromListUnsafe((1 to 2).toList)
              .traverse(i => Param(s(i), paramModes % power(10, i)))
          } yield Add(params.head, params.tail.head, s(3))
        }
        case 2 => {
          for {
            params <- NonEmptyList
              .fromListUnsafe((1 to 2).toList)
              .traverse(i => Param(s(i), paramModes % power(10, i)))
          } yield Mul(params.head, params.tail.head, s(3))
        }
        case 99 => IO.pure(Halt)
        case _  => IO.raiseError(new Exception("Unknown instruction"))
      }
    }
  }

  implicit val executor = new Executor[Instruction, Unit] {
    override def execute(
      instruction: Instruction
    )(state: State[Unit]): IO[ExecutionResult[Unit]] = {
      instruction match {
        case Add(par1, par2, to) => {
          for {
            p1 <- eval(state, par1)
            p2 <- eval(state, par2)
            newCodeDump <- IO { state.code.dump.updated(to, p1 + p2) }
            newIp <- IO.pure(state.code.ip + 4)
          } yield ExecutionResult(State(Code(newCodeDump, newIp), ()), false)
        }
        case Mul(par1, par2, to) => {
          for {
            p1 <- eval(state, par1)
            p2 <- eval(state, par2)
            newCodeDump <- IO { state.code.dump.updated(to, p1 * p2) }
            newIp <- IO.pure(state.code.ip + 4)
          } yield ExecutionResult(State(Code(newCodeDump, newIp), ()), false)
        }
        case Halt => IO.pure(ExecutionResult(state, true))
      }
    }
  }

  val res = IntCode5
    .run(State[Unit](Code(List(1002, 4, 3, 4, 33), 0), ()))
    .unsafeRunSync()

  println(res)
}
