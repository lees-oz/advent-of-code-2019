package day5
import cats.effect.IO
object IntCode5 {
  type Memory = List[Long]
  type Address = Int

  sealed trait Status
  case object Halted extends Status
  case object Running extends Status
  case object AwaitInput extends Status

  case class Code(dump: Memory, pointer: Address) {
    def param(i: Int): IO[Long] = mode(i) match {
      case 0     => ByRef(this, i)
      case 1     => ByVal(this, i)
      case m @ _ => IO.raiseError(new Exception(s"Unknown param mode $m"))
    }

    private def tenPower(p: Int): Int =
      if (p == 0) 1
      else 10 * tenPower(p - 1)

    private def modes: Int = (current / 100).toInt

    def mode(n: Int): Int = modes % tenPower(n + 1) / tenPower(n)

    lazy val current: Long = dump(pointer.toInt)
    lazy val opcode: Int = (current % 100).toInt
  }

  case class State(code: Code, input: Memory, output: Memory = Nil)

  trait Decoder[A] {
    def decode(s: State): IO[A]
  }

  case class Result(state: State, status: Status = Running)

  trait Executor[I] {
    def execute(instruction: I, state: State): IO[Result]
  }

  object ByVal {
    def apply(code: Code, n: Int): IO[Long] = IO { code.dump(code.pointer + 1 + n) }
  }

  object ByRef {
    def apply(code: Code, n: Int): IO[Long] = ByVal(code, n).map(v => code.dump(v.toInt))
  }

  final def run[I: Decoder: Executor](state: State): IO[Result] =
    for {
      instruction <- implicitly[Decoder[I]].decode(state)
      execResult <- implicitly[Executor[I]].execute(instruction, state)
      result <- execResult.status match {
        case Halted | AwaitInput => IO.pure(execResult)
        case _ => run(execResult.state)
      }
    } yield result
}