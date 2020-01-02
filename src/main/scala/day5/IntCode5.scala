package day5

import cats.effect.IO

object IntCode5 {
  case class Code(dump: List[Int], pointer: Int) {
    private def tenPower(p: Int): Int =
      if (p == 0) 1
      else 10 * tenPower(p - 1)

    private def modes: Int = current.head / 100

    val current: List[Int] = dump.drop(pointer)

    val opcode: Int = current.head % 100

    def mode(n: Int): Int = modes % tenPower(n + 1) / tenPower(n)
  }

  case class State[D](code: Code, data: D)

  trait Decoder[A] {
    def decode(code: Code): IO[A]
  }
  object Decoder {
    def apply[A](implicit d: Decoder[A]): Decoder[A] = implicitly[Decoder[A]]
  }

  case class ExecutionResult[D](state: State[D], halt: Boolean = false)

  trait Executor[I, D] {
    def execute(instruction: I)(state: State[D]): IO[ExecutionResult[D]]
  }
  object Executor {
    def apply[I, D](implicit executor: Executor[I, D]): Executor[I, D] =
      executor
  }

  object MemParam {
    def apply(code: Code, n: Int): IO[Int] = {
      code.mode(n) match {
        case 0     => ByRef(code, n)
        case 1     => ByVal(code, n)
        case m @ _ => IO.raiseError(new Exception(s"Unknown param mode $m"))
      }
    }
  }

  object ByVal {
    def apply(code: Code, n: Int): IO[Int] = IO { code.current.tail(n) }
  }

  object ByRef {
    def apply(code: Code, n: Int): IO[Int] = IO {
      code.dump(code.current.tail(n))
    }
  }

  object Input {
    def apply(): IO[Int] = IO { scala.io.StdIn.readInt() }
  }

  final def run[I: Decoder, D](
    state: State[D]
  )(implicit e: Executor[I, D]): IO[State[D]] =
    for {
      instruction <- Decoder[I].decode(state.code)
      execResult <- Executor[I, D].execute(instruction)(state)
      result <- {
        if (execResult.halt) IO.pure(execResult.state)
        else run(execResult.state)
      }
    } yield result
}
