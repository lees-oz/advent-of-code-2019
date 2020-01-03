package day5

import cats.effect.IO

object IntCode5 {
  type Memory = List[Int]
  type Address = Int

  case class Code(dump: Memory, pointer: Address) {
    private def tenPower(p: Int): Int =
      if (p == 0) 1
      else 10 * tenPower(p - 1)

    private def modes: Int = current.head / 100

    def mode(n: Int): Int = modes % tenPower(n + 1) / tenPower(n)

    val current: List[Int] = dump.drop(pointer)

    val opcode: Int = current.head % 100
  }

  case class State(code: Code, input: Memory, output: Memory = Nil)

  trait Decoder[A] {
    def decode(s: State): IO[A]
  }
  object Decoder {
    def apply[A](implicit d: Decoder[A]): Decoder[A] = implicitly[Decoder[A]]
  }

  case class Result(state: State, halt: Boolean = false)

  trait Executor[I] {
    def execute(instruction: I, state: State): IO[Result]
  }
  object Executor {
    def apply[I](implicit executor: Executor[I]): Executor[I] =
      executor
  }

  object MemParam {
    def apply(code: Code, n: Int): IO[Int] = code.mode(n) match {
      case 0     => ByRef(code, n)
      case 1     => ByVal(code, n)
      case m @ _ => IO.raiseError(new Exception(s"Unknown param mode $m"))
    }
  }

  object ByVal {
    def apply(code: Code, n: Int): IO[Int] = IO { code.current(n + 1) }
  }

  object ByRef {
    def apply(code: Code, n: Int): IO[Int] = IO { code.dump(code.current(n + 1)) }
  }

  final def run[I: Decoder](state: State)(implicit e: Executor[I]): IO[Memory] =
    for {
      instruction <- Decoder[I].decode(state)
      execResult <- Executor[I].execute(instruction, state)
      result <- {
        if (execResult.halt) IO.pure(execResult.state.output)
        else run(execResult.state)
      }
    } yield result
}
