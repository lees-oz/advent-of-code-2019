package day5

import cats.effect.IO

object IntCode5 {
  object MemParam {
    def tenPower(p: Int): Int = {
      if (p == 0) 1
      else 10 * tenPower(p - 1)
    }

    def apply(s: State, n: Int): IO[Int] = {
      val mode = s.modes % tenPower(n + 1) / tenPower(n)
      mode match {
        case 0 => ByRef(s, n)
        case 1 => ByVal(s, n)
        case m @ _ => IO.raiseError(new Exception(s"Unknown param mode $m"))
      }
    }
  }

  object ByVal {
    def apply(s: State, n: Int): IO[Int] = IO { s.current.tail(n) }
  }

  object ByRef {
    def apply(s: State, n: Int): IO[Int] = IO { s.code.dump(s.current.tail(n)) }
  }

  object Input {
    def apply(): IO[Int] = IO { scala.io.StdIn.readInt() }
  }

  final def run[I: Decoder: Executor](
    state: State
  ): IO[State] =
    for {
      instruction <- Decoder[I].decode(state)
      execResult <- Executor[I].execute(instruction)(state)
      result <- {
        if (execResult.halt) IO.pure(execResult.state)
        else run(execResult.state)
      }
    } yield result
}
