import cats.effect.IO

case class Code(dump: List[Int], ip: Int)

case class State[D](code: Code, data: D)

case class ExecutionResult[D](state: State[D], halt: Boolean)

trait Executor[I, D] {
  def execute(instruction: I)(state: State[D]): IO[ExecutionResult[D]]
}

trait Decoder[A] {
  def decode(s: List[Int]): IO[A]
}
object Decoder {
  def apply[A](implicit d: Decoder[A]): Decoder[A] = implicitly[Decoder[A]]
}

object IntCode5 {
  final def run[I: Decoder, D](
    state: State[D]
  )(implicit executor: Executor[I, D]): IO[State[D]] =
    for {
      instruction <- Decoder[I].decode(state.code.dump.drop(state.code.ip))
      execResult <- executor.execute(instruction)(state)
      result <- {
        if (execResult.halt) IO.pure(execResult.state)
        else run(execResult.state)
      }
    } yield result
}
