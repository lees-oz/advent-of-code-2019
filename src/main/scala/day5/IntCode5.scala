package day5
import cats.effect.IO
object IntCode5 {
  type Memory = Map[Long, Long]

  case class State(code: Code, input: List[Long], output: List[Long] = Nil)

  object Memory {
    def apply(l: List[Long]): Memory = (0L to l.size - 1).zip(l).toMap
  }

  sealed trait Status
  case object Halted extends Status
  case object Running extends Status
  case object AwaitInput extends Status

  case class Result(state: State, status: Status = Running)

  trait Decoder[A] {
    def decode(s: State): IO[A]
  }

  trait Executor[I] {
    def execute(instruction: I, state: State): IO[Result]
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