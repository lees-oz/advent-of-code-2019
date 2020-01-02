package day5

import cats.effect.IO

trait Executor[I] {
  def execute(instruction: I)(state: State): IO[ExecutionResult]
}

object Executor {
  def apply[I](implicit executor: Executor[I]): Executor[I] = executor
}
