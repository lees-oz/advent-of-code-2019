import scala.util.Try

object Day2 extends App {

  case class State(memory: List[Int], pointer: Int)

  sealed trait Instruction

  case object Halt extends Instruction
  case class Add(par1: Int, par2: Int, to: Int) extends Instruction
  case class Mul(par1: Int, par2: Int, to: Int) extends Instruction

  object Instruction {
    def read(from: List[Int]): Either[Throwable, Instruction] =
      from match {
        case List(1, p1, p2, p3, _*) => Right(Add(p1, p2, p3))
        case List(2, p1, p2, p3, _*) => Right(Mul(p1, p2, p3))
        case List(99, _*) => Right(Halt)
        case _ => Left(new Exception("Invalid instruction"))
      }
  }

  def intcode(program: List[Int]): Either[Throwable, State] = {
    def run(state: State): Either[Throwable, State] =
      for {
        instruction <- Instruction.read(state.memory.drop(state.pointer))
        result <- instruction match {
          case Halt => Right(state)
          case Add(par1, par2, to) =>
            Try {
              State(
                state.memory.updated(to, state.memory(par1) + state.memory(par2)),
                state.pointer + 4)
            }.toEither.flatMap(run)
          case Mul(par1, par2, to) =>
            Try {
              State(
                state.memory.updated(to, state.memory(par1) * state.memory(par2)),
                state.pointer + 4)
            }.toEither.flatMap(run)
        }
      } yield result

    run(State(program, 0))
  }

  val input = List(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,9,19,23,2,13,23,27,1,6,27,31,2,6,31,35,2,13,35,39,1,39,10,43,2,43,13,47,1,9,47,51,1,51,13,55,1,55,13,59,2,59,13,63,1,63,6,67,2,6,67,71,1,5,71,75,2,6,75,79,1,5,79,83,2,83,6,87,1,5,87,91,1,6,91,95,2,95,6,99,1,5,99,103,1,6,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0)
  val result: Either[Throwable, Int] = intcode(input
    .patch(1, Seq(12), 1)
    .patch(2, Seq(2), 1)).map(_.memory.head)
  println(result)
}
