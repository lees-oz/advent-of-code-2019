import scala.annotation.tailrec

trait Machine[S, I] {
  @tailrec
  final def run(program: String, state: S): Either[Throwable, S] = {
    if(program.isEmpty) Right(state)
    else {
      val parsedEither = parse(program)
      parsedEither match {
        case Right(i) => execute(i._1, state) match {
          case Right(s) => run(i._2, s)
          case Left(e) => Left(e)
        }
        case Left(e) => Left(e)
      }
    }
  }

  def parse(program: String): Either[Throwable, (I, String)]

  def execute(instruction: I, state: S): Either[Throwable, S]
}
