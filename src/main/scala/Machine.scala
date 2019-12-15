import scala.annotation.tailrec

case class Code[A](dump: List[A], pointer: Int)

// A machine that
// - runs through CodeMemory[C]
// - parses I from CodeMemory[C]
// - executes these instructions, manipulating with data memory D and code memory
// - in the end, resulting in either error or data D
trait Machine[C, I, D] {
  @tailrec
  final def run(code: Code[C], data: D)(implicit parser: Parse[I, C]): Either[Throwable, D] = {
    if(code.pointer >= code.dump.size) Right(data)
    else {
      parser.parse(code) match {
        case Right(i) => execute(i._1, data) match {
          case Right(s) => run(Code(code.dump, code.pointer + i._2), s)
          case Left(e) => Left(e)
        }
        case Left(e) => Left(e)
      }
    }
  }

  def execute(instruction: I, data: D): Either[Throwable, D]
}

trait Parse[I, M] {
  def parse(s: Code[M]): Either[Throwable, (I, Int)]
}