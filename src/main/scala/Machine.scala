import scala.annotation.tailrec

case class Code[A](dump: List[A], pointer: Int)

/* A machine that
 * - sequentially runs through Code[C]
 * - parses instructions I
 * - executes these instructions, manipulating with data memory D and code C
 * - handles parsing and execution errors
 * returns either error or data D
 */
trait Machine[C, I, D] {
  @tailrec
  final def run(code: Code[C],
                data: D)(implicit parser: Parse[I, C]): Either[Throwable, D] = {
    if (code.pointer >= code.dump.size) Right(data)
    else {
      parser.parse(code) match {
        case Right((instruction, size)) =>
          execute(instruction, data) match {
            case Right(s) => run(Code(code.dump, code.pointer + size), s)
            case Left(e)  => Left(e)
          }
        case Left(e) => Left(e)
      }
    }
  }

  def execute(instruction: I, data: D): Either[Throwable, D]
}

/* Tries to parse an instruction I from code of M
 * and on success returns it with its length in code
 * error otherwise
 */
trait Parse[I, M] {
  def parse(s: Code[M]): Either[Throwable, (I, Int)]
}
