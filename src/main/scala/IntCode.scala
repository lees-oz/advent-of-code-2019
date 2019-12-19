import scala.annotation.tailrec

case class Code(dump: List[String], pointer: Int)

trait Decoder[A] {
  def decode(s: List[String]): Either[Throwable, (A, Int)]
}

object Decoder {
  def apply[A](implicit d: Decoder[A]): Decoder[A] = implicitly[Decoder[A]]
}

/* A machine that
 * - sequentially runs through Code
 * - decodes instructions I
 * - executes these instructions, manipulating data D
 * - captures decoding and execution errors
 *
 * @returns either error or data D
 */
object IntCode {
  @tailrec
  final def run[I: Decoder, D](code: Code, data: D)(
    implicit executor: IntExecutor[I, D]
  ): Either[Throwable, D] = {
    if (code.pointer >= code.dump.size) Right(data)
    else {
      Decoder[I].decode(code.dump.drop(code.pointer)) match {
        case Right((instruction, size)) =>
          executor.execute(instruction, data) match {
            case Right(d) => run(Code(code.dump, code.pointer + size), d)
            case Left(e)  => Left(e)
          }
        case Left(e) => Left(e)
      }
    }
  }

}

trait IntExecutor[I, D] {
  def execute(instruction: I, data: D): Either[Throwable, D]
}
