package day5
import day5.IntCode5.Memory

case class Code(dump: Memory, pointer: Long, base: Long) {
  def getReadPar(i: Int): Long = {
    mode(i) match {
      case 0 => dump.getOrElse(rawPar(i), 0)
      case 1 => rawPar(i)
      case 2 => dump(rawPar(i) + base)
      case m@_ => throw new Exception(s"Unknown read param mode $m")
    }
  }

  def getWritePar(i: Int): Long = {
    mode(i) match {
      case 0 => rawPar(i)
      case 2 => rawPar(i) + base
      case m@_ => throw new Exception(s"Unknown write param mode $m")
    }
  }

  def mode(n: Int): Int = modes % tenPower(n + 1) / tenPower(n)

  lazy val current: Long = dump(pointer)
  lazy val opcode: Int = (current % 100).toInt

  private def rawPar(i: Int): Long = dump(pointer + 1 + i)
  private def modes: Int = (current / 100).toInt
  private def tenPower(p: Int): Int = if (p == 0) 1 else 10 * tenPower(p - 1)
}
