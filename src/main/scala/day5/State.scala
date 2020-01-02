package day5

case class State(code: Code) {
  def current: List[Int] = code.dump.drop(code.pointer)

  def opcode: Int = current.head % 100

  def modes: Int = current.head / 100
}
