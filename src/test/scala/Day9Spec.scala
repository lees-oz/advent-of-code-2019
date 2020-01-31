import day5.IntCode5
import day5.IntCode5.{Code, Memory, State}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import day5.Day5.implicits._


class Day9Spec extends AnyFlatSpec with Matchers {
//  "example 1" should "match" in {
//
//    val input = Memory(List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
//
//    val s = State(Code(input, 0), Nil)
//
//    import day5.Day5.implicits._
//
//    IntCode5.run(s).unsafeRunSync().state.output shouldBe List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
//  }

  "example 2" should "match" in {
    val input = Memory(List(1102,34915192,34915192,7,4,7,99,0).map(_.toLong))
    val s = State(Code(input, 0), Nil)
    IntCode5.run(s).unsafeRunSync().state.output shouldBe List(1219070632396864L)
  }

  "example 3" should "match" in {
    val input = Memory(List(104, 1125899906842624L, 99))
    val s = State(Code(input, 0), Nil)
    IntCode5.run(s).unsafeRunSync().state.output shouldBe List(1125899906842624L)
  }
}
