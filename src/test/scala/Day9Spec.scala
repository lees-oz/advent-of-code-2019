import day5.IntCode5
import day5.IntCode5.{Code, State}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class Day9Spec extends AnyFlatSpec with Matchers {
  "example 3" should "match" in {

    val input = List(104,1125899906842624L,99).map(_.toLong)

    val s = State(Code(input, 0), Nil)

    import day5.Day5.implicits._

    IntCode5.run(s).unsafeRunSync().state.output shouldBe List(1125899906842624L)
  }
}
