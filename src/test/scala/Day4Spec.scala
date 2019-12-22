import Day4._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFlatSpec with Matchers {
  "my sample input part 1" should "be correct" in {

    val input = 273025 to 767253
    part1(input) shouldBe 910
  }
}
