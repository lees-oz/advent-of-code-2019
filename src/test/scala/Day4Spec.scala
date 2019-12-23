import Day4._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFlatSpec with Matchers {
  val input = 273025 to 767253

  "my sample input part 1" should "be correct" in {
    part1(input) shouldBe 910
  }

  "my sample input part 2" should "be correct" in {
    part2(input) shouldBe 598
  }
}
