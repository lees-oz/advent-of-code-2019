import Day2._
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class Day2Spec extends AnyFlatSpec with Matchers {
  // Part 1

  "given example 1" should "match expected" in {
    intcode(List(1,0,0,0,99)).map(_.memory) shouldBe Right(List(2,0,0,0,99))
  }

  "given example 2" should "match expected" in {
    intcode(List(2,3,0,3,99)).map(_.memory) shouldBe Right(List(2,3,0,6,99))
  }

  "given example 3" should "match expected" in {
    intcode(List(2,4,4,5,99,0)).map(_.memory) shouldBe Right(List(2,4,4,5,99,9801))
  }

  "given example 4" should "match expected" in {
    intcode(List(1,1,1,4,99,5,6,0,99)).map(_.memory) shouldBe Right(List(30,1,1,4,2,5,6,0,99))
  }

  "my sample input part 1" should "be correct" in {
    val input = List(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,9,19,23,2,13,23,27,1,6,27,31,2,6,31,35,2,13,35,39,1,39,10,43,2,43,13,47,1,9,47,51,1,51,13,55,1,55,13,59,2,59,13,63,1,63,6,67,2,6,67,71,1,5,71,75,2,6,75,79,1,5,79,83,2,83,6,87,1,5,87,91,1,6,91,95,2,95,6,99,1,5,99,103,1,6,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0)

    intcode(input
      .patch(1, Seq(12), 1)
      .patch(2, Seq(2), 1)
    ).map(_.memory.headOption) shouldBe Right(Some(2890696))
  }

  // Part 2
  "my sample input part 2" should "be 8226" in {
    val input = List(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,9,19,23,2,13,23,27,1,6,27,31,2,6,31,35,2,13,35,39,1,39,10,43,2,43,13,47,1,9,47,51,1,51,13,55,1,55,13,59,2,59,13,63,1,63,6,67,2,6,67,71,1,5,71,75,2,6,75,79,1,5,79,83,2,83,6,87,1,5,87,91,1,6,91,95,2,95,6,99,1,5,99,103,1,6,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0)

    part2(input, 19690720) shouldBe Some(8226)
  }
}
