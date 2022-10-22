package homework_3

import utest._

import scala.util.Try

object Test extends TestSuite {

  val seq: Seq[Int] = Range(0, 11)
  val pairSeq: Seq[Int] = Range(1, 3)
  val singleSeq: Seq[Int] = Seq(1)
  val emptySeq: Seq[Int] = Seq()

  private def assertPrettyBooleanFormatter(formatter: Any => String): Unit = {
    assert(formatter(true) == "правда")
    assert(formatter(false) == "ложь")
    assert(formatter(100) == 100.toString)
    assert(formatter("kek") == "kek")
  }

  private def assertSum(sum: (Int, Int) => Int): Unit = {
    assert(sum(1, 2) == 3)
    assert(sum(0, 0) == 0)
    assert(sum(1, -1) == 0)
    assert(sum(4, 10) == 14)
  }

  val tests = Tests {
    test("test_prettyBooleanFormatter1") {
      assertPrettyBooleanFormatter(Exercises.prettyBooleanFormatter1)
    }

    test("test_prettyBooleanFormatter2") {
      assertPrettyBooleanFormatter(Exercises.prettyBooleanFormatter2)
    }

    test("test_prettyBooleanFormatter3") {
      assertPrettyBooleanFormatter(Exercises.prettyBooleanFormatter3)
    }

    test("test_max1") {
      assert(Exercises.max1(seq) == 10)
      assert(Exercises.max1(pairSeq) == 2)
      assert(Exercises.max1(singleSeq) == 1)
      assert(Try(Exercises.max1(emptySeq)).isFailure)
    }

    test("test_max2") {
      assert(Exercises.max2(seq) == Seq(10))
      assert(Exercises.max2(pairSeq) == Seq(2))
      assert(Exercises.max2(singleSeq) == Seq(1))
      assert(Exercises.max2(emptySeq) == emptySeq)
    }

    test("test_max3") {
      assert(Exercises.max3(seq).contains(10))
      assert(Exercises.max3(pairSeq).contains(2))
      assert(Exercises.max3(singleSeq).contains(1))
      assert(Exercises.max3(emptySeq).isEmpty)
    }

    test("test_sum1") {
      assertSum(Exercises.sum1)
    }

    test("test_sum2") {
      assertSum(Exercises.sum2)
    }

    test("test_sum3") {
      assertSum(Exercises.sum3)
    }
  }
}
