package homework_3

import utest._

object Test extends TestSuite {

  val tests: Tests = Tests {
    val trueStr = "правда"
    val falseStr = "ложь"
    'test_formatter1 - {
      assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
      assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
      assert(Exercises.prettyBooleanFormatter1(1) == "1")
      assert(Exercises.prettyBooleanFormatter1(null) == "null")
      assert(Exercises.prettyBooleanFormatter1("string") == "string")
      assert(Exercises.prettyBooleanFormatter1(List(1, 2, 3)) == "List(1, 2, 3)")
    }
    'test_formatter2 - {
      assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
      assert(Exercises.prettyBooleanFormatter2(false) == falseStr)
      assert(Exercises.prettyBooleanFormatter2(1) == "1")
      assert(Exercises.prettyBooleanFormatter2(null) == "null")
      assert(Exercises.prettyBooleanFormatter2("string") == "string")
      assert(Exercises.prettyBooleanFormatter2(List(1, 2, 3)) == "List(1, 2, 3)")
    }
    'test_formatter3 - {
      assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
      assert(Exercises.prettyBooleanFormatter3(false) == falseStr)
      assert(Exercises.prettyBooleanFormatter3(1) == "1")
      assert(Exercises.prettyBooleanFormatter3(null) == "null")
      assert(Exercises.prettyBooleanFormatter3("string") == "string")
      assert(Exercises.prettyBooleanFormatter3(List(1, 2, 3)) == "List(1, 2, 3)")
    }
    'test_max1 - {
      assert(Exercises.max1(Seq(1, 2, 3)) == 3)
      assert(Exercises.max1(List(1, 2, 3)) == 3)
      assert(Exercises.max1(Seq(4, 2, 1)) == 4)
      assert(Exercises.max1(Seq(1, 2, -3)) == 2)
      assert(Exercises.max1(Seq(1, 2, 3, 2, 0)) == 3)
      assert(Exercises.max1(Seq(1, 2, Int.MaxValue)) == Int.MaxValue)
      assert(Exercises.max1(Seq()) == Int.MinValue)
    }
    'test_max2 - {
      assert(Exercises.max2(Seq(1, 2, 3)) == Seq(3))
      assert(Exercises.max2(List(1, 2, 3)) == Seq(3))
      assert(Exercises.max2(Seq(4, 2, 1)) == Seq(4))
      assert(Exercises.max2(Seq(1, 2, -3)) == Seq(2))
      assert(Exercises.max2(Seq(1, 2, 3, 2, 0)) == Seq(3))
      assert(Exercises.max2(Seq(1, 2, Int.MaxValue)) == Seq(Int.MaxValue))
      assert(Exercises.max2(Seq()) == Seq.empty)
    }
    'test_max3 - {
      assert(Exercises.max3(Seq(1, 2, 3)).getOrElse(Int.MinValue) == 3)
      assert(Exercises.max3(List(1, 2, 3)).getOrElse(Int.MinValue) == 3)
      assert(Exercises.max3(Seq(4, 2, 1)).getOrElse(Int.MinValue) == 4)
      assert(Exercises.max3(Seq(1, 2, -3)).getOrElse(Int.MinValue) == 2)
      assert(Exercises.max3(Seq(1, 2, 3, 2, 0)).getOrElse(Int.MinValue) == 3)
      assert(Exercises.max3(Seq(1, 2, Int.MaxValue)) == Option(Int.MaxValue))
      assert(Exercises.max3(Seq()).getOrElse(Int.MinValue) == Int.MinValue)
    }
    'test_sum1 - {
      assert(Exercises.sum1(1, 2) == 3)
      assert(Exercises.sum1(-1, 1) == 0)
      assert(Exercises.sum1(0, 3) == 3)
      assert(Exercises.sum1(0, 0) == 0)
    }
    'test_sum2 - {
      assert(Exercises.sum2(1, 2) == 3)
      assert(Exercises.sum2(-1, 1) == 0)
      assert(Exercises.sum2(0, 3) == 3)
      assert(Exercises.sum2(0, 0) == 0)
    }
    'test_sum3 - {
      assert(Exercises.sum3(1, 2) == 3)
      assert(Exercises.sum3(-1, 1) == 0)
      assert(Exercises.sum3(0, 3) == 3)
      assert(Exercises.sum3(0, 0) == 0)
    }
  }
}
