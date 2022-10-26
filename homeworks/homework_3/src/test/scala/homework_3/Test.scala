package homework_3

import utest._

object Test extends TestSuite {

  val tests: Tests = Tests {
    'test_prettyBooleanFormatter1 - {
      assert(Exercises.prettyBooleanFormatter1(true) == "правда")
      assert(Exercises.prettyBooleanFormatter1(false) == "ложь")
      assert(Exercises.prettyBooleanFormatter1(10) == "10")
      assert(Exercises.prettyBooleanFormatter1("hi") == "hi")
    }

    'test_prettyBooleanFormatter2 - {
      assert(Exercises.prettyBooleanFormatter2(true) == "правда")
      assert(Exercises.prettyBooleanFormatter2(false) == "ложь")
      assert(Exercises.prettyBooleanFormatter2(10) == "10")
      assert(Exercises.prettyBooleanFormatter2("hi") == "hi")
    }

    'test_prettyBooleanFormatter3 - {
      assert(Exercises.prettyBooleanFormatter3(true) == "правда")
      assert(Exercises.prettyBooleanFormatter3(false) == "ложь")
      assert(Exercises.prettyBooleanFormatter3(10) == "10")
      assert(Exercises.prettyBooleanFormatter3("hi") == "hi")
    }

    'test_max1 - {
      assert(Exercises.max1(Seq(5, 0, 11)) == 11)
      assert(Exercises.max1(Seq()) == Int.MinValue)
      assert(Exercises.max1(Seq(6)) == 6)
    }

    'test_max2 - {
      assert(Exercises.max2(Seq(5, 0, 11)) == Seq(11))
      assert(Exercises.max2(Seq()).isEmpty)
      assert(Exercises.max2(Seq(6)) == Seq(6))
    }

    'test_max3 - {
      assert(Exercises.max3(Seq(5, 0, 11)) == Option(11))
      assert(Exercises.max3(Seq()).isEmpty)
      assert(Exercises.max3(Seq(6)) == Option(6))
    }

    'test_sum1 - {
      assert(Exercises.sum1(5, 4) == 9)
      assert(Exercises.sum1(100, 1000) == 1100)
      assert(Exercises.sum1(0, 0) == 0)
    }

    'test_sum2 - {
      assert(Exercises.sum2(5, 4) == 9)
      assert(Exercises.sum2(100, 1000) == 1100)
      assert(Exercises.sum2(0, 0) == 0)
    }

    'test_sum3 - {
      assert(Exercises.sum3(5, 4) == 9)
      assert(Exercises.sum3(100, 1000) == 1100)
      assert(Exercises.sum3(0, 0) == 0)
    }
  }
}
