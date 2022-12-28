package homework_3

import utest._

object Test extends TestSuite {
  val tests = Tests {
    case class TestClass(first: Int, second: String)
    val TEST_CLASS        = TestClass(1, "sec")
    val TEST_CLASS_STRING = "TestClass(1,sec)"
    val TRUE_STRING       = "правда"
    val FALSE_STRING      = "ложь"

    'test_true_formatter_1 - {
      assert(Exercises.prettyBooleanFormatter1(true) == TRUE_STRING)
    }
    'test_false_formatter_1 - {
      assert(Exercises.prettyBooleanFormatter1(false) == FALSE_STRING)
    }
    'test_int_formatter_1 - {
      assert(Exercises.prettyBooleanFormatter1(424242) == "424242")
    }
    'test_custom_class_formatter_1 - {
      assert(Exercises.prettyBooleanFormatter1(TEST_CLASS) == TEST_CLASS_STRING)
    }

    'test_true_formatter_2 - {
      assert(Exercises.prettyBooleanFormatter2(true) == TRUE_STRING)
    }
    'test_false_formatter_2 - {
      assert(Exercises.prettyBooleanFormatter2(false) == FALSE_STRING)
    }
    'test_int_formatter_2 - {
      assert(Exercises.prettyBooleanFormatter2(424242) == "424242")
    }
    'test_custom_class_formatter_2 - {
      assert(Exercises.prettyBooleanFormatter2(TEST_CLASS) == TEST_CLASS_STRING)
    }

    'test_true_formatter_3 - {
      assert(Exercises.prettyBooleanFormatter3(true) == TRUE_STRING)
    }
    'test_false_formatter_3 - {
      assert(Exercises.prettyBooleanFormatter3(false) == FALSE_STRING)
    }
    'test_int_formatter_3 - {
      assert(Exercises.prettyBooleanFormatter3(424242) == "424242")
    }
    'test_custom_class_formatter_3 - {
      assert(Exercises.prettyBooleanFormatter3(TEST_CLASS) == TEST_CLASS_STRING)
    }

    val SEQ       = Seq(1, 42, 69, 300, 14, 50)
    val EMPTY_SEQ = Seq.empty[Int]
    'test_find_max1 - {
      assert(Exercises.max1(SEQ) == 300)
    }
    'test_find_max1_empty_seq - {
      assert(Exercises.max1(EMPTY_SEQ) == 0)
    }

    'test_find_max2 - {
      assert(Exercises.max2(SEQ) == Seq(300))
    }
    'test_find_max2_empty_seq - {
      assert(Exercises.max2(EMPTY_SEQ) == Seq.empty[Int])
    }

    'test_find_max3 - {
      assert(Exercises.max3(SEQ).contains(300))
    }
    'test_find_max3_empty_seq - {
      assert(Exercises.max3(EMPTY_SEQ).isEmpty)
    }

    val X = 40
    val Y = 2
    'test_sum1 - {
      assert(Exercises.sum1(X, Y) == 42)
    }
    'test_sum2 - {
      assert(Exercises.sum2(X, Y) == 42)
    }
    'test_sum3 - {
      assert(Exercises.sum3(X, Y) == 42)
    }
  }
}
