package homework_3

import utest._

object Test extends TestSuite{
    def testPrettyBooleanFormatters(funcArg: Any, rightRes: String): Unit = {
        assert(Exercises.prettyBooleanFormatter1(funcArg) == rightRes)
        assert(Exercises.prettyBooleanFormatter2(funcArg) == rightRes)
        assert(Exercises.prettyBooleanFormatter3(funcArg) == rightRes)
    }
    def testSum(x: Int, y: Int, rightRes: Int): Unit = {
        assert(Exercises.sum1(x, y) == rightRes)
        assert(Exercises.sum2(x, y) == rightRes)
        assert(Exercises.sum3(x, y) == rightRes)
    }

    val tests = Tests{
        'test_prettyBooleanFormatters_true - {
            testPrettyBooleanFormatters(true, "правда")
        }
        'test_prettyBooleanFormatters_false - {
            testPrettyBooleanFormatters(false, "ложь")
        }
        'test_prettyBooleanFormatters_str - {
            testPrettyBooleanFormatters("false", "false")
        }
        'test_prettyBooleanFormatters_list - {
            testPrettyBooleanFormatters(List[Int](1, 2, 3), "List(1, 2, 3)")
        }
        'test_prettyBooleanFormatters_null - {
            testPrettyBooleanFormatters(null, "null")
        }
        'test_max1_numbers - {
            assert(Exercises.max1(Seq(1, 3, 1, 2, 100, 3, 4, 6)) == 100)
        }
        'test_max2_numbers - {
            assert(Exercises.max2(Seq(1, 3, 1, 2, 100, 3, 4, 6)) == Seq(100))
        }
        'test_max3_numbers - {
            assert(Exercises.max3(Seq(1, 3, 1, 2, 100, 3, 4, 6)) == Option(100))
        }
        'test_max1_empty_seq - {
            print(Exercises.max1(Seq()))
            assert(Exercises.max1(Seq()) == 0)
        }
        'test_max2_empty_seq - {
            assert(Exercises.max2(Seq()) == Seq.empty)
        }
        'test_max3_empty_seq - {
            assert(Exercises.max3(Seq()) == Option.empty)
        }
        'test_sums - {
            testSum(10, -3, 7)
        }
    }
}
