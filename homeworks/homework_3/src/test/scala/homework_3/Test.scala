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

    val tests = Tests {
        'test_prettyBooleanFormatters - {
            testPrettyBooleanFormatters(true, "правда")
            testPrettyBooleanFormatters(false, "ложь")
            testPrettyBooleanFormatters("false", "false")
            testPrettyBooleanFormatters(List[Int](1, 2, 3), "List(1, 2, 3)")
            testPrettyBooleanFormatters(null, "null")
        }
        'test_max_numbers - {
            assert(Exercises.max1(Seq(1, 3, 1, 2, 100, 3, 4, 6)) == 100)
            assert(Exercises.max2(Seq(1, 3, 1, 2, 100, 3, 4, 6)) == Seq(100))
            assert(Exercises.max3(Seq(1, 3, 1, 2, 100, 3, 4, 6)) == Option(100))
            assert(Exercises.max1(Seq(-1, -3, -1, -2, -100, -3, -4, -6)) == -1)
            assert(Exercises.max2(Seq(-1, -3, -1, -2, -100, -3, -4, -6)) == Seq(-1))
            assert(Exercises.max3(Seq(-1, -3, -1, -2, -100, -3, -4, -6)) == Option(-1))
        }
        'test_max_empty_seq - {
            assert(Exercises.max1(Seq()) == 0)
            assert(Exercises.max2(Seq()) == Seq.empty)
            assert(Exercises.max3(Seq()) == Option.empty)
        }
        'test_sums - {
            testSum(0, 0, 0)
            testSum(10, -3, 7)
            testSum(-6, -3, -9)
    }
}
