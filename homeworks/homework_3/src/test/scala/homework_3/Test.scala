package homework_3

import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_prettyBooleanFormatter1 - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            val falseStr = "ложь"
            assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter1(123) == "123")
        }
        'test_prettyBooleanFormatter2 - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
            val falseStr = "ложь"
            assert(Exercises.prettyBooleanFormatter2(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter2("123") == "123")
        }
        'test_prettyBooleanFormatter3 - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
            val falseStr = "ложь"
            assert(Exercises.prettyBooleanFormatter3(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter3(123.5) == "123.5")
        }
        'test_max1 - {
            assert(Exercises.max1(Seq(1, 2, 3)) == 3)
            assert(Exercises.max1(Seq(0)) == 0)
            assert(Exercises.max1(Seq(1, 2, -3)) == 2)
        }
        'test_max2 - {
            assert(Exercises.max2(Seq(1, 2, 3)) == Seq(3))
            assert(Exercises.max2(Seq()).isEmpty)
            assert(Exercises.max2(Seq(1, 2, -3)) == Seq(2))
        }
        'test_max3 - {
            assert(Exercises.max3(Seq(1, 2, 3)) == Option(3))
            assert(Exercises.max3(Seq()).isEmpty)
            assert(Exercises.max3(Seq(1, 2, -3)) == Option(2))
        }
        'test_sum1 - {
            assert(Exercises.sum1(1, 2) == 3)
            assert(Exercises.sum1(-1, 1) == 0)
            assert(Exercises.sum1(0, 0) == 0)
        }
        'test_sum2 - {
            assert(Exercises.sum2(1, 2) == 3)
            assert(Exercises.sum2(-1, 1) == 0)
            assert(Exercises.sum2(0, 0) == 0)
        }
        'test_sum3 - {
            assert(Exercises.sum3(1, 2) == 3)
            assert(Exercises.sum3(-1, 1) == 0)
            assert(Exercises.sum3(0, 0) == 0)
        }
    }
}
