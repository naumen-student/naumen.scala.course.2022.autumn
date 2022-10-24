package homework_3

import homework_3.Exercises._
import utest._

object Test extends TestSuite{

    val testCases = List(
        Seq(1, 2, 3, 4, 5),
        Seq(-5, -10, 4),
        Seq(12, -12),
        Seq(),
        Seq(1, Int.MaxValue)
    )

    val tests = Tests{
        'test_prettyBooleanFormatter1 - {
            val trueStr = "правда"
            val falseStr = "ложь"
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter1(500) == "500")
            assert(Exercises.prettyBooleanFormatter1(1.12) == "1.12")
            assert(Exercises.prettyBooleanFormatter1(Seq()) == Seq().toString)
        }

        'test_prettyBooleanFormatter2 - {
            val trueStr = "правда"
            val falseStr = "ложь"
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter1(500) == "500")
            assert(Exercises.prettyBooleanFormatter1(1.12) == "1.12")
            assert(Exercises.prettyBooleanFormatter1(Seq()) == Seq().toString)
        }

        'test_prettyBooleanFormatter3 - {
            val trueStr = "правда"
            val falseStr = "ложь"
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter1(500) == "500")
            assert(Exercises.prettyBooleanFormatter1(1.12) == "1.12")
            assert(Exercises.prettyBooleanFormatter1(Seq()) == Seq().toString)
        }

        'test_max1 - {
            val result1 = List(5, 4, 12, Int.MinValue, Int.MaxValue)
            for (i <- 0 to 4) {
                assert(Exercises.max1(testCases(i)) == result1(i))
            }
        }

        'test_max2 - {
            val result2 = List(Seq(5), Seq(4), Seq(12), Seq(), Seq(Int.MaxValue))
            for (i <- 0 to 4) {
                assert(Exercises.max2(testCases(i)) == result2(i))
            }
        }

        'test_max3 - {
            val result3 = List(Some(5), Some(4), Some(12), None, Some(Int.MaxValue))
            for (i <- 0 to 4) {
                assert(Exercises.max3(testCases(i)) == result3(i))
            }
        }

        'test_sum1 - {
            val testCases2 = List(
                (0, 1),
                (-10, 12),
                (0, 0),
                (2, 3),
                (-500, 50)
            )
            val results = List(1, 2, 0, 5, -450)
            for (i <- 0 to 4) {
                assert(Exercises.sum1(testCases2(i)._1, testCases2(i)._2) == results(i))
            }
        }

        'test_sum2 - {
            val testCases2 = List(
                (0, 1),
                (-10, 12),
                (0, 0),
                (2, 3),
                (-500, 50)
            )
            val results = List(1, 2, 0, 5, -450)
            for (i <- 0 to 4) {
                assert(Exercises.sum2(testCases2(i)._1, testCases2(i)._2) == results(i))
            }
        }

        'test_sum3 - {
            val testCases2 = List(
                (0, 1),
                (-10, 12),
                (0, 0),
                (2, 3),
                (-500, 50)
            )
            val results = List(1, 2, 0, 5, -450)
            for (i <- 0 to 4) {
                assert(Exercises.sum3(testCases2(i)._1, testCases2(i)._2) == results(i))
            }
        }
    }
}
