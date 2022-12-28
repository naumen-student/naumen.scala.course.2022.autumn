package homework_3

import utest._
import scala.util.Try

object Test extends TestSuite{
    val trueStr = "правда"
    val falseStr = "ложь"
    val str = "12345"

    val empty_seq = Seq.empty[Int]
    val not_empty_seq = Seq(1, 2, 3, 4, 5)

    val tests = Tests{
        'test_prettyBooleanFormatter1 - {
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter1(12345) == str)
        }

        'test_prettyBooleanFormatter12 - {
            assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter2(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter2(12345) == str)
        }

        'test_prettyBooleanFormatter3 - {
            assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter3(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter3(12345) == str)
        }

        'test_max1 - {
            assert(Try(Exercises.max1(empty_seq)).isFailure)
            assert(Exercises.max1(not_empty_seq) == 5)
        }

        'test_max2 - {
            assert(Exercises.max2(empty_seq) == empty_seq)
            assert(Exercises.max2(not_empty_seq) == Seq(5))
        }

        'test_max3 - {
            assert(Exercises.max3(empty_seq).isEmpty)
            assert(Exercises.max3(not_empty_seq).contains(5))
        }

        'test_sum1 - {
            assert(Exercises.sum1(1, 2) == 3)
            assert(Exercises.sum1(0, 10) == 10)
        }

        'test_sum2 - {
            assert(Exercises.sum2(1, 2) == 3)
            assert(Exercises.sum2(0, 10) == 10)
        }

        'test_sum3 - {
            assert(Exercises.sum3(1, 2) == 3)
            assert(Exercises.sum3(0, 10) == 10)
        }
    }
}
