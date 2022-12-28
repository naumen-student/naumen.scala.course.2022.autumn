package homework_3

import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_prettyBooleanFormatter - {
            val trueStr = "правда"
            val falseStr = "ложь"

            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter3(true) == trueStr)

            assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter2(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter3(false) == falseStr)

            assert(Exercises.prettyBooleanFormatter1("abc") == "abc")
            assert(Exercises.prettyBooleanFormatter2("abc") == "abc")
            assert(Exercises.prettyBooleanFormatter3("abc") == "abc")

            assert(Exercises.prettyBooleanFormatter1(123) == "123")
            assert(Exercises.prettyBooleanFormatter2(123) == "123")
            assert(Exercises.prettyBooleanFormatter3(123) == "123")
        }

        'test_max - {
            val emptySeq = Seq()
            val sampleSeq = Seq(-1, 0, 1)
            val maxSample = 1

            assert(Exercises.max1(emptySeq) == 0)
            assert(Exercises.max1(sampleSeq) == maxSample)

            assert(Exercises.max2(emptySeq) == emptySeq)
            assert(Exercises.max2(sampleSeq) == Seq(maxSample))

            assert(Exercises.max3(emptySeq).isEmpty)
            assert(Exercises.max3(sampleSeq).contains(maxSample))
        }

        'test_sum - {
            assert(Exercises.sum1(3, 2) == 5)
            assert(Exercises.sum2(3, 2) == 5)
            assert(Exercises.sum3(3, 2) == 5)
            assert(Exercises.sum1(0, 0) == 0)
            assert(Exercises.sum2(0, 0) == 0)
            assert(Exercises.sum3(0, 0) == 0)
            assert(Exercises.sum1(-100, 100) == 0)
            assert(Exercises.sum2(-100, 100) == 0)
            assert(Exercises.sum3(-100, 100) == 0)
        }
    }
}
