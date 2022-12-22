package homework_3

import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_task1 - {
            val trueStr = "правда"
            val falseStr = "ложь"
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter1(1) == "1")
            assert(Exercises.prettyBooleanFormatter1("qwerty") == "qwerty")
            assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter2(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter2(2) == "2")
            assert(Exercises.prettyBooleanFormatter2("abc") == "abc")
            assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter3(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter3(228) == "228")
            assert(Exercises.prettyBooleanFormatter3("qazxsw") == "qazxsw")
        }

        'test_task2 - {
            assert(Exercises.max1(Seq(1,2,3,4,5)) == 5)
            assert(Exercises.max1(Seq()) == Int.MinValue)
            assert(Exercises.max1(Seq(-444,-1,12,5)) == 12)
            assert(Exercises.max2(Seq(4, 2, 1, 17, -1)) == Seq(17))
            assert(Exercises.max2(Seq()) == Seq())
            assert(Exercises.max2(Seq(-444, -1, 12, 5)) == Seq(12))
            assert(Exercises.max3(Seq(-50, 15, 21, 23, 42, 11, -0, 19)).contains(42))
            assert(Exercises.max3(Seq(4, 2, 1, 17, -1,19,22)).contains(22))
            assert(Exercises.max3(Seq()).isEmpty)
            Exercises.sum2(5,10)

        }
        'test_task3 - {
            assert(Exercises.sum1(10,5) == 15)
            assert(Exercises.sum1(-90, 200) == 110)
            assert(Exercises.sum1(0, 0) == 0)
            assert(Exercises.sum2(10, 5) == 15)
            assert(Exercises.sum2(-90, 200) == 110)
            assert(Exercises.sum2(0, 0) == 0)
            assert(Exercises.sum3(10, 5) == 15)
            assert(Exercises.sum3(-90, 200) == 110)
            assert(Exercises.sum3(0, 0) == 0)
        }

    }
}
