package homework_3

import utest._
    
object Test extends TestSuite{

    val tests = Tests{
        'test_prettyBooleanFormatter1 - {
            assert(Exercises.prettyBooleanFormatter1(true) == "правда")
            assert(Exercises.prettyBooleanFormatter1(false) == "ложь")
            assert(Exercises.prettyBooleanFormatter1(45) == "45")
            assert(Exercises.prettyBooleanFormatter1(45.25) == "45.25")
        }

        'test_prettyBooleanFormatter2 - {
            assert(Exercises.prettyBooleanFormatter2(true) == "правда")
            assert(Exercises.prettyBooleanFormatter2(false) == "ложь")
            assert(Exercises.prettyBooleanFormatter2(45) == "45")
            assert(Exercises.prettyBooleanFormatter1(45.25) == "45.25")
        }

        'test_prettyBooleanFormatter3 - {
            assert(Exercises.prettyBooleanFormatter3(true) == "правда")
            assert(Exercises.prettyBooleanFormatter3(false) == "ложь")
            assert(Exercises.prettyBooleanFormatter3(45) == "45")
            assert(Exercises.prettyBooleanFormatter1(45.25) == "45.25")
        }

        'test_max1 - {
            assert(Exercises.max1(Seq(5, 10, 8)) == 10)
            assert(Exercises.max1(Seq(5)) == 5)
            assert(Exercises.max1(Seq(5, 10, -8)) == 10)
        }

        'test_max2 - {
            assert(Exercises.max2(Seq(5, 10, 8)) == Seq(10))
            assert(Exercises.max2(Seq(5)) == Seq(5))
            assert(Exercises.max2(Seq(5, 10, -8)) == Seq(10))
            assert(Exercises.max2(Seq()).isEmpty)
        }

        'test_max3 - {
            assert(Exercises.max3(Seq(5, 10, 8)) == Option(10))
            assert(Exercises.max3(Seq(5)) == Option(5))
            assert(Exercises.max3(Seq(5, 10, -8)) == Option(10))
            assert(Exercises.max3(Seq()).isEmpty)
        }

        'test_sum1 - {
            assert(Exercises.sum1(5, 3) == 8)
            assert(Exercises.sum1(15, 13) == 28)
            assert(Exercises.sum1(4, 7) == 11)
        }

        'test_sum2 - {
            assert(Exercises.sum2(5, 3) == 8)
            assert(Exercises.sum2(15, 13) == 28)
            assert(Exercises.sum2(4, 7) == 11)
        }

        'test_sum3 - {
            assert(Exercises.sum3(5, 3) == 8)
            assert(Exercises.sum3(15, 13) == 28)
            assert(Exercises.sum3(4, 7) == 11)
        }
    }
}
