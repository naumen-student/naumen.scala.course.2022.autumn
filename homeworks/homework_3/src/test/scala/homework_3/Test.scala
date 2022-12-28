package homework_3

object Test extends TestSuite{
    val trueStr = "правда"
    val falseStr = "ложь"
    val tests = Tests {
        'test_example - {
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
        }

        'prettyBooleanFormatter1_test - {
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter1(1) == "1")
            assert(Exercises.prettyBooleanFormatter1("a") == "a")
        }

        'prettyBooleanFormatter2_test - {
            assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter2(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter2(1) == "1")
            assert(Exercises.prettyBooleanFormatter2("a") == "a")
        }

        'prettyBooleanFormatter3_test - {
            assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter3(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter3(1) == "1")
            assert(Exercises.prettyBooleanFormatter3("a") == "a")
        }

        'max1_test - {
            assert(Exercises.max1(Seq()) == 0)
            assert(Exercises.max1(Seq(1)) == 1)
            assert(Exercises.max1(Seq(1, 2)) == 2)
        }

        'max2_test - {
            assert(Exercises.max2(Seq()) == Seq())
            assert(Exercises.max2(Seq(1)) == Seq(1))
            assert(Exercises.max2(Seq(1, 2)) == Seq(2))
        }

        'max3_test - {
            assert(Exercises.max3(Seq()).isEmpty)
            assert(Exercises.max3(Seq(1)).contains(1))
            assert(Exercises.max3(Seq(1, 2)).contains(2))
        }

        'sum1_test - {
            assert(Exercises.sum1(0, 0) == 0)
            assert(Exercises.sum1(0, 1) == 1)
            assert(Exercises.sum1(1, 1) == 2)
            assert(Exercises.sum1(-1, 1) == 0)
        }

        'sum2_test - {
            assert(Exercises.sum2(0, 0) == 0)
            assert(Exercises.sum2(0, 1) == 1)
            assert(Exercises.sum2(1, 1) == 2)
            assert(Exercises.sum2(-1, 1) == 0)
        }

        'sum3_test - {
            assert(Exercises.sum3(0, 0) == 0)
            assert(Exercises.sum3(0, 1) == 1)
            assert(Exercises.sum3(1, 1) == 2)
            assert(Exercises.sum3(-1, 1) == 0)
        }
    }
}
