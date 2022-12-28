package homework_3

import homework_3.Exercises.{max1, max2, max3, prettyBooleanFormatter1, prettyBooleanFormatter2, prettyBooleanFormatter3, sum1, sum2, sum3}
import utest._

object Test extends TestSuite{

    val tests = Tests{
        'prettyBooleanFormatter1 - {
            assert(prettyBooleanFormatter1(1) == "1")
            assert(prettyBooleanFormatter1(true) == "правда")
            assert(prettyBooleanFormatter1(false) == "ложь")
        }
        'prettyBooleanFormatter2 - {
            assert(prettyBooleanFormatter2(1) == "1")
            assert(prettyBooleanFormatter2(true) == "правда")
            assert(prettyBooleanFormatter2(false) == "ложь")
        }
        'prettyBooleanFormatter3 - {
            assert(prettyBooleanFormatter3(1) == "1")
            assert(prettyBooleanFormatter3(true) == "правда")
            assert(prettyBooleanFormatter3(false) == "ложь")
        }
        'max1 - {
            assert(max1(Seq(1, 2, 3)) == 3)
            assert(max1(Seq(-3, -2, -1)) == -1)
            assert(max1(Seq(1)) == 1)
        }
        'max2 - {
            assert(max2(Seq(1, 2, 3)) == Seq(3))
            assert(max2(Seq(-3, -2, -1)) == Seq(-1))
            assert(max2(Seq()).isEmpty)
        }
        'max3 - {
            assert(max3(Seq(1, 2, 3)) == Option(3))
            assert(max3(Seq(-3, -2, -1)) == Option(-1))
            assert(max3(Seq()) == Option.empty)
        }
        'sum1 - {
            assert(sum1(1, 2) == 3)
            assert(sum1(0, 5) == 5)
            assert(sum1(-1, 1) == 0)
        }
        'sum2 - {
            assert(sum2(1, 2) == 3)
            assert(sum2(0, 5) == 5)
            assert(sum2(-1, 1) == 0)
        }
        'sum3 - {
            assert(sum3(1, 2) == 3)
            assert(sum3(0, 5) == 5)
            assert(sum3(-1, 1) == 0)
        }
    }
}
