package homework_3

import utest._

object Test extends TestSuite {

    val tests = Tests {
        'prettyBooleanFormatter1 - {
            val str = "boolean"
            val trueStr = "правда"
            val falseStr = "ложь"
            val value = 1
            assert(Exercises.prettyBooleanFormatter1(str) == str)
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter1(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter1(value) == value.toString)
        }
        'prettyBooleanFormatter2 - {
            val str = "boolean"
            val trueStr = "правда"
            val falseStr = "ложь"
            val value = 1
            assert(Exercises.prettyBooleanFormatter2(str) == str)
            assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter2(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter2(value) == value.toString)
        }
        'prettyBooleanFormatter3 - {
            val str = "boolean"
            val trueStr = "правда"
            val falseStr = "ложь"
            val value = 1
            assert(Exercises.prettyBooleanFormatter3(str) == str)
            assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter3(false) == falseStr)
            assert(Exercises.prettyBooleanFormatter3(value) == value.toString)
        }
        'max1 - {
            val positiveSeq = Seq(1, 3, 9,7, 6, 8)
            assert(Exercises.max1(positiveSeq) == 9)
            val negativeSeq = Seq(-10, -10, - 3, -3, -4, -7)
            assert(Exercises.max1(negativeSeq) == -3)
            val empty = intercept[Exception] {
                Exercises.max1(Seq[Int]())
            }
            assert(empty.isInstanceOf[UnsupportedOperationException])
        }
        'max2 - {
            val positiveSeq = Seq(1, 3, 9,7, 6, 8)
            assert(Exercises.max2(positiveSeq) == Seq(9))
            val negativeSeq = Seq(-10, -10, - 3, -3, -4, -7)
            assert(Exercises.max2(negativeSeq) == Seq(-3))
            val empty = Seq[Int]()
            assert(Exercises.max2(empty).isEmpty)
        }
        'max3 - {
            val positiveSeq = Seq(1, 3, 9,7, 6, 8)
            assert(Exercises.max3(positiveSeq).get == 9)
            val negativeSeq = Seq(-10, -10, - 3, -3, -4, -7)
            assert(Exercises.max3(negativeSeq).get == -3)
            val empty = Seq.empty
            assert(Exercises.max3(empty).isEmpty)
        }
        'sum1 - {
            assert(Exercises.sum1(6, 4) == 10)
            assert(Exercises.sum1(-6, -4) == -10)
            assert(Exercises.sum1(-6, 4) == -2)
        }
        'sum2 - {
            assert(Exercises.sum2(6, 4) == 10)
            assert(Exercises.sum2(-6, -4) == -10)
            assert(Exercises.sum2(-6, 4) == -2)
        }
        'sum3 - {
            assert(Exercises.sum3(6, 4) == 10)
            assert(Exercises.sum3(-6, -4) == -10)
            assert(Exercises.sum3(-6, 4) == -2)
        }
    }
}
