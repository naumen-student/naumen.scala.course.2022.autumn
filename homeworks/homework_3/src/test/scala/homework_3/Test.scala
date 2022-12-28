package homework_3

import homework_3.Exercises._
import utest._

object Test extends TestSuite {

    val trueStr = "правда"
    val falseStr = "ложь"
    val numsStr = "123"

    val items: Seq[Int] = Seq[Int](1, 2, 3)
    val oneEl: Seq[Int] = Seq[Int](1)
    val empty: Seq[Int] = Seq[Int]()

    val tests: Tests = Tests {
        'prettyBooleanFormatter1 - {
            assert(prettyBooleanFormatter1(true) == trueStr)
            assert(prettyBooleanFormatter1(false) == falseStr)
            assert(prettyBooleanFormatter1(123) == numsStr)
        }
        'prettyBooleanFormatter2 - {
            assert(prettyBooleanFormatter2(true) == trueStr)
            assert(prettyBooleanFormatter2(false) == falseStr)
            assert(prettyBooleanFormatter2(123) == numsStr)
        }
        'prettyBooleanFormatter3 - {
            assert(prettyBooleanFormatter3(true) == trueStr)
            assert(prettyBooleanFormatter3(false) == falseStr)
            assert(prettyBooleanFormatter3(123) == numsStr)
        }
        'max1 - {
            assert(max1(items) == items.max)
            assert(max1(oneEl) == oneEl.head)
        }
        'max2 - {
            assert(max2(items).head == items.max)
            assert(max2(empty).isEmpty)
            assert(max2(oneEl).head == oneEl.head)
        }
        'max3 - {
            assert(max3(items).head == items.max)
            assert(max3(empty).isEmpty)
            assert(max3(oneEl).head == oneEl.head)
        }
        'sum1 - {
            assert(sum1(1, 2) == 3)
            assert(sum1(6, -4) == 2)
            assert(sum1(-10, 5) == -5)
        }
        'sum2 - {
            assert(sum2(1, 2) == 3)
            assert(sum2(6, -4) == 2)
            assert(sum2(-10, 5) == -5)
        }
        'sum3 - {
            assert(sum3(1, 2) == 3)
            assert(sum3(6, -4) == 2)
            assert(sum3(-10, 5) == -5)
        }
    }
}
