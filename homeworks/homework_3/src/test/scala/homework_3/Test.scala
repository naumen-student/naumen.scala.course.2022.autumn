package homework_3


import homework_3.Exercises._
import utest._

object Test extends TestSuite{
    val nums: Seq[Int] = Seq[Int](1, 2, 3, 4,5)
    val oneEl: Seq[Int] = Seq[Int](1)

    val tests = Tests {
        'prettyBooleanFormatter1 - {
            assert(prettyBooleanFormatter1(123) == "123")
            assert(prettyBooleanFormatter1(true) == "правда")
            assert(prettyBooleanFormatter1(false) == "ложь")
        }
        'prettyBooleanFormatter2 - {
            assert(prettyBooleanFormatter2(123) == "123")
            assert(prettyBooleanFormatter2(true) == "правда")
            assert(prettyBooleanFormatter2(false) == "ложь")
        }
        'prettyBooleanFormatter3 - {
            assert(prettyBooleanFormatter3(123) == "123")
            assert(prettyBooleanFormatter3(true) == "правда")
            assert(prettyBooleanFormatter3(false) == "ложь")
        }
        'max1 - {
            assert(max1(nums) == nums.max)
            val thrown = intercept[Exception] {
                max1(Seq[Int]())
            }
            assert(thrown.isInstanceOf[UnsupportedOperationException])
            assert(max1(oneEl) == oneEl.head)
        }
        'max2 - {
            assert(max2(nums).head == nums.max)
            assert(max2(Seq[Int]()).isEmpty)
            assert(max2(oneEl).head == oneEl.head)
        }
        'max3 - {
            assert(max3(nums).head == nums.max)
            assert(max3(Seq[Int]()).isEmpty)
            assert(max3(oneEl).head == oneEl.head)
        }
        'sum1 - {
            assert(sum1(1000, 1) == 1001)
            assert(sum1(-3, 0) == -3)
            assert(sum1(-120, 120) == 0)
        }
        'sum2 - {
            assert(sum2(1000, 1) == 1001)
            assert(sum2(-3, 3) == 0)
            assert(sum2(-10, 13) == 3)
        }
        'sum3 - {
            assert(sum3(1000, 1) == 1001)
            assert(sum3(-3, -5) == -8)
            assert(sum3(-5, 13) == 8)
        }
    }
}
