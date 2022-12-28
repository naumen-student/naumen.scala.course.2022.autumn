package homework_3

import utest._
import Exercises._

object Test extends TestSuite {

  val tests: Tests = Tests {
    'prettyBooleanFormatter - {
      val trueStr = "правда"
      val falseStr = "ложь"
      val twentyStr = "20"


      assert(prettyBooleanFormatter1(true) == trueStr)
      assert(prettyBooleanFormatter1(false) == falseStr)
      assert(prettyBooleanFormatter1(20) == twentyStr)

      assert(prettyBooleanFormatter2(true) == trueStr)
      assert(prettyBooleanFormatter2(false) == falseStr)
      assert(prettyBooleanFormatter2(20) == twentyStr)

      assert(prettyBooleanFormatter3(true) == trueStr)
      assert(prettyBooleanFormatter3(false) == falseStr)
      assert(prettyBooleanFormatter3(20) == twentyStr)
    }
    'max - {
      val nums = Seq[Int](1, 2, 4, 1)
      val numsMax = nums.max
      val empty = Seq[Int]()
      val oneEl = Seq[Int](1)


      assert(max1(nums) == numsMax)
      assert(max2(nums).head == numsMax)
      assert(max3(nums).head == numsMax)

      var thrown = intercept[Exception] {
        max1(empty)
      }
      assert(thrown.isInstanceOf[UnsupportedOperationException])
      assert(max2(empty).isEmpty)
      assert(max3(empty).isEmpty)

      assert(max1(oneEl) == oneEl.head)
      assert(max2(oneEl).head == oneEl.head)
      assert(max3(oneEl).head == oneEl.head)
    }
    'sum - {
      assert(sum1(1, 2) == 3)
      assert(sum1(2, 2) == 4)
      assert(sum1(9, 9) == 18)

      assert(sum2(1, 2) == 3)
      assert(sum2(2, 2) == 4)
      assert(sum2(9, 9) == 18)

      assert(sum3(1, 2) == 3)
      assert(sum3(2, 2) == 4)
      assert(sum3(9, 9) == 18)
    }
  }
}
