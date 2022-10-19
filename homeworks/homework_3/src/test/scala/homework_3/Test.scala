package homework_3

import utest._
import Exercises._

object Test extends TestSuite {
  val trueStr = "правда"
  val falseStr = "ложь"
  val oneStr = "1"

  val nums = Seq[Int](1, 2, 3)
  val numsMax = nums.max
  val empty = Seq[Int]()
  val oneEl = Seq[Int](1)

  val tests = Tests {
    'prettyBooleanFormatter1 - {
      assert(prettyBooleanFormatter1(1) == oneStr)
      assert(prettyBooleanFormatter1(true) == trueStr)
      assert(prettyBooleanFormatter1(false) == falseStr)
    }
    'prettyBooleanFormatter2 - {
      assert(prettyBooleanFormatter2(1) == oneStr)
      assert(prettyBooleanFormatter2(true) == trueStr)
      assert(prettyBooleanFormatter2(false) == falseStr)
    }
    'prettyBooleanFormatter3 - {
      assert(prettyBooleanFormatter3(1) == oneStr)
      assert(prettyBooleanFormatter3(true) == trueStr)
      assert(prettyBooleanFormatter3(false) == falseStr)
    }
    'max1 - {
      assert(max1(nums) == numsMax)
      var thrown = intercept[Exception] {
        max1(empty)
      }
      assert(thrown.isInstanceOf[UnsupportedOperationException])
      assert(max1(oneEl) == oneEl.head)
    }
    'max2 - {
      assert(max2(nums).head == numsMax)
      assert(max2(empty).isEmpty)
      assert(max2(oneEl).head == oneEl.head)
    }
    'max3 - {
      assert(max3(nums).head == numsMax)
      assert(max3(empty).isEmpty)
      assert(max3(oneEl).head == oneEl.head)
    }
    'sum1 - {
      assert(sum1(2, 2) == 4)
      assert(sum1(-3, -4) == -7)
      assert(sum1(-10, 13) == 3)
    }
    'sum2 - {
      assert(sum2(2, 2) == 4)
      assert(sum2(-3, -4) == -7)
      assert(sum2(-10, 13) == 3)
    }
    'sum3 - {
      assert(sum3(2, 2) == 4)
      assert(sum3(-3, -4) == -7)
      assert(sum3(-10, 13) == 3)
    }
  }
}
