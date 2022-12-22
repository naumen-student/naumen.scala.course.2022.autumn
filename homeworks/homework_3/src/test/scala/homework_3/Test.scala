package homework_3

import cats.Eq
import cats.implicits.catsSyntaxEq
import utest._

object Test extends TestSuite {

  def is_correct_on[A, B](f: A => B, input: A, result: B)(implicit eq: Eq[B]): Unit = assert(f(input) === result)

  override val tests: Tests =
    Tests {
    test("test true 1") {
      is_correct_on(Exercises.prettyBooleanFormatter1, true, "правда")
    }
    test("test false 1") {
      is_correct_on(Exercises.prettyBooleanFormatter1, false, "ложь")
    }
    test("test int 1") {
      is_correct_on(Exercises.prettyBooleanFormatter1, 2, "2")
    }
    test("test list 1") {
      is_correct_on(Exercises.prettyBooleanFormatter1, 1 :: 2 :: Nil, "List(1, 2)")
    }
  } ++
    Tests {
      test("test true 2") {
        is_correct_on(Exercises.prettyBooleanFormatter2, true, "правда")
      }
      test("test false 2") {
        is_correct_on(Exercises.prettyBooleanFormatter2, false, "ложь")
      }
      test("test int 2") {
        is_correct_on(Exercises.prettyBooleanFormatter2, 2, "2")
      }
      test("test list 2") {
        is_correct_on(Exercises.prettyBooleanFormatter2, 1 :: 2 :: Nil, "List(1, 2)")
      }
    } ++
    Tests {
      test("test true 3") {
        is_correct_on(Exercises.prettyBooleanFormatter3, true, "правда")
      }
      test("test false 3") {
        is_correct_on(Exercises.prettyBooleanFormatter3, false, "ложь")
      }
      test("test int 3") {
        is_correct_on(Exercises.prettyBooleanFormatter3, 2, "2")
      }
      test("test list 3") {
        is_correct_on(Exercises.prettyBooleanFormatter3, 1 :: 2 :: Nil, "List(1, 2)")
      }
    } ++
    Tests {
      test("regular list 1") {
        is_correct_on(Exercises.max1, 1 :: 2 :: 3 :: Nil, 3)
      }
      test("doubled max 1") {
        is_correct_on(Exercises.max1, 3 :: 2 :: 3 :: Nil, 3)
      }
      //      test("empty list") {
      //        is_correct_on(Exercises.max1, Nil, 3)  //В этой либе можно проверять, что вылетает ошибка нормально?
      //      }
    } ++
    Tests {
      implicit def seqEq[A](implicit eq: Eq[A]): Eq[Seq[A]] = _.toList === _.toList

      test("regular list 2") {
        is_correct_on(Exercises.max2, 1 :: 2 :: 3 :: Nil, Seq(3))
      }
      test("doubled max 2") {
        is_correct_on(Exercises.max2, 3 :: 2 :: 3 :: Nil, Seq(3, 3))
      }
      test("empty list 2") {
        is_correct_on(Exercises.max2, Nil, Seq.empty)
      }
    } ++
    Tests {
      test("regular list 3") {
        is_correct_on(Exercises.max3, 1 :: 2 :: 3 :: Nil, Some(3))
      }
      test("doubled max 3") {
        is_correct_on(Exercises.max3, 3 :: 2 :: 3 :: Nil, Some(3))
      }
      test("empty list 3") {
        is_correct_on(Exercises.max3, Nil, None)
      }
    } ++
    Tests {
      test("sum 1 1") {
        assert(Exercises.sum1(2, 3) === 5)
      }
      test("sum 1 2") {
        assert(Exercises.sum1(0, 0) === 0)
      }
      test("sum 1 3") {
        assert(Exercises.sum1(-1, 2) === 1)
      }
    } ++
    Tests {
      test("sum 2 1") {
        assert(Exercises.sum2(2, 3) === 5)
      }
      test("sum 2 2") {
        assert(Exercises.sum2(0, 0) === 0)
      }
      test("sum 2 3") {
        assert(Exercises.sum2(-1, 2) === 1)
      }
    } ++
    Tests {
      test("sum 3 1") {
        assert(Exercises.sum3(2, 3) === 5)
      }
      test("sum 3 2") {
        assert(Exercises.sum3(0, 0) === 0)
      }
      test("sum 3 3") {
        assert(Exercises.sum3(-1, 2) === 1)
      }
    }
}
