import Exercises.Vector2D
import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(
        Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21,
          24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60,
          63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99)
      )
    }
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 3) == 0)
      assert(Exercises.sumOfDivBy3Or5(1, 15) == 15)
      assert(Exercises.sumOfDivBy3Or5(1, 30) == 45)
      assert(Exercises.sumOfDivBy3Or5(23, 30) == 30)
    }

    'test_sumByFunc - {
      assert(
        Exercises.sumByFunc(
          Vector2D(0, 1),
          Vector2D(1, 1),
          (v1, v2) => v1.x + v2.x,
          Vector2D(1, 1),
          Vector2D(2, 1)
        ) == 4
      )
    }

    'test_sortByHeavyweight - {
      assert(
        Exercises.sortByHeavyweight(
          Map(
            "Aluminum" -> (3, 0.01),
            "Tungsten" -> (3, 0.02),
            "Graphite" -> (12, 2.1)
          )
        ) ==
          List("Aluminum", "Tungsten", "Graphite")
      )
    }
  }
}
