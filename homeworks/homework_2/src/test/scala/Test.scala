import utest._
import Exercises.Vector2D

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(3, 5) == 8)
      assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
      assert(Exercises.sumOfDivBy3Or5(3, 3) == 3)
      assert(Exercises.sumOfDivBy3Or5(15, 25) == 123)
    }
    'test_primeFactor - {
      assert(Exercises.primeFactor(69) == Seq(3, 23))
      assert(Exercises.primeFactor(64) == Seq(2))
      assert(Exercises.primeFactor(36) == Seq(2, 3))
    }

    'test_sumByFunc - {
      assert(Exercises.sumScalars(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) == 4)
      assert("%.2f".format(Exercises.sumCosines(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1))) == "2,00")
      assert(Exercises.sumScalars(Vector2D(1, 0), Vector2D(1, 0), Vector2D(0, 1), Vector2D(0, 1)) == 2)
      assert("%.2f".format(Exercises.sumCosines(Vector2D(1, 0), Vector2D(0, 1), Vector2D(1, 0), Vector2D(0, 1))) == "0,00")
    }

    'test_sortByHeavyweight - {
      val resultSeq = Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium",
        "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium",
        "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite")
      assert(Exercises.sortByHeavyweight(Exercises.balls) == resultSeq)
    }
  }
}
