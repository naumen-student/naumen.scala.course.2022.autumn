import Exercises.Vector2D
import utest._

object Test extends TestSuite {

  var tests: Tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    'sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
      assert(Exercises.sumOfDivBy3Or5(6, 12) == 37)
    }
    'primeFactor - {
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
    }
    'test_sumCosins - {
      assert(Exercises.sumCosines(Vector2D(10, 0), Vector2D(0, 10), Vector2D(10, 0), Vector2D(0, 10)) == 0)
      assert(2 - Exercises.sumCosines(Vector2D(5, 5), Vector2D(5, 5), Vector2D(5, 5), Vector2D(5, 5)) < 0.0000001)
      assert(2 - Exercises.sumCosines(Vector2D(3, 9), Vector2D(4, 12), Vector2D(4, 12), Vector2D(3, 9)) < 0.0000001)
    }
    'test_sumScalars - {
      assert(Exercises.sumScalars(Vector2D(3, 0), Vector2D(0, 555), Vector2D(14, 0), Vector2D(0, 7)) == 0)
      assert(Exercises.sumScalars(Vector2D(3, 3), Vector2D(3, 3), Vector2D(3, 3), Vector2D(2, 2)) == 30)
      assert(Exercises.sumScalars(Vector2D(5, 2), Vector2D(1, 4), Vector2D(2, 7), Vector2D(1, 1)) == 22)
    }
    'test_sortByHeavyweight - {
      assert(Exercises.sortByHeavyweight(Map.empty) == List.empty)
      assert(Exercises.sortByHeavyweight(Exercises.balls) == List("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium", "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium",  "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"))
    }
  }
}