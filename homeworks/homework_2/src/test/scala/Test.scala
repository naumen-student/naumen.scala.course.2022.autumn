import Exercises.Vector2D
import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 10) == 33)
            assert(Exercises.sumOfDivBy3Or5(5, 6) == 11)
            assert(Exercises.sumOfDivBy3Or5(3, 5) == 8)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(64) == Seq(2))
            assert(Exercises.primeFactor(59049) == Seq(3))
            assert(Exercises.primeFactor(100) == Seq(2, 5))
        }
        'test_sumCosins - {
            assert(Exercises.sumCosines(Vector2D(10, 0), Vector2D(0, 10), Vector2D(10, 0), Vector2D(0, 10)) == 0)
            assert(2 - Exercises.sumCosines(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) < 0.0000001)
            assert(2 - Exercises.sumCosines(Vector2D(2, 4), Vector2D(3, 6), Vector2D(2, 4), Vector2D(3, 6)) < 0.0000001)
        }
        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(10, 0), Vector2D(0, 10), Vector2D(10, 0), Vector2D(0, 10)) == 0)
            assert(Exercises.sumScalars(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) == 4)
            assert(Exercises.sumScalars(Vector2D(2, 4), Vector2D(3, 6), Vector2D(2, 4), Vector2D(3, 6)) == 60)
        }

        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(Map.empty) == Seq.empty)
            assert(Exercises.sortByHeavyweight(Exercises.balls) == Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium",
            "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium",
            "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite"))
            assert(Exercises.sortByHeavyweight(Map("ball" -> (0 , 0))) == Seq("ball"))
            assert(Exercises.sortByHeavyweight(Map("ball1" -> (1 , 1), "ball2" -> (1 , 2))) == Seq("ball1", "ball2"))
        }
    }
}
