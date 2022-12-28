import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5' - {
            assert(Exercises.sumOfDivBy3Or5(3, 15) == 60)
            assert(Exercises.sumOfDivBy3Or5(2, 12) == 45)
            assert(Exercises.sumOfDivBy3Or5(5, 15) == 57)
        }

        'test_primeFactor' - {
            assert(Exersises.primeFactor(80) == Seq(2, 5))
            assert(Exersises.primeFactor(98) == Seq(2, 7))
            assert(Exersises.primeFactor(63) == Seq(3, 7))
        }

        'test_sumCosines - {
            assert(Exercises.sumCosines(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) === 2)
            assert(Exercises.sumCosines(Vector2D(9, 0), Vector2D(0, 9), Vector2D(9, 0), Vector2D(0, 9)) === 0)
            assert(Exercises.sumCosines(Vector2D(1, 0), Vector2D(1, 0), Vector2D(1, 0), Vector2D(1, 0)) === 2)
        }

        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) === 4)
            assert(Exercises.sumScalars(Vector2D(9, 0), Vector2D(0, 9), Vector2D(9, 0), Vector2D(0, 9)) === 0)
            assert(Exercises.sumScalars(Vector2D(5, 4), Vector2D(2, 4), Vector2D(2, 7), Vector2D(5, 7)) === 85)
        }

        'test_sortByHeavyweight' - {
            assert(Exersises.sortByHeavyweight(Exersises.balls) ==
              List(Tin, Platinum, Nickel, Aluminum, Titanium, Lead, Sodium, Uranium, Gold, Tungsten, Zirconium, Chrome, Iron, Copper, Silver, Plutonium, Cobalt, Cesium, Calcium, Lithium, Magnesium, Potassium, Graphite))
            assert(Exersises.sortByHeavyweight(Map.empty) == List.empty)
        }
    }
}
