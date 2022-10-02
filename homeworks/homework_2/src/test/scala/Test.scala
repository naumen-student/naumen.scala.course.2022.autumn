import Exercises.Vector2D
import utest._

object Test extends TestSuite {

    val tests = Tests {
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }

        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 2) == 0)
            assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
            assert(Exercises.sumOfDivBy3Or5(1, 4) == 3)
            assert(Exercises.sumOfDivBy3Or5(1, 6) == 14)
            assert(Exercises.sumOfDivBy3Or5(5, 5) == 5)
        }

        'test_primeFactor - {
            assert(Exercises.primeFactor(1) == Seq.empty[Int])
            assert(Exercises.primeFactor(2) == Seq(2))
            assert(Exercises.primeFactor(512) == Seq(2))
            assert(Exercises.primeFactor(7 * 7 * 7 * 5 * 2) == Seq(2, 5, 7))
        }

        'test_sumScalars - {
            assert(Exercises.sumScalars(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) == 4)
            assert(Exercises.sumScalars(Vector2D(1, 0), Vector2D(1, 0), Vector2D(0, 1), Vector2D(0, 1)) == 2)
        }

        'test_sumCosines - {
            assert(2 - Exercises.sumCosines(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) < 0.0001)
            assert(1 + Exercises.sumCosines(Vector2D(1, 0), Vector2D(0, 1), Vector2D(1, 1), Vector2D(-1, -1)) < 0.0001)
        }

        'test_sortByHeavyWeight - {
            assert(Exercises.sortByHeavyweight(Map.empty) == Seq.empty)
            assert(Exercises.sortByHeavyweight(Map("1" -> (1, 1))) == Seq("1"))
            assert(Exercises.sortByHeavyweight(Map("1" -> (1, 1), "2" -> (1, 2))) == Seq("1", "2"))
            assert(Exercises.sortByHeavyweight(Map("1" -> (9, 3), "2" -> (2, 1))) == Seq("2", "1"))
        }
    }
}
