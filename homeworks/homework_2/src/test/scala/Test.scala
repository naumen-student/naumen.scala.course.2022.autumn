import Exercises._
import utest._

object Test extends TestSuite{

    val tests: Tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30,
                33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84,
                87, 90, 91, 93, 96, 98, 99))
        }

        'test_divBy3Or5 - {
            assert(sumOfDivBy3Or5(0, 2) == 0)
            assert(sumOfDivBy3Or5(15, 16) == 15)
            assert(sumOfDivBy3Or5(5, 6) == Seq(5, 6).sum)
            assert(sumOfDivBy3Or5(5, 16) == Seq(5, 6, 9, 10, 12, 15).sum)
        }

        'test_primeFactor - {
            assert(List(2, 4, 8, 16, 32, 1024).map(primeFactor).forall(_ == Seq(2)))
            assert(primeFactor(6) == Seq(2, 3))
            assert(primeFactor(210) == Seq(2, 3, 5, 7))
            assert(primeFactor(7) == Seq(7))
        }

        'test_sumScalars - {
            assert(sumScalars(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) == 4)
            assert(sumScalars(Vector2D(0, 1), Vector2D(1, 0), Vector2D(0, 0), Vector2D(1, 1)) == 0)
            assert(sumScalars(Vector2D(5, -2), Vector2D(2, 3), Vector2D(1, 1), Vector2D(2, 3)) == 9)
        }

        'test_sumCosines - {
            assert((sumCosines(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) - 2d).abs < 1e-6)
            assert((sumCosines(Vector2D(-1, -1), Vector2D(1, 1), Vector2D(-1, 0), Vector2D(1, 0)) - -2d).abs < 1e-6)
            assert((sumCosines(Vector2D(1, 1), Vector2D(1, -1), Vector2D(-1, 1), Vector2D(1, 1)) - 0d).abs < 1e-6)
            assert((sumCosines(Vector2D(10, 0), Vector2D(9, 9), Vector2D(1, -1), Vector2D(0, -1)) - math.sqrt(2)).abs < 1e-6)
        }

        'test_sortByHeavyweight - {
            assert(sortByHeavyweight(Map.empty) == Seq.empty)
            assert(sortByHeavyweight(Map("x" -> (1, 1))) == Seq("x"))
            assert(sortByHeavyweight(Map("x" -> (1, 1), "y" -> (2, 2))) == Seq("x", "y"))
            assert(sortByHeavyweight(Map("x" -> (2, 1), "y" -> (1, 2), "z" -> (1, 1))) == Seq("z", "y", "x"))
        }
    }
}
