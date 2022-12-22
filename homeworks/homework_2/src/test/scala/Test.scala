import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(3, 9) == 23)
            assert(Exercises.sumOfDivBy3Or5(10, 15) == 37)
            assert(Exercises.sumOfDivBy3Or5(12, 12) == 12)
            assert(Exercises.sumOfDivBy3Or5(18, 18) == 18)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(3) == Seq(3))
            assert(Exercises.primeFactor(10) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
            assert(Exercises.primeFactor(1) == Seq())
            assert(Exercises.primeFactor(0) == Seq())
            assert(Exercises.primeFactor(712836) == Seq(2, 3, 19801))
            assert(Exercises.primeFactor(-712836) == Seq(2, 3, 19801))
        }
        'test_sumScalars - {
            assert(Exercises.sumScalars(Exercises.Vector2D(0, 0), Exercises.Vector2D(1, 1),
                Exercises.Vector2D(1, 1), Exercises.Vector2D(2, 2)) == 4)
            assert(Exercises.sumScalars(Exercises.Vector2D(1, 2), Exercises.Vector2D(3, 4),
                Exercises.Vector2D(5, 6), Exercises.Vector2D(7, 8)) == 94)
            assert(Exercises.sumScalars(Exercises.Vector2D(1, 0), Exercises.Vector2D(1, 1),
                Exercises.Vector2D(10, 20), Exercises.Vector2D(30, 40)) == 1101)
            assert(Exercises.sumScalars(Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0),
                Exercises.Vector2D(-1, -1), Exercises.Vector2D(-2, -2)) == 4)
            assert(Exercises.sumScalars(Exercises.Vector2D(0, 0), Exercises.Vector2D(0, 0),
                Exercises.Vector2D(1, 1), Exercises.Vector2D(-2, -2)) == -4)
        }
        'test_sumCosines - {
            assert(Exercises.sumCosines(Exercises.Vector2D(10, 10), Exercises.Vector2D(-10, -10),
                Exercises.Vector2D(10, 10), Exercises.Vector2D(20, 20)) < 1e-6)
            assert((Exercises.sumCosines(Exercises.Vector2D(1, 1), Exercises.Vector2D(-1, 1),
                Exercises.Vector2D(1, 1), Exercises.Vector2D(0, 1)) - 2.3561) < 1e-6)
            assert((Exercises.sumCosines(Exercises.Vector2D(1, 1), Exercises.Vector2D(-1, 1),
                Exercises.Vector2D(1, 0), Exercises.Vector2D(0, 1)) - math.Pi) < 1e-2)
        }
        'test_sortByHeavyweight - {
            assert(Exercises.sortByHeavyweight(Map()) == Seq())
            assert(Exercises.sortByHeavyweight(Map("123" -> (12, 0.5))) == Seq("123"))
            assert(Exercises.sortByHeavyweight(Map("Aluminum" -> (3,   2.6889),
                "Tungsten" ->  (2,   19.35), "Graphite" ->  (12,  2.1))) == Seq("Aluminum",  "Tungsten", "Graphite"))
            assert(Exercises.sortByHeavyweight(Map("Tin" -> (1,   7.29),
                "Platinum" -> (1,   21.45), "Plutonium" -> (3,   19.25))) == Seq("Tin", "Platinum", "Plutonium"))
            assert(Exercises.sortByHeavyweight(Map("Potassium" -> (14,  0.862), "Calcium" -> (8,   1.55),
                "Cobalt" -> (4,   8.90))) == Seq("Cobalt", "Calcium", "Potassium"))
        }
    }
}
