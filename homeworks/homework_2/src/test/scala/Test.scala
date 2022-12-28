import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(Exercises.sumOfDivBy3Or5(1, 2) == 0L)
            assert(Exercises.sumOfDivBy3Or5(1, 5) == 8L)
            assert(Exercises.sumOfDivBy3Or5(0, 18) == 78L)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(5) == Seq(5))
            assert(Exercises.primeFactor(121) == Seq(11))
            assert(Exercises.primeFactor(124) == Seq(2, 31))
            assert(Exercises.primeFactor(1245) == Seq(3, 5, 83))
        }
        'test_sumCosines - {
            assert("%.3f".format(Exercises.sumCosines(Exercises.Vector2D(1, 5), Exercises.Vector2D(2, 2),
                Exercises.Vector2D(0, 1), Exercises.Vector2D(1, 0))).toDouble == 0.832)
            assert("%.3f".format(Exercises.sumCosines(Exercises.Vector2D(1, 4), Exercises.Vector2D(3, 2),
                Exercises.Vector2D(0, 8), Exercises.Vector2D(8, 2))).toDouble == 0.982)
            assert("%.3f".format(Exercises.sumCosines(Exercises.Vector2D(4, 0), Exercises.Vector2D(0, 2),
                Exercises.Vector2D(0, 13), Exercises.Vector2D(8, 0))).toDouble == 0)
            assert(Exercises.sumCosines(Exercises.Vector2D(3, 14), Exercises.Vector2D(3, 1),
                Exercises.Vector2D(9, 2), Exercises.Vector2D(4, 0)) == 1.484172080128247)

        }
        'test_sumScalars - {
            assert(Exercises.sumScalars(Exercises.Vector2D(1, 5), Exercises.Vector2D(2, 2),
                Exercises.Vector2D(0, 1), Exercises.Vector2D(1, 0)) == 12)
            assert(Exercises.sumScalars(Exercises.Vector2D(1, 4), Exercises.Vector2D(3, 2),
                Exercises.Vector2D(0, 8), Exercises.Vector2D(8, 2)) == 27)
            assert(Exercises.sumScalars(Exercises.Vector2D(4, 0), Exercises.Vector2D(0, 2),
                Exercises.Vector2D(0, 13), Exercises.Vector2D(8, 0)) == 0)
            assert(Exercises.sumScalars(Exercises.Vector2D(3, 14), Exercises.Vector2D(3, 1),
                Exercises.Vector2D(9, 2), Exercises.Vector2D(4, 0)) == 59)

        }
        'test_sortByHeavyWeight - {
            assert(Exercises.sortByHeavyweight(Map.empty) == Seq.empty)
            assert(Exercises.sortByHeavyweight(Map("Steel" -> (1, 7.86))) == Seq("Steel"))
            assert(Exercises.sortByHeavyweight(Map("Steel" -> (1, 7.86), "Iron" -> (1, 7.874))) == Seq("Steel", "Iron"))
            assert(Exercises.sortByHeavyweight(Map("Steel" -> (3, 7.86), "Iron" -> (3, 7.874), "Aluminum" -> (3, 2.7))) == Seq("Aluminum", "Steel", "Iron"))
        }
    }
}
