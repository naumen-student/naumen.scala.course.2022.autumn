import utest._
import Exercises._

object Test extends TestSuite{

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(Exercises.divBy3Or7(1, 3) == Seq(3))
            assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
    }

    'test_divBy3Or5 - {
        assert(sumOfDivBy3Or5(0, 2) == 0)
        assert(sumOfDivBy3Or5(14, 16) == 15)
        assert(sumOfDivBy3Or5(4, 6) == Seq(5, 6).sum)
        assert(sumOfDivBy3Or5(5, 19) == Seq(5, 6, 9, 10, 12, 15, 18).sum)
    }

    'test_primeFactor - {
        assert(primeFactor(8) == Seq(2))
        assert(primeFactor(50) == Seq(2, 5))
        assert(primeFactor(17) == Seq(17))
    }

    'test_sumScalars - {
        assert(sumScalars(Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1), Vector2D(1, 1)) == 4)
        assert(sumScalars(Vector2D(0, 0), Vector2D(0, 0), Vector2D(0, 0), Vector2D(0, 0)) == 0)
        assert(sumScalars(Vector2D(1, 1), Vector2D(2, 2), Vector2D(1, 1), Vector2D(3, 3)) == 10)
    }

    'test_sumCosines - {
        assert(Exercises.sumCosines(Vector2D(1, 0), Vector2D(0, 1), Vector2D(1, 0), Vector2D(1, 0)) == 1)
        assert((sumCosines(Vector2D(1, 1), Vector2D(1, -1), Vector2D(-1, 1), Vector2D(1, 1)) - 0d).abs < 1e-6)
        assert((sumCosines(Vector2D(10, 0), Vector2D(9, 9), Vector2D(1, -1), Vector2D(0, -1)) - math.sqrt(2)).abs < 1e-6)
    }

    'test_sortByHeavyweight - {
        assert(sortByHeavyweight(Map.empty) == Seq.empty)
        assert(sortByHeavyweight(Map("Tin" -> (1, 7.29))) == Seq("Tin"))
        assert(
            sortByHeavyweight() == Seq(
                "Tin",
                "Platinum",
                "Nickel",
                "Aluminum",
                "Titanium",
                "Lead",
                "Sodium",
                "Uranium",
                "Gold",
                "Tungsten",
                "Zirconium",
                "Chrome",
                "Iron",
                "Copper",
                "Silver",
                "Plutonium",
                "Cobalt",
                "Cesium",
                "Calcium",
                "Lithium",
                "Magnesium",
                "Potassium",
                "Graphite"
            )
        )
    }
}
}
}
