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
            assert(Exercises. sumOfDivBy3Or5(1, 5) == 8)
            assert(Exercises. sumOfDivBy3Or5(2, 10) == 33)
            assert(Exercises. sumOfDivBy3Or5(3, 3) == 3)
            assert(Exercises. sumOfDivBy3Or5(10, 30) == 202)
        }
        'test_primeFactor - {
            assert(Exercises.primeFactor(80) == Seq(2, 5))
            assert(Exercises.primeFactor(98) == Seq(2, 7))
        }
        'test_sumByFunc - {
            val leftVec0 = Vector2D(1, 2)
            val leftVec1 = Vector2D(-2, -2)
            val rightVec0 = Vector2D(4, 3)
            val rightVec1 = Vector2D(-2, 2)
            assert(Exercises.sumScalars(leftVec0, leftVec1, rightVec0, rightVec1) == -8)
            assert("%.2f".format(Exercises.sumCosines(leftVec0, leftVec1, rightVec0, rightVec1)) == "-1,09")
        }
        'test_sortByHeavyweight - {
            val resultSeq = Seq("Tin", "Platinum", "Nickel", "Aluminum", "Titanium", "Lead", "Sodium", "Uranium",
                "Gold", "Tungsten", "Zirconium", "Chrome", "Iron", "Copper", "Silver", "Plutonium", "Cobalt", "Cesium",
                "Calcium", "Lithium", "Magnesium", "Potassium", "Graphite")
            assert(Exercises.sortByHeavyweight(Exercises.balls) == resultSeq)
        }
    }
}
