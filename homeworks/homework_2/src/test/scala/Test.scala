import Exercises._
import utest._

object Test extends TestSuite {
  trait EpsEq[A] {
    def ~=(left: A, right: A): Boolean
  }

  implicit class EpsEqOps[A](private val left: A) extends AnyVal {
    def ~=(right: A)(implicit epsEq: EpsEq[A]): Boolean =
      epsEq.~=(left, right)
  }

  implicit val EpsEqDouble: EpsEq[Double] = {
    val epsilon = 1e-6
    (left: Double, right: Double) => math.abs(left - right) < epsilon
  }

  val tests: Tests = Tests {
    'test_divBy3Or7 - {
      assert(divBy3Or7(1, 3) == Seq(3))
      assert(divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(
        divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28,
          30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69,
          70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99)
      )
    }
    'test_sumOfDivBy3Or5 - {
      assert(sumOfDivBy3Or5(0, 1) == 0)
      assert(sumOfDivBy3Or5(0, 3) == 3)
      assert(sumOfDivBy3Or5(0, 5) == 8)
      assert(sumOfDivBy3Or5(0, 6) == 14)
      assert(sumOfDivBy3Or5(6, 6) == 6)
    }
    'test_primeFactor - {
      assert(primeFactor(1) == Seq.empty[Int])
      assert(primeFactor(2) == Seq(2))
      assert(List(2, 4, 8, 16).map(primeFactor).forall(_ == Seq(2)))
      assert(primeFactor(1 * 2 * 3 * 4 * 5) == Seq(2, 3, 5))
    }
    'test_sumScalars - {
      assert(
        sumScalars(
          Vector2D(0, 0),
          Vector2D(0, 0),
          Vector2D(0, 0),
          Vector2D(0, 0)
        ) ~= 0
      )
      assert(
        sumScalars(
          Vector2D(1, 0),
          Vector2D(1, 0),
          Vector2D(1, 1),
          Vector2D(1, 1)
        ) ~= 3
      )
      assert(
        sumScalars(
          Vector2D(5, 1),
          Vector2D(4, 2),
          Vector2D(3, 3),
          Vector2D(2, 4)
        ) ~= 40
      )
    }

    'test_sumCosines - {
      assert(
        sumCosines(
          Vector2D(1, 1),
          Vector2D(1, 1),
          Vector2D(1, 1),
          Vector2D(1, 1)
        ) ~= 2
      )
      assert(
        sumCosines(
          Vector2D(1, 0),
          Vector2D(0, 1),
          Vector2D(1, 1),
          Vector2D(1, 1)
        ) ~= 1
      )
      assert(
        sumCosines(
          Vector2D(1, 0),
          Vector2D(0, 1),
          Vector2D(1, 1),
          Vector2D(-1, -1)
        ) ~= -1
      )
      assert(
        sumCosines(
          Vector2D(1, 0),
          Vector2D(0, 1),
          Vector2D(1, 0),
          Vector2D(-1, 0)
        ) ~= -1
      )
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
