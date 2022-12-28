import utest._

object Test extends TestSuite {

  val tests = Tests {
    'test_divBy3Or7 - {
      assert(Exercises.divBy3Or7(1, 3) == Seq(3))
      assert(Exercises.divBy3Or7(5, 9) == Seq(6, 7, 9))
      assert(Exercises.divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
    }
    'test_sumOfDivBy3Or5 - {
      assert(Exercises.sumOfDivBy3Or5(1, 1) == 0)
      assert(Exercises.sumOfDivBy3Or5(-1, 1) == 0)
      assert(Exercises.sumOfDivBy3Or5(1, 3) == 3)
      assert(Exercises.sumOfDivBy3Or5(3, 3) == 3)
      assert(Exercises.sumOfDivBy3Or5(-3, -2) == -3)
      assert(Exercises.sumOfDivBy3Or5(-3, 3) == 0)
      assert(Exercises.sumOfDivBy3Or5(2, 5) == 8)
      assert(Exercises.sumOfDivBy3Or5(-3, 5) == 5)
      assert(Exercises.sumOfDivBy3Or5(1, 20) == 98)
      assert(Exercises.sumOfDivBy3Or5(100, 103) == 202)
    }
    'test_primeFactor - {
      assert(Exercises.primeFactor(80) == Seq(2, 5))
      assert(Exercises.primeFactor(98) == Seq(2, 7))
      assert(Exercises.primeFactor(-98) == Seq(2, 7))
      assert(Exercises.primeFactor(13) == Seq(13))
      assert(Exercises.primeFactor(0) == Seq())
      assert(Exercises.primeFactor(1) == Seq())
      assert(Exercises.primeFactor(58) == Seq(2, 29))
      assert(Exercises.primeFactor(1024) == Seq(2))
      assert(Exercises.primeFactor(65676) == Seq(2, 3, 13, 421))
    }
    'test_sumScalars - {
      assert(Exercises.sumScalars(
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(1, 0)) == 2)
      assert(Exercises.sumScalars(
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(0, 1),
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(1, 0)) == 1)
      assert(Exercises.sumScalars(
        Exercises.Vector2D(2, 3),
        Exercises.Vector2D(1, 2),
        Exercises.Vector2D(4, 2),
        Exercises.Vector2D(9, 1)) == 46)
      assert(Exercises.sumScalars(
        Exercises.Vector2D(132, 12),
        Exercises.Vector2D(72, 18),
        Exercises.Vector2D(32, 12),
        Exercises.Vector2D(81, 37))
        ==
        Exercises.sumScalars(
          Exercises.Vector2D(72, 18),
          Exercises.Vector2D(132, 12),
          Exercises.Vector2D(81, 37),
          Exercises.Vector2D(32, 12))
      )
    }
    'test_cosBetween - {
      assert(Exercises.sumCosines(
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(0, 1),
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(1, 0)) == 1)
      assert(Exercises.sumCosines(
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(0, 1),
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(1, 1)) - 1 / math.sqrt(2) < 1e-5)
      assert(Exercises.sumCosines(
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(-1, 0),
        Exercises.Vector2D(1, 0),
        Exercises.Vector2D(1, 1)) - (-1 + 1 / math.sqrt(2)) < 1e-5)
    }
    'test_sortByHeavyweight -> {
      assert(Exercises.sortByHeavyweight(Map("Aluminum" -> (3, 2.6889), "Gold" -> (6, 19.32))) == Seq("Aluminum", "Gold"))
      assert(Exercises.sortByHeavyweight(Map("Aluminum" -> (3, 2.6889), "Potassium" -> (14, 0.862))) == Seq("Aluminum", "Potassium"))
      assert(Exercises.sortByHeavyweight(Map()) == Seq())
    }
  }
}
