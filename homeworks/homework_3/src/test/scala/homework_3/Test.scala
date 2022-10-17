package homework_3

import utest._

object Test extends TestSuite{

    val tests = Tests{
        'test_example - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
        }
        'test_prettyBooleanFormatter - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter2(0) == "0")
            assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
            assert(Exercises.prettyBooleanFormatter3("string") == "string")
        }

        'test_max_seq_have_elems - {
            val someSeq = Seq(1, 2, 3)
            assert(Exercises.max1(someSeq) == 3)
            assert(Exercises.max2(someSeq) == Seq(3))
            assert(Exercises.max3(someSeq).contains(3))
        }

        'test_max_seq_empty - {
            val emptySeq = Seq()
            assert(Exercises.max2(emptySeq).isEmpty)
            assert(Exercises.max3(emptySeq).isEmpty)
        }
    }
}
