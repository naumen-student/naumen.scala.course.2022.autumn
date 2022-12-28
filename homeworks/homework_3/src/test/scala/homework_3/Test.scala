package homework_3

import homework_3.Exercises.EmptySeqException
import utest._

object Test extends TestSuite{

    val tests = Tests{
        'givenTrue_whenPrettyBooleanFormatter1_thenReturnStringRussianTrue - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter1(true) == trueStr)
        }

        'givenFalse_whenPrettyBooleanFormatter1_thenReturnStringRussianFalse - {
            val trueStr = "ложь"
            assert(Exercises.prettyBooleanFormatter1(false) == trueStr)
        }

        'givenString_whenPrettyBooleanFormatter1_thenReturnString - {
            val str = "test"
            assert(Exercises.prettyBooleanFormatter1(str) == str)
        }

        'givenInt_whenPrettyBooleanFormatter1_thenReturnStringValueInt - {
            val value = 10
            assert(Exercises.prettyBooleanFormatter1(value) == value.toString)
        }

        'givenCaseClass_whenPrettyBooleanFormatter1_thenReturnStringValueCaseClass - {
            case class MyClass(name: String)
            val value = MyClass("test")
            assert(Exercises.prettyBooleanFormatter1(value) == value.toString)
        }

        'givenTrue_whenPrettyBooleanFormatter2_thenReturnStringRussianTrue - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter2(true) == trueStr)
        }

        'givenFalse_whenPrettyBooleanFormatter2_thenReturnStringRussianFalse - {
            val trueStr = "ложь"
            assert(Exercises.prettyBooleanFormatter2(false) == trueStr)
        }

        'givenString_whenPrettyBooleanFormatter2_thenReturnString - {
            val str = "test"
            assert(Exercises.prettyBooleanFormatter2(str) == str)
        }

        'givenInt_whenPrettyBooleanFormatter2_thenReturnStringValueInt - {
            val value = 10
            assert(Exercises.prettyBooleanFormatter2(value) == value.toString)
        }

        'givenCaseClass_whenPrettyBooleanFormatter2_thenReturnStringValueCaseClass - {
            case class MyClass(name: String)
            val value = MyClass("test")
            assert(Exercises.prettyBooleanFormatter2(value) == value.toString)
        }

        'givenTrue_whenPrettyBooleanFormatter3_thenReturnStringRussianTrue - {
            val trueStr = "правда"
            assert(Exercises.prettyBooleanFormatter3(true) == trueStr)
        }

        'givenFalse_whenPrettyBooleanFormatter3_thenReturnStringRussianFalse - {
            val trueStr = "ложь"
            assert(Exercises.prettyBooleanFormatter3(false) == trueStr)
        }

        'givenString_whenPrettyBooleanFormatter3_thenReturnString - {
            val str = "test"
            assert(Exercises.prettyBooleanFormatter3(str) == str)
        }

        'givenInt_whenPrettyBooleanFormatter3_thenReturnStringValueInt - {
            val value = 10
            assert(Exercises.prettyBooleanFormatter3(value) == value.toString)
        }

        'givenCaseClass_whenPrettyBooleanFormatter3_thenReturnStringValueCaseClass - {
            case class MyClass(name: String)
            val value = MyClass("test")
            assert(Exercises.prettyBooleanFormatter3(value) == value.toString)
        }

        'givenSeqPositiveInt_whenMax1_thenReturnMaxSeqValue - {
            val testSeq = Seq(1, 5, 7, 2, 3, 4)
            assert(Exercises.max1(testSeq) == 7)
        }

        'givenSeqNegativeInt_whenMax1_thenReturnMaxSeqValue - {
            val testSeq = Seq(-3, -2, -1, -7)
            assert(Exercises.max1(testSeq) == -1)
        }

        'givenEmptySeq_whenMax1_thenThrowEmptySeqException - {
            val testSeq = Seq.empty
            try{
                Exercises.max1(testSeq)
                assert(1 == 0)
            }
            catch {
                case ex: EmptySeqException => assert(ex.getMessage == "empty seq")
            }
        }

        'givenSeqPositiveInt_whenMax2_thenReturnSeqOfMaxValue - {
            val testSeq = Seq(1, 5, 7, 2, 3, 4)
            assert(Exercises.max2(testSeq) == Seq(7))
        }

        'givenSeqNegativeInt_whenMax2_thenReturnSeqOfMaxValue - {
            val testSeq = Seq(-3, -2, -1, -7)
            assert(Exercises.max2(testSeq) == Seq(-1))
        }

        'givenEmptySeq_whenMax2_thenReturnEmptySeq - {
            val testSeq = Seq.empty
            assert(Exercises.max2(testSeq) == Seq.empty)
        }

        'givenSeqPositiveInt_whenMax3_thenReturnSomeOfMaxValue - {
            val testSeq = Seq(1, 5, 7, 2, 3, 4)
            assert(Exercises.max3(testSeq).get == 7)
        }

        'givenSeqNegativeInt_whenMax3_thenReturnSomeOfMaxValue - {
            val testSeq = Seq(-3, -2, -1, -7)
            assert(Exercises.max3(testSeq).get == -1)
        }

        'givenEmptySeq_whenMax3_thenReturnNone- {
            val testSeq = Seq.empty
            assert(Exercises.max3(testSeq).isEmpty)
        }

        'givenPositiveValues_whenSum1_thenReturnSum- {
            val x = 5
            val y = 2
            assert(Exercises.sum1(x, y) == x + y)
        }

        'givenNegativeValues_whenSum1_thenReturnSum- {
            val x = -5
            val y = -2
            assert(Exercises.sum1(x, y) == x + y)
        }

        'givenPositiveAndZiroValues_whenSum1_thenReturnSum- {
            val x = -5
            val y = 0
            assert(Exercises.sum1(x, y) == x + y)
        }

        'givenPositiveValues_whenSum2_thenReturnSum- {
            val x = 5
            val y = 2
            assert(Exercises.sum2(x, y) == x + y)
        }

        'givenNegativeValues_whenSum2_thenReturnSum- {
            val x = -5
            val y = -2
            assert(Exercises.sum2(x, y) == x + y)
        }

        'givenPositiveAndZiroValues_whenSum2_thenReturnSum- {
            val x = -5
            val y = 0
            assert(Exercises.sum2(x, y) == x + y)
        }

        'givenPositiveValues_whenSum3_thenReturnSum- {
            val x = 5
            val y = 2
            assert(Exercises.sum3(x, y) == x + y)
        }

        'givenNegativeValues_whenSum3_thenReturnSum- {
            val x = -5
            val y = -2
            assert(Exercises.sum3(x, y) == x + y)
        }

        'givenPositiveAndZiroValues_whenSum3_thenReturnSum- {
            val x = -5
            val y = 0
            assert(Exercises.sum3(x, y) == x + y)
        }
    }
}
