import utest._


object Test extends TestSuite {

  val tests: Tests = Tests {
    'reverse - {
      assert(Exercises.reverse(Seq(1, 2, 3)) == Seq(3, 2, 1))
      assert(Exercises.reverse(Seq(-1, -2, -3)) == Seq(-3, -2, -1))
	@@ -21,7 +21,7 @@ object Test extends TestSuite{

    'morse - {
      assert(Exercises.morse("SOS") == "... --- ...")
      assert(Exercises.morse("Hello world!") == ".... . .-.. .-.. ---   .-- --- .-. .-.. -.. !")
    }

    'wordReverse - {