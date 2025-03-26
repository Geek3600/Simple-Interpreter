package InterpreterTest 
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import Interpreter._


class AdderSpec extends AnyFlatSpec 
{
    behavior of "Adder"
    val numSample = 100000
    it should "Random Test" in {
        for (i <- 1 to numSample) {
            val a = Random.nextInt(10)
            val b = Random.nextInt(10)
            val sum = a + b

            info(s"$i/$numSample: $a + $b = $sum")
            val interpreter = new Interpreter(s"$a+$b")
            assert(interpreter.expr() == sum)
        }
    }
}