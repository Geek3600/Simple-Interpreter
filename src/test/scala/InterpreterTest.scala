package InterpreterTest 
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import Interpreter._


class AdderSpec extends AnyFlatSpec 
{
    behavior of "Adder"
    val numSample = 1000
    it should "Random Test" in {
        for (i <- 1 to numSample) {
            val a = Random.nextInt(99999)
            val b = Random.nextInt(999999)
            val op_select = Random.nextInt(2) 
            val op = if (op_select == 0) "+" else "-"

            var sum = 0
            if (op_select == 0)  {
                sum = a + b

            }
            else  sum = a - b

            info(s"$i/$numSample: $a $op $b = $sum")
            val interpreter = new Interpreter(s"$a$op$b")
            assert(interpreter.expr() == sum)
        }
    }
}