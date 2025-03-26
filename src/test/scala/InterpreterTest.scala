package InterpreterTest 
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import Interpreter._
import Lexer._

class AdderSpec extends AnyFlatSpec 
{   
    def expression_generator(): Tuple2[String, Int] = {
        var expr: String = ""
        val get_random_op  = () => {if (Random.nextInt(2) == 0) "+" else "-"}
        val get_random_num = () => {Random.nextInt(10020)}
        var op: String = "+"
        var result: Int = 0
        var random_int:Int = 0

        val expr_length = 101
        for (i <- 1 to expr_length) {

            if (i % 2 == 1) {
                random_int = get_random_num()
                expr = expr + random_int.toString()
                if (op == "+") {
                    result += random_int
                }
                else {
                    result -= random_int
                }
            }
            else {
                op = get_random_op()
                expr = expr + op
            }
        }


        (expr, result)
    }

    behavior of "Adder"
    val numSample = 1000
    it should "Random Test" in {
        for (i <- 1 to numSample) {
            val expr_gen:Tuple2[String, Int] = expression_generator()

            info(s"$i/$numSample")

            val lexer = new Lexer(expr_gen._1)
            val interpreter = new Interpreter(lexer)
            assert(interpreter.expr() == expr_gen._2)
        }
    }
}