package InterpreterTest 
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.{Random, Try}
import scala.sys.process._
import Interpreter._
import Lexer._

class AdderSpec extends AnyFlatSpec 
{   
    val numberRange = 5
    val operatorRange = 4
    def expressionGenerator(): Tuple2[String, Int] = {

        def operatorGenerator() = Random.nextInt(operatorRange) match {
                case 0 => "+"
                case 1 => "-"
                case 2 => "*"
                case 3 => "/" 
        }

        def numberGenerator(): String = {
            val number = Random.nextInt(numberRange) + 1
            number.toString
        }
        
        val expression: String = numberGenerator()

        def loop(expression: String): String = Random.nextInt(2) match {
            case 0 => 
                val number: String = numberGenerator()
                val newOperator: String = operatorGenerator()
                val newExpression: String = expression + newOperator + number
                loop(newExpression)
            case _ => 
                expression
        }
            

        def evaluateWithPython(expr: String): Either[String, Int] = {
            val pythonCode = s"\"print(round(eval('$expr')))\"".replace("/", "//")
            try {
                val result = s"python -c $pythonCode".!!.trim.toInt
                Right(result)
            } 
            catch {
                case e: Exception => 
                    Left(s"Error: ${e.getMessage}")
            }
        }

        val newExpression = loop(expression)
        val result = evaluateWithPython(newExpression) match {
            case Right(value) => value
            case Left(error) => 0
        }
        (newExpression, result)

    }

    behavior of "Adder"
    val numSample = 100
    it should "Random Test" in {
        for (i <- 1 to numSample) {
            val exprGenerated:Tuple2[String, Int] = expressionGenerator()

            info(s"$i/$numSample" + " " + exprGenerated._1 + " " + exprGenerated._2)

            val lexer = new Lexer(exprGenerated._1)
            val interpreter = new Interpreter(lexer)
            assert(interpreter.expr() == exprGenerated._2)
        }
    }
}