package InterpreterTest 
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.{Random, Try}
import scala.sys.process._
import scala.util.Try

import Interpreter._
import Parser._
import Lexer._

class AdderSpec extends AnyFlatSpec 
{   
    val numberRange = 5
    val binaryOperatorRange = 4
    val unaryOperatorRange = 2
    def expressionGenerator(): Tuple2[String, Int] = {

        def binaryOperatorGenerator() = Random.nextInt(binaryOperatorRange) match {
                case 0 => "+"
                case 1 => "-"
                case 2 => "*"
                case 3 => "/" 
        }

        def unaryOperatorGenerator() = Random.nextInt(unaryOperatorRange) match {
            case 0 => "+"
            case 1 => "-"
        }

        def numberGenerator(): String = {
            val number = Random.nextInt(numberRange) + 1
            number.toString
        }
        
        val expression: String = numberGenerator()

        def loop(expression: String): String = Random.nextInt(4) match {
            case 0 => 
                val number: String = numberGenerator()
                val newOperator: String = binaryOperatorGenerator()
                val newExpression: String = expression + newOperator + number
                loop(newExpression)
            case 1 =>
                val newExpression: String = "(" + expression + ")"
                loop(newExpression)
            case 2 =>
                val newOperator: String = unaryOperatorGenerator()
                val newExpression: String = newOperator + expression
                loop(newExpression)
            case _ => 
                expression
        }
            

        def evaluateWithScala(expr: String): Either[String, Int] = {
            val scalaCode = s"\"println($expr)\""
            println(s"scala -nc -e $scalaCode")
            try {
                val result = s"scala -nc -e $scalaCode".!!.trim.toInt
                Right(result)
            } 
            catch {
                case e: Exception => 
                    Left(s"Error: ${e.getMessage}")
            }
        }

        val newExpression = loop(expression)
        val result = evaluateWithScala(newExpression) match {
            case Right(value) => value
            case Left(error) => 0
        }
        (newExpression, result)

    }

    behavior of "Adder"
    val numSample = 10
    it should "Random Test" in {
        for (i <- 1 to numSample) {
            val exprGenerated:Tuple2[String, Int] = expressionGenerator()

            info(s"$i/$numSample" + " " + exprGenerated._1 + " " + exprGenerated._2)

            val lexer = new Lexer(exprGenerated._1)
            val parser = new Parser(lexer)
            val interpreter = new Interpreter(parser)
            assert(interpreter.interprete() == exprGenerated._2)
        }
    }
}