package Interpreter

import org.xml.sax.ext.LexicalHandler
import Lexer._
import Parser._
import Token._


// 解释器类
class Interpreter(lexer:Lexer)
{

    val _lexer: Lexer = lexer
    var current_token: Token = this._lexer.get_next_token()

    // 遇到不支持的token时抛出异常
    def unknownError() = {
        throw new Exception("Unknown token in parsing input")
    }

    // 除0错误时抛出异常
    def DivisionZeroError() = {
        throw new Exception("Division by zero")
    }

    // 判断当前token的类型符合预期，并且获取下一个token
    def eat(token_type: String) = {
        if (this.current_token._type == token_type) {
            this.current_token = this._lexer.get_next_token()
        }
        else {
            this.unknownError()
        }
    }

    // 获取当前整数
    // factor产生式规则
    def factor(): Int = {
        val token: Token = this.current_token
        this.eat(TOKENS_TYPE.INTEGER)
        return token._value.toInt
    }

    // Parser + Interpreter 
    // term产生式规则
    def term():Int = {
        
        var result: Int = this.factor()

        @scala.annotation.tailrec
        def loop(result: Int):Int =  this.current_token._type match {
            case TOKENS_TYPE.MUL => 
                this.eat(TOKENS_TYPE.MUL)
                val new_result:Int = result * this.factor()
                loop(new_result)
            case TOKENS_TYPE.DIV =>
                this.eat(TOKENS_TYPE.DIV)
                val number: Int = this.factor() match {
                    case 0 => this.DivisionZeroError()
                    case n => n
                }
                val new_result:Int = result / number
                loop(new_result)
            case _ => result
        }

        return loop(result)
    }

    // expr产生式规则
    def expr(): Int = {

        var result: Int = this.term()

        @scala.annotation.tailrec
        def loop(result: Int):Int =  this.current_token._type match {
            case TOKENS_TYPE.PLUS => 
                this.eat(TOKENS_TYPE.PLUS)
                val new_result:Int = result + this.term()
                loop(new_result)
            case TOKENS_TYPE.SUB =>
                this.eat(TOKENS_TYPE.SUB)
                val new_result:Int = result - this.term()
                loop(new_result)
            case _ => result
        }

        return loop(result)
    }


}


object Main {

    def main(args: Array[String]): Unit = {
        while (true) {
            var line: String = null

            // 读取输入
            try {
                line = scala.io.StdIn.readLine("calc> ")
            } catch {
                case e: Exception => {
                    return
                }
            }

            if (line != null) {
                val lexer = new Lexer(line)
                val interpreter: Interpreter = new Interpreter(lexer)
                val result = interpreter.expr()
                println(result)
            }
        }

    }
}