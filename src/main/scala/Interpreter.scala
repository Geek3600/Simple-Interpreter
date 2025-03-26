package Interpreter

import org.xml.sax.ext.LexicalHandler
import Lexer._
import Parser._
import Token._


// 解释器类
class Interpreter(lexer:Lexer)
{

    val _lexer: Lexer = lexer
    var current_token: Token = null

    // 遇到不支持的token时抛出异常
    def unknown_error() = {
        throw new Exception("Unknown token in parsing input")
    }

    // 判断当前token的类型符合预期，并且获取下一个token
    def eat(token_type: String) = {
        if (this.current_token._type == token_type) {
            this.current_token = this._lexer.get_next_token()
        }
        else {
            this.unknown_error()
        }
    }

    // 获取当前整数
    def factor(): Int = {
        val token: Token = this.current_token
        this.eat(TOKENS_TYPE.INTEGER)
        return token._value.toInt
    }

    // Parser + Interpreter 
    def expr():Int = {

        this.current_token = this._lexer.get_next_token()

        var result: Int = this.factor()
        while (this.current_token._type == TOKENS_TYPE.MUL || this.current_token._type == TOKENS_TYPE.DIV) {
            if (this.current_token._type == TOKENS_TYPE.MUL) {
                this.eat(TOKENS_TYPE.MUL)
                result *= this.factor()
            }
            else {
                this.eat(TOKENS_TYPE.DIV)
                result /= this.factor()
            }
        }
        return result
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