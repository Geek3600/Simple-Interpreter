package Interpreter


// token类型
object TOKENS_TYPE
{
    val EOF    : String = "EOF"
    val INTEGER: String = "INTEGER"
    val PLUS   : String = "+"
    val SUB    : String = "-"
    val SPACE  : String = "SPACE"
}

// token类
class Token(token_type: String, token_value:String)
{
    val _type  = token_type
    val _value = token_value

    def str():String = s"Token($_type, $_value)"
}

// 解释器类
class Interpreter(line:String)
{
    var pos: Int = 0
    val text: String = line
    var current_token: Token = null
    
    def error() = {
        throw new Exception("Error parsing input")
    }

    // 识别并获取下一个token
    def get_next_token(): Token = {
        
        // 保存整个表达式
        var _text:String = this.text

        // 如果已经到了句子的末尾，返回EOF
        if (this.pos > this.text.length() - 1)  {
            return new Token(TOKENS_TYPE.EOF, null)
        }

        // 保存当前指向的字符
        val current_char:Char = _text(this.pos)

        // 如果当前字符是数字，则返回一个整数token
        if (current_char.isDigit) {
            val temp_token = new Token(TOKENS_TYPE.INTEGER, current_char.toString())
            this.pos += 1
            return temp_token
        }
        else if (current_char == '+') {  // 如果当前字符是加号，则返回一个加号token
            val temp_token = new Token(TOKENS_TYPE.PLUS, "+")
            this.pos += 1
            return temp_token
        }
        else if (current_char == '-') {  // 如果当前字符是减号，则返回一个减号token
            val temp_token = new Token(TOKENS_TYPE.SUB, "-")
            this.pos += 1
            return temp_token
        }
        else if (current_char == ' ') {  // 如果当前字符是空格，直接跳过
            this.pos += 1
            return temp_token
        }

        this.error()
    }

    // 保存获取的token
    def eat(token_type: String) = {
        if (this.current_token._type == token_type) {
            this.current_token = this.get_next_token()
        }
        else {
            this.error()
        }
    }

    def expr() = {
        /*expression -> INTEGER PLUS INTEGER*/

        // 获取第一个token
        this.current_token = this.get_next_token()

        // 获取左边第一个整数
        val left:Token = this.current_token
        // println(left.str())
        this.eat(TOKENS_TYPE.INTEGER)

        // 获取加号
        val op:Token = this.current_token
        //  println(op.str())
        this.eat(TOKENS_TYPE.PLUS)

        // 获取右边第二个整数
        val right:Token  = this.current_token
        //  println(right.str())
        this.eat(TOKENS_TYPE.INTEGER)   

        // 相加
        left._value.toInt + right._value.toInt
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
                val interpreter: Interpreter = new Interpreter(line)
                val result = interpreter.expr()
                println(result)
            }
        }

    }
}