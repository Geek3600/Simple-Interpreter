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
    var current_char: Char = this.text(this.pos)
    
    // 遇到不支持的token时抛出异常
    def unknown_error() = {
        throw new Exception("Unknown token in parsing input")
    }

    // 扫描下一个字符
    def scan_next_position() = {
        this.pos += 1 // 指向下一个字符
        if (this.pos > this.text.length() - 1) { // 如果已经到了句子的末尾
            this.current_char = '\u0000'
        }
        else { // 如果没有到了句子的末尾
            this.current_char = this.text(this.pos)
        } 

    }
    
    // 获取当前整数
    def term(): Int = {
        val token: Token = this.current_token
        this.eat(TOKENS_TYPE.INTEGER)
        return token._value.toInt
    }


    def find_integer(): String = {
        var integer: String = ""
        while (this.current_char != '\u0000' && this.current_char.isDigit) {
            integer = integer + this.current_char
            this.scan_next_position()
        }
        return integer
    }

    // 词法分析器: 分析一个token的类型，并返回token
    def get_next_token(): Token = {

        // 循环是为了跳过任意个空格
        while (this.current_char != '\u0000') {
            if (current_char.isDigit) {     // 如果当前字符是数字，则返回一个整数token
                return new Token(TOKENS_TYPE.INTEGER, this.find_integer())
            }
            else if (current_char == '+') {  // 如果当前字符是加号，则返回一个加号token
                val temp_token = new Token(TOKENS_TYPE.PLUS, "+")
                this.scan_next_position()
                return temp_token
            }
            else if (current_char == '-') {  // 如果当前字符是减号，则返回一个减号token
                val temp_token = new Token(TOKENS_TYPE.SUB, "-")
                this.scan_next_position()
                return temp_token
            }
            else if (current_char == ' ') {  // 如果当前字符是空格，直接跳过
                this.scan_next_position()
            }
            else {
                this.unknown_error()
            }
        }
        return new Token(TOKENS_TYPE.EOF, "")
    }

    // 判断当前token的类型符合预期，并且获取下一个token
    def eat(token_type: String) = {
        if (this.current_token._type == token_type) {
            this.current_token = this.get_next_token()
        }
        else {
            this.unknown_error()
        }
    }

    // Parser + Interpreter 
    def expr():Int = {

        this.current_token = this.get_next_token()

        var result: Int = this.term()
        while (this.current_token._type == TOKENS_TYPE.PLUS || this.current_token._type == TOKENS_TYPE.SUB) {
            if (this.current_token._type == TOKENS_TYPE.PLUS) {
                this.eat(TOKENS_TYPE.PLUS)
                result += this.term()
            }
            else {
                this.eat(TOKENS_TYPE.SUB)
                result -= this.term()
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
                val interpreter: Interpreter = new Interpreter(line)
                val result = interpreter.expr()
                println(result)
            }
        }

    }
}