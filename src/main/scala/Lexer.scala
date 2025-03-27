package Lexer

import Token._

// 词法分析器，传入待解析的表达式
class Lexer(line:String)
{   
    var pos: Int = 0
    val text: String = line
    var current_char: Char = this.text(this.pos)

    def unknownError() = {
        throw new Exception("Unknown token in parsing input")
    }

    // 扫描下一个字符
    def scan_next_position() = {
        this.pos += 1 // 指向下一个字符
        if (this.pos > this.text.length() - 1) { // 如果已经到了句子的末尾
            this.current_char = '\u0000'
        }
        else { // 未到句子的末尾
            this.current_char = this.text(this.pos)
        } 

    }
    
    // 查找整数
    def find_integer(): String = {
        var integer: String = ""
        while (this.current_char != '\u0000' && this.current_char.isDigit) {
            integer = integer + this.current_char
            this.scan_next_position()
        }
        return integer
    }

    // 识别token的类型，并划分token
    def get_next_token(): Token = {

        // 循环是为了跳过任意个空格
        @scala.annotation.tailrec
        def loop(): Token = this.current_char match {
            case '\u0000' => 
                new Token(TOKENS_TYPE.EOF, "")
            case c if c.isDigit =>
                new Token(TOKENS_TYPE.INTEGER, this.find_integer())
            case '+' =>
                this.scan_next_position()
                new Token(TOKENS_TYPE.PLUS, "+")
            case '-' =>
                this.scan_next_position()
                new Token(TOKENS_TYPE.SUB, "-")
            case '*' =>
                this.scan_next_position()
                new Token(TOKENS_TYPE.MUL, "*")
            case '/' =>
                this.scan_next_position()
                new Token(TOKENS_TYPE.DIV, "/")
            case ' ' => 
                this.scan_next_position()
                loop()
            case _ => 
                unknownError()
                
        }
        
        return loop()
    }

}