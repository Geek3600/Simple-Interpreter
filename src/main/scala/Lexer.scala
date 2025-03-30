package Lexer

import Token._
import Error._

// 词法分析器
// 词法分析器的作用是将输入的字符串划分成一个个有效的token，并返回有效的token，无效的字符会被忽略（如空格，注释）
class Lexer(val text:String)
{   
    var pos: Int = 0
    var currentChar: Char = this.text(this.pos)
    var lineno: Int = 1
    var column: Int = 1

    def lexerError() = {
        val errorString = "Lexer error on '%c' line: %s column: %s".format(this.currentChar, this.lineno.toString, this.column.toString)
        val lexErr = new LexerError(message = errorString)
        throw new Exception(lexErr.errorMessage)
    }

    // 扫描下一个字符
    def scanNextChar(): Unit= {

        if (this.currentChar == '\n') { // 如果当前字符是换行符
            this.lineno += 1
            this.column = 0
        }

        this.pos += 1 // 指向下一个字符
        if (this.pos > this.text.length() - 1) { // 如果已经到了句子的末尾
            this.currentChar = '\u0000'
        }
        else { // 未到句子的末尾
            this.currentChar = this.text(this.pos)
            this.column += 1
        } 
    }

    // 查看下一个字符，但不移动指针,为了应对多字符的关键字，或者运算符
    def peekNextChar(): Char = {
        var peekPosition = this.pos + 1
        peekPosition match {
            case peekPosition if peekPosition > this.text.length() - 1 => '\u0000'
            case _ => this.text(peekPosition)
        }

    }
    // 查找整数
    def findNumber(): Token = {

        @scala.annotation.tailrec
        def loop(number: String): String = this.currentChar match {
            case c if c.isDigit =>
                val newNumber = number + c
                this.scanNextChar()
                loop(newNumber)
            case _ => number
        }

        var integer: String = loop("")

        this.currentChar match {
            case '.' => 
                integer = integer + "."
                this.scanNextChar()
                integer = integer + loop("")
                new Token(TokenType.REAL_CONSTANT, integer, lineno=this.lineno, column=this.column)
            case _ => new Token(TokenType.INTEGER_CONSTANT, integer, this.lineno, this.column)
        }
    }

    def findIdentifier(): Token = {

        var identifier: String = ""

        @scala.annotation.tailrec
        def loop(): Token = {
            this.currentChar match {
                case c if c.isLetter || c.isDigit =>
                    identifier = identifier + c
                    this.scanNextChar()
                    loop()
                case _ => 
                    // println(identifier)
                    identifier.toUpperCase match {
                        case "BEGIN"   => Token(TokenType.BEGIN, identifier, this.lineno, this.column)
                        case "END"     => Token(TokenType.END, identifier, this.lineno, this.column)
                        case "PROGRAM" => Token(TokenType.PROGRAM, identifier, this.lineno, this.column)
                        case "VAR" => Token(TokenType.VAR, identifier, this.lineno, this.column)
                        case "REAL" => Token(TokenType.REAL, identifier, this.lineno, this.column)
                        case "INTEGER" => Token(TokenType.INTEGER, identifier, this.lineno, this.column)
                        case "DIV" => Token(TokenType.INTEGER_DIV, identifier, this.lineno, this.column)
                        case "PROCEDURE" => Token(TokenType.PROCEDURE, identifier,this.lineno, this.column)
                        case _         => Token(TokenType.IDENTIFIER, identifier, this.lineno, this.column)
                }
            }
        }

        return loop()
    }

    // 跳过注释
    def skipComment(): Unit = {

        @scala.annotation.tailrec
        def loop(): Unit = {
            this.currentChar match {
                case c if c != '}' => 
                    this.scanNextChar()
                    loop()
                case _ =>
                    this.scanNextChar()
            }
        }

        loop()
    }

    // 识别token的类型，并划分token
    def getNextToken(): Token = {

        // 循环是为了跳过任意个空格
        // TODO使用枚举类型进行优化，减少代码量
        @scala.annotation.tailrec
        def loop(): Token = this.currentChar match {
            case '\u0000' => 
                new Token(TokenType.EOF, "", this.lineno, this.column)
            case c if c.isLetter =>        // 识别变量
                this.findIdentifier()
            case c if c.isDigit =>
                this.findNumber()
            case c if c == ':' && this.peekNextChar() == '='=>
                this.scanNextChar()
                this.scanNextChar()
                new Token(TokenType.ASSIGN, ":=", this.lineno, this.column)
            case ';' =>
                this.scanNextChar()
                new Token(TokenType.SEMICONLON, ";", this.lineno, this.column)
            case '.' => 
                this.scanNextChar()
                new Token(TokenType.DOT, ".", this.lineno, this.column)
            case '+' =>
                this.scanNextChar()
                new Token(TokenType.PLUS, "+", this.lineno, this.column)
            case '-' =>
                this.scanNextChar()
                new Token(TokenType.SUB, "-", this.lineno, this.column)
            case '*' =>
                this.scanNextChar()
                new Token(TokenType.MUL, "*", this.lineno, this.column)
            case '/' =>
                this.scanNextChar()
                new Token(TokenType.FLOAT_DIV, "/", this.lineno, this.column)
            case '(' =>
                this.scanNextChar()
                new Token(TokenType.LPAREN, "(", this.lineno, this.column)
            case ')' =>
                this.scanNextChar()
                new Token(TokenType.RPAREN, ")", this.lineno, this.column)
            case ':' =>
                this.scanNextChar()
                new Token(TokenType.COLON, ":", this.lineno, this.column)
            case ',' =>
                this.scanNextChar()
                new Token(TokenType.COMMA, ",", this.lineno, this.column)
            case '{' =>
                this.scanNextChar()
                this.skipComment()
                loop()
            case ' ' => 
                this.scanNextChar()
                loop()
            case _ => 
                // println(this.currentChar)
                lexerError()
                
        }
        
        return loop()
    }

}