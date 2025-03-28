package Lexer

import Token._

// 词法分析器，传入待解析的表达式
class Lexer(val text:String)
{   
    var pos: Int = 0
    var currentChar: Char = this.text(this.pos)

    def unknownError() = {
        throw new Exception("Unknown token in parsing input")
    }

    // 扫描下一个字符
    def scanNextChar(): Unit= {
        this.pos += 1 // 指向下一个字符
        if (this.pos > this.text.length() - 1) { // 如果已经到了句子的末尾
            this.currentChar = '\u0000'
        }
        else { // 未到句子的末尾
            this.currentChar = this.text(this.pos)
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
                new Token(TokenType.REAL_CONSTANT, integer)
            case _ => new Token(TokenType.INTEGER_CONSTANT, integer)
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
                    identifier match {
                        case "BEGIN"   => Token(TokenType.BEGIN, identifier)
                        case "END"     => Token(TokenType.END, identifier)
                        case "PROGRAM" => Token(TokenType.PROGRAM, identifier)
                        case "VAR" => Token(TokenType.VAR, identifier)
                        case "REAL" => Token(TokenType.REAL, identifier)
                        case "INTEGER" => Token(TokenType.INTEGER, identifier)
                        case "DIV" => Token(TokenType.INTEGER_DIV, identifier)
                        case _         => Token(TokenType.IDENTIFIER, identifier)
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
        @scala.annotation.tailrec
        def loop(): Token = this.currentChar match {
            case '\u0000' => 
                new Token(TokenType.EOF, "")
            case c if c.isLetter =>        // 识别变量
                this.findIdentifier()
            case c if c.isDigit =>
                this.findNumber()
            case c if c == ':' && this.peekNextChar() == '='=>
                this.scanNextChar()
                this.scanNextChar()
                new Token(TokenType.ASSIGN, ":=")
            case ';' =>
                this.scanNextChar()
                new Token(TokenType.SEMICONLON, ";")
            case '.' => 
                this.scanNextChar()
                new Token(TokenType.DOT, ".")
            case '+' =>
                this.scanNextChar()
                new Token(TokenType.PLUS, "+")
            case '-' =>
                this.scanNextChar()
                new Token(TokenType.SUB, "-")
            case '*' =>
                this.scanNextChar()
                new Token(TokenType.MUL, "*")
            case '/' =>
                this.scanNextChar()
                new Token(TokenType.FLOAT_DIV, "/")
            case '(' =>
                this.scanNextChar()
                new Token(TokenType.LPAREN, "(")
            case ')' =>
                this.scanNextChar()
                new Token(TokenType.RPAREN, ")")
            case ':' =>
                this.scanNextChar()
                new Token(TokenType.COLON, ":")
            case ',' =>
                this.scanNextChar()
                new Token(TokenType.COMMA, ",")
            case '{' =>
                this.scanNextChar()
                this.skipComment()
                loop()
            case ' ' => 
                this.scanNextChar()
                loop()
            case _ => 
                // println(this.currentChar)
                unknownError()
                
        }
        
        return loop()
    }

}