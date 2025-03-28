package Token

// token类型
object TokenType
{
    val EOF: String = "EOF"
    val INTEGER: String = "INTEGER"
    val PLUS: String = "+"
    val SUB: String = "-"
    val MUL: String = "*"
    val INTEGER_DIV: String = "DIV"
    val FLOAT_DIV: String = "/"
    val SPACE: String = "SPACE"
    val LPAREN: String = "("
    val RPAREN: String = ")"
    val IDENTIFIER: String = "ID" // 变量名
    val ASSIGN:      String = ":="
    val SEMICONLON: String = ";"
    val BEGIN: String = "BEGIN"
    val END: String = "END"
    val DOT: String = "."
    val COMMA: String = ","
    val COLON: String = ":"
    val VAR: String = "VAR"
    val PROGRAM: String = "PROGRAM"
    val INTEGER_CONSTANT: String = "INTEGER_CONSTANT"
    val REAL_CONSTANT: String = "FLOAT_CONSTANT"
    val REAL: String = "REAL"
}

// token类
class Token(val tokenType: String, val tokenValue:String )
{
    def str():String = s"Token($tokenType, $tokenValue)"
}