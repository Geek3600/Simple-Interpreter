package Token

// token类型
object TOKENS_TYPE
{
    val EOF    : String = "EOF"
    val INTEGER: String = "INTEGER"
    val PLUS   : String = "+"
    val SUB    : String = "-"
    val MUL    : String = "*"
    val DIV    : String = "/"
    val SPACE  : String = "SPACE"
}

// token类
class Token(token_type: String, token_value:String)
{
    val _type  = token_type
    val _value = token_value

    def str():String = s"Token($_type, $_value)"
}