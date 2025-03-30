package Error
import Token._
object ErrorCode
{
    val UNKNOWN_TOKEN        = "Unknown token"
    val ID_NOT_FOUND         = "Identifier not found"
    val DUPLICATE_ID         = "Duplicate id found"
    val PARSER_END_ERROR     = "Parser end error"
    val PARSER_TYPE_ERROR    = "Parser type error"
    val PARSER_FACTOR_ERROR  = "Parser factor error"
}

abstract class Error(val errorCode: String = null, val token: Token = null, val message: String = null)
{
    val errorMessage: String = s"${this.getClass.getSimpleName} : $message"
}



class LexerError(override val message: String) extends Error(message = message)
class ParserError(override val errorCode: String, override val token: Token, override val message: String) extends Error(errorCode=errorCode, token=token, message=message)
class SemanticError(override val errorCode: String, override val token: Token, override val message: String) extends Error(errorCode=errorCode, token=token, message=message)