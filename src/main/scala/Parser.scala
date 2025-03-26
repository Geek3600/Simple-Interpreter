package Parser
import Token._

class Parser
{

    // var current_token: Token = null

    // // 遇到不支持的token时抛出异常
    // def unknown_error() = {
    //     throw new Exception("Unknown token in parsing input")
    // }


    // // 判断当前token的类型符合预期，并且获取下一个token
    // def eat(token_type: String) = {
    //     if (this.current_token._type == token_type) {
    //         this.current_token = this.get_next_token()
    //     }
    //     else {
    //         this.unknown_error()
    //     }
    // }

    // // 获取当前整数
    // def term(): Int = {
    //     val token: Token = this.current_token
    //     this.eat(TOKENS_TYPE.INTEGER)
    //     return token._value.toInt
    // }

    // // Parser + Interpreter 
    // def expr():Int = {

    //     this.current_token = this.get_next_token()

    //     var result: Int = this.term()
    //     while (this.current_token._type == TOKENS_TYPE.PLUS || this.current_token._type == TOKENS_TYPE.SUB) {
    //         if (this.current_token._type == TOKENS_TYPE.PLUS) {
    //             this.eat(TOKENS_TYPE.PLUS)
    //             result += this.term()
    //         }
    //         else {
    //             this.eat(TOKENS_TYPE.SUB)
    //             result -= this.term()
    //         }
    //     }
    //     return result
    // }
}