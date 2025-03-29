package Parser
import Token._
import Lexer._
import AbstractTree._

// 递归下降语法分析器，构造抽象语法树AST
// 根据语法规则，也就是产生式，识别语法结构，检查语法是否错误，并产生对应的抽象语法树
// 将产生式转为实际代码
class Parser(val lexer: Lexer)
{
    var currentToken: Token = this.lexer.getNextToken()

    def syntaxError() = {
        throw new Exception("Syntax error")
    }
    // 除0错误时抛出异常

    // 先判断当前token的类型是否符合预期，是否符合语法规则，然后获取下一个token
    def eat(tokenType: String) = {
        if (this.currentToken.tokenType == tokenType) {
            // println("syntaxError " + this.currentToken.tokenType + " " + tokenType + " "+ this.currentToken.tokenValue)
            this.currentToken = this.lexer.getNextToken()
        }
        else {
            println("syntaxError " + this.currentToken.tokenType + " " + tokenType + " "+ this.currentToken.tokenValue)
            this.syntaxError()
        }
    }

    // factor产生式规则
    def factor(): ASTNode = {
        /* factor : PLUS factor | SUB factor| INTEGER_CONST | FLOAT_CONST | LPAREN expr RPAREN | variable */
        val token: Token = this.currentToken
        token.tokenType match {
            case TokenType.INTEGER_CONSTANT =>
                this.eat(TokenType.INTEGER_CONSTANT)
                return NumberNode(token)
            case TokenType.REAL_CONSTANT =>
                this.eat(TokenType.REAL_CONSTANT)
                return NumberNode(token)
            case TokenType.LPAREN =>
                this.eat(TokenType.LPAREN)      // (
                val node: ASTNode = this.expr() // expr
                this.eat(TokenType.RPAREN)      // )
                return node    
            case TokenType.PLUS =>
                this.eat(TokenType.PLUS)
                return UnaryOperationNode(Token(TokenType.PLUS, "+"), this.factor())
            case TokenType.SUB =>
                this.eat(TokenType.SUB)
                return UnaryOperationNode(Token(TokenType.SUB, "-"), this.factor())
            case TokenType.IDENTIFIER =>
                return this.variable()
            case _ => this.syntaxError()
        }
    }

    // term产生式规则
    def term(): ASTNode = {
        /* term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)* */
        var node: ASTNode = this.factor()

        @scala.annotation.tailrec
        def loop(node: ASTNode): ASTNode = this.currentToken.tokenType match {
            case TokenType.MUL => 
                this.eat(TokenType.MUL)
                loop(BinaryOperationNode(node, Token(TokenType.MUL, "*"), this.factor()))
            case TokenType.INTEGER_DIV =>
                this.eat(TokenType.INTEGER_DIV)
                loop(BinaryOperationNode(node, Token(TokenType.INTEGER_DIV, "DIV"), this.factor()))
            case TokenType.FLOAT_DIV =>
                this.eat(TokenType.FLOAT_DIV)
                loop(BinaryOperationNode(node, Token(TokenType.FLOAT_DIV, "/"), this.factor()))
            case _ => node
        }

        return loop(node)
    }

    // expr产生式规则
    def expr(): ASTNode = {

        /* expr : term((PLUS|SUB)term)*  */
        var node: ASTNode = this.term()

        @scala.annotation.tailrec
        def loop(node: ASTNode): ASTNode = this.currentToken.tokenType match {
            case TokenType.PLUS => 
                this.eat(TokenType.PLUS)
                loop(BinaryOperationNode(node, Token(TokenType.PLUS, "+"), this.term()))
            case TokenType.SUB =>
                this.eat(TokenType.SUB)
                loop(BinaryOperationNode(node, Token(TokenType.SUB, "-"), this.term()))
            case _ => node
        }

        return loop(node)
    }

    // program产生式规则
    def program(): ASTNode = {
        /* program : PROGRAM variable SEMI block DOT */
        // var node: ASTNode = this.compoundStatement()
        // this.eat(TokenType.DOT)
        // return node
        
        // 吞掉PROGRAM关键字
        this.eat(TokenType.PROGRAM)
        val variableNode: ASTNode = this.variable() // 获取PROGRAM后的标识符节点
        val programName: String = variableNode.asInstanceOf[VariableNode].token.tokenValue // 获取PROGRAM块的名字

        this.eat(TokenType.SEMICONLON) // 吞掉分号
        val blockNode: ASTNode = this.block()
        this.eat(TokenType.DOT) // 吞掉.

        ProgramNode(programName, blockNode)
    }

    def compoundStatement(): ASTNode = {
        /* compound statement: BEGIN statement_list END */
        this.eat(TokenType.BEGIN) // begin
        val nodes: List[ASTNode] = this.statementList()
        this.eat(TokenType.END) // end

        val root: ASTNode = CompoundStatementNode(nodes)
        return root
    }

    // 为什么要用链表？因为无法确定有多少条声明语句，所以用链表来存储
    def statementList(): List[ASTNode] = {
        /* statement_list: statement | statement SEMICONLON statement_list */

        @scala.annotation.tailrec
        def loop(statementNodeList: List[ASTNode]): List[ASTNode] = this.currentToken.tokenType match {
            case TokenType.SEMICONLON =>
                this.eat(TokenType.SEMICONLON)
                val statementNode: ASTNode = this.statement()
                val newStatementNodeList: List[ASTNode] = statementNodeList :+ statementNode  // 把节点插到链表的末尾
                loop(newStatementNodeList)
            case _ => statementNodeList
        }

        val node: ASTNode = this.statement()
        val resultList: List[ASTNode] = List(node)
        
        return loop(resultList)
    }

    // 语句产生式
    def statement(): ASTNode = {
        /* statement: compound_statement | assignment_statement | empty */
        this.currentToken.tokenType match {
            case TokenType.BEGIN => this.compoundStatement()
            case TokenType.IDENTIFIER => this.assignmentStatement()
            case _ => this.empty()
        }
    }

    // 赋值语句产生式
    def assignmentStatement(): ASTNode = {
        /* assignment_statement: variable ASSIGN expr */
        val left: ASTNode = this.variable()
        val assignToken = this.currentToken
        this.eat(TokenType.ASSIGN)
        val right: ASTNode = this.expr()
        return AssignStatementNode(left, assignToken, right)
    }

    // 变量产生式
    def variable(): ASTNode = {
        /* variable: ID */
        val node: ASTNode = VariableNode(this.currentToken)
        this.eat(TokenType.IDENTIFIER)
        return node
    }

    // 空产生式
    def empty(): ASTNode = {
        /* empty: */
        return EmptyOperationNode()
    }

    def block(): ASTNode = {
        /* block: declarations compound_statement */
        val declarations: List[ASTNode] = this.declarations()
        val compoundStatement: ASTNode = this.compoundStatement()
        return BlockNode(declarations, compoundStatement)
    }
    
    def formalParameters(): List[ASTNode] = {
        /* formalParameters : ID (COMMA ID)* COLON type_spec */
        @scala.annotation.tailrec
        def loop(paramTokensList: List[Token]): List[Token] = this.currentToken.tokenType match {
            case TokenType.COMMA =>
                this.eat(TokenType.COMMA)
                val newParamTokensList: List[Token] = paramTokensList :+ this.currentToken
                this.eat(TokenType.IDENTIFIER)
                loop(newParamTokensList)
            case _ => paramTokensList
        }

        val paramTokensList: List[Token] = List(this.currentToken)
        this.eat(TokenType.IDENTIFIER) // 吞掉第一个参数
        val newParamTokensList: List[Token] = loop(paramTokensList) // 递归获取所有参数的token
        this.eat(TokenType.COLON) // 吞掉冒号
        val typeNode: ASTNode = this.typeDefinition() // 里面已经吞掉了类型关键字

        val parmsNodeList: List[ASTNode] = newParamTokensList.map(paramToken => ParameterNode(VariableNode(paramToken), typeNode))
        return parmsNodeList
    }

    def formalParametersList(): List[ASTNode] = {
        /*  formalParameterList : formalParameters | formalParameters SEMI formalParameterList */

        @scala.annotation.tailrec
        def loop(paramNodesList: List[ASTNode]): List[ASTNode] = this.currentToken.tokenType match {
            case TokenType.SEMICONLON =>
                this.eat(TokenType.SEMICONLON) // 吞掉分号
                val newParamNodesList: List[ASTNode] = paramNodesList ++ this.formalParameters()
                loop(newParamNodesList)
            case _ => paramNodesList
        }

        this.currentToken.tokenType match {
            case TokenType.IDENTIFIER =>
                val formalParameters: List[ASTNode] = this.formalParameters()
                loop(formalParameters)
            case _ =>
                return List[ASTNode]()
        }

    }

    def declarations(): List[ASTNode] = {
        /* declarations: (VAR (variable_declaration SEMI)+)* | (PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI)* | empty */
        @scala.annotation.tailrec
        def loopInter(declarationNodeList: List[ASTNode]): List[ASTNode] = this.currentToken.tokenType match {
            case TokenType.IDENTIFIER =>
                val newDeclarationNodes: List[ASTNode] = declarationNodeList ++ this.variableDeclaration()
                this.eat(TokenType.SEMICONLON) // 吞掉分号
                loopInter(newDeclarationNodes)
            case TokenType.PROCEDURE => 
                this.eat(TokenType.PROCEDURE) // 吞掉过程关键字
                val proceudreName: String = this.currentToken.tokenValue
                this.eat(TokenType.IDENTIFIER) // 吞掉过程名

                // val formalParameterList: List[ASTNode] = List()

                val formalParamList: List[ASTNode] = this.currentToken.tokenType match { // 判断是否有参数的括号，有括号则进入括号内处理，但是不一定有参数
                    case TokenType.LPAREN =>
                        this.eat(TokenType.LPAREN)
                        val newFormalParamList: List[ASTNode] = this.formalParametersList()
                        this.eat(TokenType.RPAREN)
                        newFormalParamList
                    case _ => null
                }

                this.eat(TokenType.SEMICONLON)  // 吞掉分号
                val blockNode: ASTNode = this.block()
                val newDeclarationNodes: List[ASTNode] = declarationNodeList :+ ProcedureDeclaratioNode(proceudreName, formalParamList, blockNode)
                this.eat(TokenType.SEMICONLON)
                loopInter(newDeclarationNodes)
            case _ => declarationNodeList
        }

        @scala.annotation.tailrec
        def loopOuter(declarationNodeList: List[ASTNode]): List[ASTNode] = this.currentToken.tokenType match {
                case TokenType.VAR =>
                    this.eat(TokenType.VAR)
                    val newDecNodes = loopInter(declarationNodeList) // 递归获取同一行的所有变量声明
                    return loopOuter(newDecNodes)
                case TokenType.PROCEDURE =>
                    val newDecNodes = loopInter(declarationNodeList) // 递归获取同一行的所有变量声明
                    return loopOuter(newDecNodes)
                case _ => return declarationNodeList
        }

        val declarationNodes: List[ASTNode] = List()
        return loopOuter(declarationNodes)
    }

    // 为什么要用链表？因为无法确定同一行的变量声明有多少个，所以用链表来存储
    def variableDeclaration(): List[ASTNode] = {
        /* variable_declaration: ID (COMMA ID)* COLON type_spec */

        // 递归获取所有位于同一行的ID标识符节点
        @scala.annotation.tailrec
        def loop(variableNodes: List[ASTNode]): List[ASTNode] = this.currentToken.tokenType match {
            case TokenType.COMMA =>
                this.eat(TokenType.COMMA)
                val newVariableNodes: List[ASTNode] = variableNodes :+ VariableNode(this.currentToken)
                this.eat(TokenType.IDENTIFIER)
                loop(newVariableNodes)
            case _ => variableNodes
        }   

        val variableNodes: List[ASTNode] = List(VariableNode(this.currentToken)) // 第一个ID标识符
        this.eat(TokenType.IDENTIFIER) // 吞掉第一个ID标识符
        val newVariableNodes: List[ASTNode] = loop(variableNodes) // 将剩下的ID标识符节点加入到链表中
        this.eat(TokenType.COLON) // 吞掉冒号
        val typeNode: ASTNode = this.typeDefinition() // 获取类型节点
        val variableDeclarationNodes: List[ASTNode] = newVariableNodes.map(VarDeclarationNode(_, typeNode))
        return variableDeclarationNodes
    }

    def typeDefinition(): ASTNode = {
        /* type_spec: INTEGER | REAL */
        val token: Token = this.currentToken
        // println(token.tokenType)

        this.currentToken.tokenType match {
            case TokenType.INTEGER =>
                this.eat(TokenType.INTEGER)
                return TypeNode(token)
            case TokenType.REAL =>
                this.eat(TokenType.REAL)
                return TypeNode(token)
            case _ => this.syntaxError()
        }
    }



    def parse(): ASTNode = {
        val node = this.program() // 从顶部开始解析，递归向下
        
        // 解析完之后检查是否还有剩余的token，如果有则抛出异常
        this.currentToken.tokenType match {
            case TokenType.EOF => node  // 返回AST的根节点
            case _ => this.syntaxError()
        }
    }
}