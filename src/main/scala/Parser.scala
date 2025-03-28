package Parser
import Token._
import Lexer._
import AbstractTree._

// 语义分析，构造抽象语法树AST
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
            this.currentToken = this.lexer.getNextToken()
        }
        else {
            // println("syntaxError " + this.currentToken.tokenType + tokenType + this.currentToken.tokenValue)
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

    def declarations(): List[ASTNode] = {
        /* declarations: VAR (variable_declaration SEMICONLON)+ | empty */
        @scala.annotation.tailrec
        def loop(declarationNodeList: List[ASTNode]): List[ASTNode] = this.currentToken.tokenType match {
            case TokenType.IDENTIFIER =>
                val newDeclarationNodes: List[ASTNode] = declarationNodeList ++ this.variableDeclaration()
                this.eat(TokenType.SEMICONLON)
                loop(newDeclarationNodes)
            case _ => declarationNodeList
        }

        val declarationNodes: List[ASTNode] = List()
        this.currentToken.tokenType match {
            case TokenType.VAR =>
                this.eat(TokenType.VAR)
                return loop(declarationNodes)
            case _ => return declarationNodes
        }
    }

    def variableDeclaration(): List[ASTNode] = {
        /* variable_declaration: ID (COMMA ID)* COLON type_spec */

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
        val newVariableNodes: List[ASTNode] = variableNodes ++ loop(variableNodes)

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