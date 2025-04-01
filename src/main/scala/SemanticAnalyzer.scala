package SemanticAnalyzer
import AbstractTree._
import Lexer._
import Token._
import scala.collection.mutable.Map
import Symbol._
import Error._

object SemanticAnalyzerLog {
    var isLogEnabled: Boolean = false
}
// 符号表构造器，通过遍历AST，构造符号表
class SemanticAnalyzer extends NodeVisitor
{
    // val globalScopeSymbolTable = new ScopedSymbolTable(scopeName = "global", scopeLevel = 1)
    var currentScopeSymbolTable: ScopedSymbolTable = null

    def semanticError(errorCode: String, token: Token): Unit = {
        // println(token.tokenType + " " + token.tokenValue)
        val errorString = "%s -> %s".format(errorCode, token)
        val semanticError = new SemanticError(errorCode, token, errorString)
        throw new Exception(semanticError.errorMessage)
    }

    def visitBlockNode(node: ASTNode): Unit = {
        val newNode = node match {
            case block: BlockNode => block
            case _ => throw new Exception("Unknown node type")
        }
        newNode.declarations.foreach(this.visit(_))
        this.visit(newNode.compoundStatement)
    }

    def visitProgramNode(node: ASTNode): Unit = {
        val newNode = node match {
            case program: ProgramNode => program
            case _ => throw new Exception("Unknown node type")
        }

        this.log("ENTER scope: global")
        val globalScopeSymbolTable = new ScopedSymbolTable("global", 1, this.currentScopeSymbolTable) // 创建全局符号表
        this.currentScopeSymbolTable = globalScopeSymbolTable       // 将当前符号表设置为全局符号表
        this.visit(newNode.block)

        this.log(globalScopeSymbolTable.str())

        this.currentScopeSymbolTable = this.currentScopeSymbolTable.fatherScope // 为什么要回退？
        this.log("LEAVE scope: global")
    }

    def visitBinaryOperationNode(node: ASTNode): Unit = {
        val newNode = node match {
            case bin: BinaryOperationNode => bin
            case _ => throw new Exception("Unknown node type")
        }
        this.visit(newNode.left)
        this.visit(newNode.right)
    }

    
    def visitNumberNode(node: ASTNode): Unit = {
    }

    def visitUnaryOperationNode(node: ASTNode): Unit = {
        val newNode = node match {
            case unary: UnaryOperationNode => unary
            case _ => throw new Exception("Unknown node type")
        }
        this.visit(newNode.right)
    }

    def visitCompoundStatementNode(node: ASTNode): Unit = {
        val newNode = node match {
            case compound: CompoundStatementNode => compound
            case _ => throw new Exception("Unknown node type")
        }
        newNode.children.foreach(this.visit(_))
    }

    def visitEmptyOperationNode(node: ASTNode): Unit = {
    }

    //TODO:: 把各个visit中的newNode改成对应类型的Node名称
    def visitVarDeclarationNode(node: ASTNode): Unit = {
        val newNode = node match {
            case varDecl: VarDeclarationNode => varDecl
            case _ => throw new Exception("Unknown node type")
        }

        // 提取变量类型名
        val varType = newNode.typeNode.asInstanceOf[TypeNode].token.tokenValue
        // 根据类型名，创建类型符号
        val typeSymbol = BuiltInTypeSymbol(varType)

        // 提取变量名
        val varName = newNode.varNode.asInstanceOf[VariableNode].token.tokenValue
        
        // 检查变量名是否已经存在，如果存在，则重复定义
        this.currentScopeSymbolTable.lookupSymbol(varName, currentScopeOnly = true) match {
            case symbol: Symbol => this.semanticError(ErrorCode.DUPLICATE_ID, newNode.varNode.asInstanceOf[VariableNode].token)
            case null => ()
        }

        // 根据变量名和类型符号，创建变量符号
        val varSymbol = VariableSymbol(varName, typeSymbol)
        // 将变量符号存入符号表中
        this.currentScopeSymbolTable.defineNewSymbol(varSymbol)
    }

    def visitAssignStatementNode(node: ASTNode): Unit = {
        val newNode = node match {
            case assign: AssignStatementNode => assign
            case _ => throw new Exception("Unknown node type")
        }
        
        // 访问赋值号右边的节点
        this.visit(newNode.right)
         // 访问赋值号左边的节点
        this.visit(newNode.left)

    }

    def visitVariableNode(node: ASTNode): Unit = {
        val variableNode = node match {
            case variable: VariableNode => variable
            case _ => throw new Exception("Unknown node type")
        }
        // 从符号表中查找变量，检查其是否存在
        this.currentScopeSymbolTable.lookupSymbol(variableNode.token.tokenValue, currentScopeOnly = false) match {
            case symbol: Symbol => ()
            case null => this.semanticError(ErrorCode.ID_NOT_FOUND, variableNode.token)
        }
    }


    // 访问过程节点
    // 创建过程符号，将过程符号保存到当前scope的符号表中
    // 创建一个更深scope的符号表，把它作为当前的scope
    // 将过程参数保存到该符号表中
    def visitProcedureDeclarationNode(node: ASTNode): Unit = {
        val newNode = node match {
            case procedure: ProcedureDeclarationNode => procedure
            case _ => throw new Exception("Unknown node type")
        }

        // 提取过程名
        val procedureName = newNode.name

        // 创建过程符号
        val procedureSymbol = ProcedureSymbol(procedureName)
        procedureSymbol.blockNode = newNode.block // 将过程体节点保存到过程符号中，方便解释器找到过程体

        // 将过程符号存入符号表中
        this.currentScopeSymbolTable.defineNewSymbol(procedureSymbol)
        this.log("Enter scope: %s".format(procedureName))

        val procedureScopeSymbolTable = new ScopedSymbolTable(
            procedureName, 
            this.currentScopeSymbolTable.scopeLevel + 1, 
            this.currentScopeSymbolTable)
        
        // 进入更深一级的scope
        this.currentScopeSymbolTable = procedureScopeSymbolTable

        // 将全部参数节点，提取他们的符号，存入符号表中
        for (parameter <- newNode.formalParameters) {
            val newParam = parameter.asInstanceOf[ParameterNode]
            val paramTypeSymbol = this.currentScopeSymbolTable.lookupSymbol(newParam.typeNode.asInstanceOf[TypeNode].token.tokenValue, currentScopeOnly = false)
            val paramName: String = newParam.varNode.asInstanceOf[VariableNode].token.tokenValue
            val paramSymbol = VariableSymbol(paramName, paramTypeSymbol)
            this.currentScopeSymbolTable.defineNewSymbol(paramSymbol)
            procedureSymbol.addParameter(paramSymbol)
        }

        this.visit(newNode.block)
        this.log(procedureScopeSymbolTable.str())
        this.currentScopeSymbolTable = this.currentScopeSymbolTable.fatherScope
        this.log("LEAVE scope: %s".format(procedureName))
        
    }

    def visitProcedureCallNode(node: ASTNode): Unit = {
        val procedureCallNode = node match {
            case procedureCall: ProcedureCallNode => procedureCall
            case _ => throw new Exception("Unknown node type")
        }

        procedureCallNode.actualParameters.foreach(this.visit(_)) // 递归访问参数节点，计入符号表

        // 找出过程中的符号，里面有参数
        val procedureSymbol = this.currentScopeSymbolTable.lookupSymbol(procedureCallNode.procedureName) 
        procedureCallNode.procedureSymbol = procedureSymbol.asInstanceOf[ProcedureSymbol]
    }
    def visitParameterNode(node: ASTNode) = {}
    def log(msg: String) = if (SemanticAnalyzerLog.isLogEnabled) println(msg)
}