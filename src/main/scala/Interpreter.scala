package Interpreter

import org.xml.sax.ext.LexicalHandler
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import Lexer._
import Parser._
import Token._
import AbstractTree._
import Symbol._
import SemanticAnalyzer._
import SourceToSourceCompiler._
import Error._
import CallStack._
import com.liangdp.graphviz4s.Digraph
import com.liangdp.graphviz4s.Graph
import scala.collection.mutable.Map
import ASTVisualizer._

object InterpreterLog {
    var isLogEnabled: Boolean = false
}
// 解释器
// 解析抽象语法树
// class Interpreter extends NodeVisitor
// {
//     // 全局变量存储器，存储变量名和他的值
//     val callStack: CallStack = CallStack()

//     def divisionZeroError() = {
//         throw new Exception("Division by zero")
//     }

//     def visitBinaryOperationNode(node: ASTNode): AnyVal = {
        
//         val newNode = node match {
//             case bin: BinaryOperationNode => bin
//             case _ => throw new Exception("Unknown node type")
//         }
//         newNode.operator.tokenType match {
//             case TokenType.PLUS => this.visit(newNode.left) + this.visit(newNode.right)
//             case TokenType.SUB => this.visit(newNode.left) - this.visit(newNode.right)
//             case TokenType.MUL => this.visit(newNode.left) * this.visit(newNode.right)
//             case TokenType.INTEGER_DIV => this.visit(newNode.left) / this.visit(newNode.right)
//             case TokenType.FLOAT_DIV => this.visit(newNode.left) / this.visit(newNode.right)
//             case _ => 0
//         }
//     }

//     def visitNumberNode(node: ASTNode): AnyVal = {

//         val newNode = node match {
//             case num: NumberNode => num
//             case _ => throw new Exception("Unknown node type")
//         }
//         newNode.value
//     }

    
//     def visitUnaryOperationNode(node: ASTNode): AnyVal = {

//         val newNode = node match {
//             case unary: UnaryOperationNode => unary
//             case _ => throw new Exception("Unknown node type")
//         }

//         newNode.operator.tokenType match {
//             case TokenType.PLUS => this.visit(newNode.right)
//             case TokenType.SUB  => -this.visit(newNode.right)
//             case _ => 0
//         }
//     }

//     def visitCompoundStatementNode(node: ASTNode): Unit = {
        
//         val newNode = node match {
//             case compound: CompoundStatementNode => compound
//             case _ => throw new Exception("Unknown node type")
//         }

//         newNode.children.foreach(this.visit(_))
//     }

//     // 变量赋值，将变量名和值存入符号表中
//     def visitAssignStatementNode(node: ASTNode): Unit = {
//         val newNode = node match {
//             case assign: AssignStatementNode => assign
//             case _ => throw new Exception("Unknown node type")
//         }

//         val variableName = newNode.left.asInstanceOf[VariableNode].token.tokenValue // 获取赋值语句左边的变量名
//         val variableValue = this.visit(newNode.right) // 获取赋值语句右边的值

//         val activeRecord: ActiveRecord = this.callStack.peek() // 获取当前的活动记录
//         activeRecord.setMember(variableName, variableValue) // 将变量名和值存入活动记录中
//     }

//     // 从符号表中查找变量的值
//     def visitVariableNode(node: ASTNode): AnyVal = {
//         val newNode = node match {
//             case variable: VariableNode => variable
//             case _ => throw new Exception("Unknown node type")
//         }

//         val variableName = newNode.token.tokenValue // 获取变量名

//         val activeRecord: ActiveRecord = this.callStack.peek() // 获取当前的活动记录
//         activeRecord.getMember(variableName) match { // 从符号表中查找变量的值
//             case Some(value) => value
//             case None => throw new Exception(s"$variableName is an undefined variable")
//         }
//     }
    
//     def visitProgramNode(node: ASTNode): Unit = {
//         val newNode = node match {
//             case program: ProgramNode => program
//             case _ => throw new Exception("Unknown node type")
//         }

//         val programName = newNode.name // 获取程序名
//         this.log(s"ENTER: PROGRAM $programName")
//         val activeRecord = ActiveRecord(programName, ActiveRecordType.PROGRAM, 1)

//         this.callStack.push(activeRecord)
//         this.log(this.callStack.toString)

//         this.visit(newNode.block)

//         this.log(s"LEAVE: PROGRAM $programName")
//         this.log(this.callStack.toString)
//         this.callStack.pop()
//     }

//     def visitBlockNode(node: ASTNode): Unit = {
//         val newNode = node match {
//             case block: BlockNode => block
//             case _ => throw new Exception("Unknown node type")
//         }
//         newNode.declarations.foreach(this.visit(_))
//         this.visit(newNode.compoundStatement)
//     }

//     def visitVarDeclarationNode(node: ASTNode): Unit = {
//         val newNode = node match {
//             case varDecl: VarDeclarationNode => varDecl
//             case _ => throw new Exception("Unknown node type")
//         }
//     }

//     def visitTypeNode(node: ASTNode): Unit = {
//         val newNode = node match {
//             case typeNode: TypeNode => typeNode
//             case _ => throw new Exception("Unknown node type")
//         }
//     }

//     def visitEmptyOperationNode(node: ASTNode): Unit = {
//     }



//     // 过程声明不需要解释
//     def visitProcedureDeclarationNode(node: ASTNode): Unit = {
//     }


//     def visitProcedureCallNode(node: ASTNode): Unit = {
//         val procedureCallNode = node match {
//             case procedureCall: ProcedureCallNode => procedureCall
//             case _ => throw new Exception("Unknown node type")
//         }

//         val procedureName: String = procedureCallNode.procedureName // 获取过程名
        
//         val procedureSymbol: ProcedureSymbol = procedureCallNode.procedureSymbol// 取出过程符号
//         val activeRecord = ActiveRecord(procedureName, ActiveRecordType.PROCEDURE, procedureSymbol.scopeLevel + 1) // 过程调用了，要创建活动记录
//         // println(procedureSymbol)
//         val formalParams: ListBuffer[Symbol] = procedureSymbol.parameters     // 获取过程声明时的形式参数
//         val actualParams: List[ASTNode] = procedureCallNode.actualParameters  // 获取过程调用时的实际传入的参数，都是一些表示式

//         for ((formalParamSymbol, actualParamNode) <- formalParams.zip(actualParams)) {
//             activeRecord.setMember(formalParamSymbol.symbolName, this.visit(actualParamNode)) // 解释传入参数的表达式，将值存入活动记录中
//         }

//         this.callStack.push(activeRecord) // 将活动记录压入调用栈

//         this.log("ENTER: PROCEDURE %s".format(procedureName))
//         this.log(this.callStack.toString)
//         this.visit(procedureSymbol.blockNode) // 解释过程体
//         this.log("LEAVE: PROCEDURE %s".format(procedureName))
//         this.log(this.callStack.toString)
//         this.callStack.pop() // 过程执行完毕，将活动记录出栈
//     }

//     def log(msg: String) = if (InterpreterLog.isLogEnabled) println(msg)

//     def interprete(rootNode: ASTNode) = {
//         this.visit(rootNode)
//     }

    
// }


object Main {

    def main(args: Array[String]): Unit = {
 //====================
        InterpreterLog.isLogEnabled = false
        SemanticAnalyzerLog.isLogEnabled = false
        SymbolLog.isLogEnabled = false
        AbstractTreeLog.isLogEnabled = false

        val text =  "program Main;procedure Alpha(a : integer; b : integer);var x : integer;begin x := (a + b ) * 2;end;begin { Main }Alpha(3 + 5, 7);  { procedure call }end.  { Main }"
        val lexer = Lexer(text)
        val parser = Parser(lexer)
        // val interpreter = Interpreter()
        val astVisualizer = ASTVisualizer()
        val astTree = parser.parse()
        // val semanticAnalyzer = SemanticAnalyzer()
        // semanticAnalyzer.visit(astTree)
        astVisualizer.view(astTree)

        // interpreter.interprete(astTree)

        // val sourceToSourceCompiler = SourceToSourceCompiler()
        // sourceToSourceCompiler.visit(astTree, isS2SCompile = true)
        // println(sourceToSourceCompiler.output)
//=====================
      
    }
}