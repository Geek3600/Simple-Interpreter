package Interpreter

// import org.xml.sax.ext.LexicalHandler
// import scala.util.Try
// import scala.util.Success
// import scala.util.Failure
// import scala.collection.mutable.Map
// import scala.collection.mutable.ListBuffer
// import Lexer._
// import Parser._
// import Token._
// import AbstractTree._
// import Symbol._
// import SemanticAnalyzer._
// import SourceToSourceCompiler._
// import Error._
// import CallStack._
// import com.liangdp.graphviz4s.Digraph
// import com.liangdp.graphviz4s.Graph
// import scala.collection.mutable.Map
// import ASTVisualizer._

// object InterpreterLog {
//     var isLogEnabled: Boolean = false
// }
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


// object Main {

//     def main(args: Array[String]): Unit = {
//  //====================
//         InterpreterLog.isLogEnabled = false
//         SemanticAnalyzerLog.isLogEnabled = false
//         SymbolLog.isLogEnabled = false
//         AbstractTreeLog.isLogEnabled = false

//         val text =  "program Main;procedure Alpha(a : integer; b : integer);var x : integer;begin x := (a + b ) * 2;end;begin { Main }Alpha(3 + 5, 7);  { procedure call }end.  { Main }"
//         val lexer = Lexer(text)
//         val parser = Parser(lexer)
//         // val interpreter = Interpreter()
//         val astVisualizer = ASTVisualizer()
//         val astTree = parser.parse()
//         // val semanticAnalyzer = SemanticAnalyzer()
//         // semanticAnalyzer.visit(astTree)
//         astVisualizer.view(astTree)

//         // interpreter.interprete(astTree)

//         // val sourceToSourceCompiler = SourceToSourceCompiler()
//         // sourceToSourceCompiler.visit(astTree, isS2SCompile = true)
//         // println(sourceToSourceCompiler.output)
// //=====================
      
//     }
// }

import org.bytedeco.llvm.LLVM._
import org.bytedeco.llvm.global.LLVM._
import java.nio.ByteBuffer
import org.bytedeco.javacpp.PointerPointer
object ASTToLLVM {
  def main(args: Array[String]): Unit = {
    // 初始化 LLVM
    LLVMInitializeCore(LLVMGetGlobalPassRegistry())
    LLVMInitializeNativeTarget()
    LLVMInitializeNativeAsmPrinter()

    // 创建上下文和模块
    val context = LLVMContextCreate()
    val module = LLVMModuleCreateWithNameInContext("myScalaCompiler", context)
    val builder = LLVMCreateBuilderInContext(context)

    // 示例 AST
    val ast = Program(List(
      Function("add", List(Param("a", "i32"), Param("b", "i32")), BinaryOp("+", Var("a"), Var("b")))
    ))

    // 转换 AST 到 LLVM IR
    generateIR(ast, module, builder, context)

    // 打印 IR
    LLVMDumpModule(module)

    // 写入文件（可选）
    val error: ByteBuffer = null
    LLVMPrintModuleToFile(module, "output.ll", error)

    // 清理资源
    LLVMDisposeBuilder(builder)
    LLVMDisposeModule(module)
    LLVMContextDispose(context)
  }

  def generateIR(ast: Program, module: LLVMModuleRef, builder: LLVMBuilderRef, context: LLVMContextRef): Unit = {
    ast.functions.foreach { func =>
      // 定义函数参数类型
      val paramTypes = func.params.map { param =>
        param.typ match {
        case "i32" => LLVMInt32TypeInContext(context)
        case _ => throw new RuntimeException(s"Unsupported type: ${param.typ}")
      }
    }.toArray[LLVMTypeRef]

      // 创建函数类型 (i32 (i32, i32))
      val returnType = LLVMInt32TypeInContext(context)
      val paramTypesPointer = new PointerPointer(paramTypes*) // 转换为 PointerPointer
        val funcType = LLVMFunctionType(
        returnType,           // 返回值类型
        paramTypesPointer,    // 参数类型指针数组
        paramTypes.length,    // 参数数量
        0                     // 非可变参数函数
    )

      // 创建函数
      val llvmFunc = LLVMAddFunction(module, func.name, funcType)

      // 创建基本块
      val entryBlock = LLVMAppendBasicBlockInContext(context, llvmFunc, "entry")
      LLVMPositionBuilderAtEnd(builder, entryBlock)

      // 获取参数
      val paramsMap = func.params.zipWithIndex.map { case (param, i) =>
        val llvmParam = LLVMGetParam(llvmFunc, i)
        LLVMSetValueName2(llvmParam, param.name, param.name.length.toLong)
        param.name -> llvmParam
      }.toMap

      // 生成函数体
      val result = generateExpr(func.body, paramsMap, builder, context)

      // 返回结果
      LLVMBuildRet(builder, result)
    }
  }

  def generateExpr(expr: Expr, params: Map[String, LLVMValueRef], builder: LLVMBuilderRef, context: LLVMContextRef): LLVMValueRef = expr match {
    case Var(name) =>
      params.getOrElse(name, throw new RuntimeException(s"Unknown variable: $name"))
    case Const(value) =>
      LLVMConstInt(LLVMInt32TypeInContext(context), value, 0)
    case BinaryOp("+", left, right) =>
      val leftVal = generateExpr(left, params, builder, context)
      val rightVal = generateExpr(right, params, builder, context)
      LLVMBuildAdd(builder, leftVal, rightVal, "addtmp")
    case _ =>
      throw new RuntimeException("Unsupported expression")
  }
}

// AST 定义
sealed trait AST
case class Program(functions: List[Function]) extends AST
case class Function(name: String, params: List[Param], body: Expr) extends AST
case class Param(name: String, typ: String) extends AST
case class BinaryOp(op: String, left: Expr, right: Expr) extends AST
case class Var(name: String) extends AST
case class Const(value: Int) extends AST
type Expr = AST