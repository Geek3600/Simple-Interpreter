package Interpreter

import org.xml.sax.ext.LexicalHandler
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.collection.mutable.Map
import Lexer._
import Parser._
import Token._
import AbstractTree._


// 解释器
// 解析抽象语法树
class Interpreter(val parser: Parser) extends NodeVisitor
{
    val symbleTable: Map[String, AnyVal] = Map()

    def divisionZeroError() = {
        throw new Exception("Division by zero")
    }

    def visitBinaryOperationNode(node: ASTNode): AnyVal = {
        
        val newNode = node match {
            case bin: BinaryOperationNode => bin
            case _ => throw new Exception("Unknown node type")
        }
        // println(newNode.operator.tokenType)
        newNode.operator.tokenType match {
            case TokenType.PLUS => this.visit(newNode.left) + this.visit(newNode.right)
            case TokenType.SUB => this.visit(newNode.left) - this.visit(newNode.right)
            case TokenType.MUL => this.visit(newNode.left) * this.visit(newNode.right)
            case TokenType.INTEGER_DIV => this.visit(newNode.left) / this.visit(newNode.right)
            case TokenType.FLOAT_DIV => this.visit(newNode.left) / this.visit(newNode.right)
            case _ => 0
        }
    }

    def visitNumberNode(node: ASTNode): AnyVal = {

        val newNode = node match {
            case num: NumberNode => num
            case _ => throw new Exception("Unknown node type")
        }
        newNode.value
    }

    
    def visitUnaryOperationNode(node: ASTNode): AnyVal = {

        val newNode = node match {
            case unary: UnaryOperationNode => unary
            case _ => throw new Exception("Unknown node type")
        }

        newNode.operator.tokenType match {
            case TokenType.PLUS => this.visit(newNode.right)
            case TokenType.SUB  => -this.visit(newNode.right)
            case _ => 0
        }
    }

    def visitCompoundStatementNode(node: ASTNode): Unit = {
        
        val newNode = node match {
            case compound: CompoundStatementNode => compound
            case _ => throw new Exception("Unknown node type")
        }

        newNode.children.foreach(this.visit(_))
    }

    // 变量赋值，将变量名和值存入符号表中
    def visitAssignStatementNode(node: ASTNode): Unit = {
        val newNode = node match {
            case assign: AssignStatementNode => assign
            case _ => throw new Exception("Unknown node type")
        }

        val variableName = newNode.left.asInstanceOf[VariableNode].token.tokenValue // 获取赋值语句左边的变量名
        val v = this.visit(newNode.right) // 获取赋值语句右边的值
        symbleTable(variableName) = v // 获取赋值语句右边的值，存到符号表中
    }

    // 从符号表中查找变量的值
    def visitVariableNode(node: ASTNode): AnyVal = {
        val newNode = node match {
            case variable: VariableNode => variable
            case _ => throw new Exception("Unknown node type")
        }

        val variableName = newNode.token.tokenValue // 获取变量名
        symbleTable.get(variableName) match { // 从符号表中查找变量的值
            case Some(value) => value
            case None => throw new Exception(s"$variableName is an undefined variable")
        }
    }
    
    def visitProgramNode(node: ASTNode): Unit = {
        val newNode = node match {
            case program: ProgramNode => program
            case _ => throw new Exception("Unknown node type")
        }
        this.visit(newNode.block)
    }

    def visitBlockNode(node: ASTNode): Unit = {
        val newNode = node match {
            case block: BlockNode => block
            case _ => throw new Exception("Unknown node type")
        }
        newNode.declarations.foreach(this.visit(_))
        this.visit(newNode.compoundStatement)
    }

    def visitVarDeclarationNode(node: ASTNode): Unit = {
        val newNode = node match {
            case varDecl: VarDeclarationNode => varDecl
            case _ => throw new Exception("Unknown node type")
        }
    }

    def visitTypeNode(node: ASTNode): Unit = {
        val newNode = node match {
            case typeNode: TypeNode => typeNode
            case _ => throw new Exception("Unknown node type")
        }
    }

    def visitEmptyOperationNode(node: ASTNode): Unit = {
    }

    def interprete() = {
        val rootNode = this.parser.parse()
        this.visit(rootNode)
    }
}


object Main {

    def main(args: Array[String]): Unit = {
        // while (true) {
        //     val line: Try[String] = Try(scala.io.StdIn.readLine("calc> "))

        //     line match {
        //         case Success(value) => {
        //             val lexer = new Lexer(value)
        //             val parser = new Parser(lexer)
        //             val interpreter = new Interpreter(parser)
        //             val result = interpreter.interprete()
        //             println(result)
        //         }
        //         case Failure(_) => {
        //             return
        //         }
        //     }
        // }
        // val text =  "BEGIN  BEGIN    number := 2;    a := number;    b := 10 * a + 10 * number / 4;    c := a - - b  END;  x := 11;END."
        // println(text)
        val text = "PROGRAM Part10AST;VAR a, b : INTEGER;y    : REAL;BEGIN {Part10AST}a := 2;b := 10 * a + 10 * a DIV 4;y := 20 / 7 + 3.14; END.  {Part10AST}"
        val lexer = new Lexer(text)
        val parser = new Parser(lexer)
        val interpreter = new Interpreter(parser)
        interpreter.interprete()
        interpreter.symbleTable.foreach {case (key, value) => println(s"$key  $value")}

    }
}