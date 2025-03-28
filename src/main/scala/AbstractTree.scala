package AbstractTree
import Token._

abstract class NodeVisitor
{
    def unknownError() = {
        throw new Exception("Unknown node type")
    }

    // 返回对应节点类型的visit方法
    def visit(node: ASTNode) = {
        // 根据类型创造方法名
        val method_name = "visit" + node.getClass.getSimpleName

        // 查找对应的方法
        val visitor = this.getClass.getDeclaredMethod(method_name, classOf[ASTNode])
        val result = visitor.invoke(this, node).asInstanceOf[Double]
        result
    }
}

class AST


abstract class ASTNode


class BinaryOperationNode(val left: ASTNode, val operator: Token, val right: ASTNode) extends ASTNode


class NumberNode(val token: Token) extends ASTNode
{
    val value: AnyVal = this.token.tokenType match {
        case TokenType.INTEGER_CONSTANT => 
            // this.token.tokenValue.toInt
            this.token.tokenValue.toDouble // 统一数据类型
        case TokenType.REAL_CONSTANT => 
            this.token.tokenValue.toDouble // 统一数据类型
            // this.token.tokenValue.toFloat
        case _ => throw new Exception("Unknown constant type")
    }
}

class UnaryOperationNode(val operator: Token, val right: ASTNode) extends ASTNode


class CompoundStatementNode(val children: List[ASTNode]) extends ASTNode

class AssignStatementNode(val left: ASTNode, val assign: Token, val right: ASTNode) extends ASTNode

class VariableNode(val token: Token) extends ASTNode

class EmptyOperationNode extends ASTNode

class ProgramNode(val name: String, val block: ASTNode) extends ASTNode

class VarDeclarationNode(val varNode: ASTNode, val typeNode: ASTNode) extends ASTNode

class BlockNode(val declarations: List[ASTNode], val compoundStatement: ASTNode) extends ASTNode

class TypeNode(val token: Token) extends ASTNode