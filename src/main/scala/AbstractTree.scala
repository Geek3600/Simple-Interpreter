package AbstractTree
import Token._
import Symbol._
import RelectionMacros._

object AbstractTreeLog 
{
    var isLogEnabled: Boolean = false
}

abstract class NodeVisitor
{
    def unknownError() = {
        throw new Exception("Unknown node type")
    }

    //TODO如何做到传进来的不是父类，可以表达具体的子类类型
    // 返回对应节点类型的visit方法
    def visit(node: ASTNode) = {
        // 根据类型创造方法名
        val method_name = "visit" + node.getClass.getSimpleName

        // 查找对应的方法
        val visitor = this.getClass.getDeclaredMethod(method_name, classOf[ASTNode])
        if (AbstractTreeLog.isLogEnabled) println(node.nodeName)

        val result = visitor.invoke(this, node).asInstanceOf[String]
        result
    }
}



abstract class ASTNode
{
    val nodeName: String = ""
}


class BinaryOperationNode(val left: ASTNode, val operator: Token, val right: ASTNode) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}


class NumberNode(val token: Token) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
    val value: AnyVal = this.token.tokenType match {
        case TokenType.INTEGER_CONSTANT => 
            this.token.tokenValue.toDouble // 统一数据类型
        case TokenType.REAL_CONSTANT => 
            this.token.tokenValue.toDouble // 统一数据类型
        case _ => throw new Exception("Unknown constant type")
    }
}

class UnaryOperationNode(val operator: Token, val right: ASTNode) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}
// 为什么要用链表？因为无法确定同一行的变量声明有多少个，所以用链表来存储
class CompoundStatementNode(val children: List[ASTNode]) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}

class AssignStatementNode(val left: ASTNode, val assign: Token, val right: ASTNode) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}

class VariableNode(val token: Token) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}
class TypeNode(val token: Token) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}

class EmptyOperationNode extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}

class ProgramNode(val name: String, val block: ASTNode) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}

class VarDeclarationNode(val varNode: ASTNode, val typeNode: ASTNode) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}

// 为什么要用链表？因为无法确定同一行的变量声明有多少个，所以用链表来存储
class BlockNode(val declarations: List[ASTNode], val compoundStatement: ASTNode) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}

class ParameterNode(val varNode: ASTNode, val typeNode: ASTNode) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}

// 不确定参数有几个，所以用链表来存储
// 形式参数表示过程声明时的参数
class ProcedureDeclarationNode(val name: String, val formalParameters: List[ASTNode], val block: ASTNode) extends ASTNode
{
    override val nodeName: String = this.getClass.getSimpleName
}

// 实际参数表示过程调用时，实际传入的参数
class ProcedureCallNode(val procedureName: String, val actualParameters: List[ASTNode], val token: Token) extends ASTNode
{
    var procedureSymbol: ProcedureSymbol = null
    override val nodeName: String = this.getClass.getSimpleName
}