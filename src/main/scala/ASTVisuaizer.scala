package ASTVisualizer
import AbstractTree._
import Symbol._
import com.liangdp.graphviz4s._
import com.liangdp.graphviz4s.Digraph
import scala.collection.mutable.Map



// 符号表构造器，通过遍历AST，构造符号表
class ASTVisualizer extends NodeVisitor
{
    val ASTree = new Graph("AbstractSyntaxTree")
    var index = 0

    def getNodeName(nodeName: String): String = {
        val wordToRemove = Seq[String]("Node", "Statement")
        val name = wordToRemove.foldLeft(nodeName)((current, word) => current.replace(word, "")).trim.replaceAll("\\s+", " ")
        this.index += 1
        "%s%s".format(name, this.index)
    }

    def visitProgramNode(node: ASTNode): String = {
        val programNode = node match {
            case program: ProgramNode => program
            case _ => throw new Exception("Unknown node type")
        }

        // val globalScopeSymbolTable = new ScopedSymbolTable("global", 1, this.currentScopeSymbolTable) // 创建全局符号表
        // this.currentScopeSymbolTable = globalScopeSymbolTable       // 将当前符号表设置为全局符号表
        val childNode: String = this.visit(programNode.block)

        val currentNode: String = this.getNodeName(programNode.getClass.getSimpleName) + s"_${programNode.name}"

        ASTree.node(currentNode)
        ASTree.node(childNode)
        ASTree.edge(currentNode, childNode)
        currentNode + s"_${programNode.name}"

        // this.currentScopeSymbolTable = this.currentScopeSymbolTable.fatherScope // 为什么要回退？
    }

    def visitBlockNode(node: ASTNode): String = {
        val blockNode = node match {
            case block: BlockNode => block
            case _ => throw new Exception("Unknown node type")
        }

        var childNodes1: List[String] = List()
        blockNode.declarations.foreach(node => {
            val childNode: String = this.visit(node)
            childNodes1 = childNodes1 :+ childNode
        })

        val currentNode = this.getNodeName(blockNode.getClass.getSimpleName)

        childNodes1.foreach(childNode => {
            ASTree.node(childNode)
            ASTree.edge(currentNode, childNode)
        })

        val childNode2: String = this.visit(blockNode.compoundStatement)
        ASTree.node(childNode2)
        ASTree.edge(currentNode, childNode2)
        currentNode
    }


    def visitBinaryOperationNode(node: ASTNode): String = {
        val binaryOperationNode = node match {
            case bin: BinaryOperationNode => bin
            case _ => throw new Exception("Unknown node type")
        }

        val childNodeLeft: String = this.visit(binaryOperationNode.left)
        val childNodeRight: String = this.visit(binaryOperationNode.right)
        val currentNode = this.getNodeName(binaryOperationNode.getClass.getSimpleName)
        ASTree.node(childNodeLeft)
        ASTree.node(childNodeRight)
        ASTree.node(currentNode)
        ASTree.edge(currentNode, childNodeLeft)
        ASTree.edge(currentNode, childNodeRight)
        currentNode
    }
    
    def visitNumberNode(node: ASTNode): String = {
        val numberNode = node match {
            case number: NumberNode => number
            case _ => throw new Exception("Unknown node type")
        }
        this.getNodeName(numberNode.getClass.getSimpleName) + s"_${numberNode.token.tokenValue}"
    }

    def visitUnaryOperationNode(node: ASTNode): String = {
        val unaryOperationNode = node match {
            case unary: UnaryOperationNode => unary
            case _ => throw new Exception("Unknown node type")
        }

        val childNode: String = this.visit(unaryOperationNode.right)
        val currentNode = this.getNodeName(unaryOperationNode.getClass.getSimpleName)
        ASTree.node(childNode)
        ASTree.node(currentNode)
        ASTree.edge(currentNode, childNode)
        currentNode
    }


    def visitCompoundStatementNode(node: ASTNode): String = {
        val compoundStatementNode = node match {
            case compound: CompoundStatementNode => compound
            case _ => throw new Exception("Unknown node type")
        }

        var childNodes: List[String] = List()
        compoundStatementNode.children.foreach(node => {
            val childNode: String = this.visit(node)
            childNodes = childNodes :+ childNode
        })
        val currentNode = this.getNodeName(compoundStatementNode.getClass.getSimpleName)
        childNodes.foreach(childNode => {
            ASTree.node(childNode)
            ASTree.edge(currentNode, childNode)
        })
        currentNode
    }


    def visitEmptyOperationNode(node: ASTNode): String = {
        val emptyOperationNode = node match {
            case empty: EmptyOperationNode => empty
            case _ => throw new Exception("Unknown node type")
        }
        this.getNodeName(emptyOperationNode.getClass.getSimpleName)
    }

    
    //TODO:: 把各个visit中的newNode改成对应类型的Node名称
    def visitVarDeclarationNode(node: ASTNode): String = {
        val varDeclarationNode = node match {
            case varDecl: VarDeclarationNode => varDecl
            case _ => throw new Exception("Unknown node type")
        }
        this.getNodeName(varDeclarationNode.getClass.getSimpleName)
        // // 提取变量类型名
        // val varType = varDeclarationNode.typeNode.asInstanceOf[TypeNode].token.tokenValue
        // // 根据类型名，创建类型符号
        // val typeSymbol = BuiltInTypeSymbol(varType)

        // // 提取变量名
        // val varName = varDeclarationNode.varNode.asInstanceOf[VariableNode].token.tokenValue
        
        // // 检查变量名是否已经存在，如果存在，则重复定义
        // this.currentScopeSymbolTable.lookupSymbol(varName, currentScopeOnly = true) match {
        //     case symbol: Symbol => this.semanticError(ErrorCode.DUPLICATE_ID, varDeclarationNode.varNode.asInstanceOf[VariableNode].token)
        //     case null => ()
        // }

        // // 根据变量名和类型符号，创建变量符号
        // val varSymbol = VariableSymbol(varName, typeSymbol)
        // // 将变量符号存入符号表中
        // this.currentScopeSymbolTable.defineNewSymbol(varSymbol)
    }

    def visitAssignStatementNode(node: ASTNode): String = {
        val assignStatementNode = node match {
            case assign: AssignStatementNode => assign
            case _ => throw new Exception("Unknown node type")
        }
        
        val currentNode = this.getNodeName(assignStatementNode.getClass.getSimpleName)
        ASTree.node(currentNode)
       
        // 访问赋值号右边的节点
        val leftString = this.visit(assignStatementNode.left)
        ASTree.node(leftString)
        ASTree.edge(currentNode, leftString)
        
        // 访问赋值号左边的节点
        val rightString = this.visit(assignStatementNode.right)
        ASTree.node(rightString)
        ASTree.edge(currentNode, rightString)

        currentNode
    }

    def visitVariableNode(node: ASTNode): String = {
        val variableNode = node match {
            case variable: VariableNode => variable
            case _ => throw new Exception("Unknown node type")
        }
        // println(this.getNodeName(variableNode.getClass.getSimpleName) + "\n" + s"${variableNode.token.tokenValue}")
        this.getNodeName(variableNode.getClass.getSimpleName) +  s"_${variableNode.token.tokenValue}"
        // 从符号表中查找变量，检查其是否存在
        // this.currentScopeSymbolTable.lookupSymbol(variableNode.token.tokenValue, currentScopeOnly = false) match {
        //     case symbol: Symbol => ()
        //     case null => this.semanticError(ErrorCode.ID_NOT_FOUND, variableNode.token)
        // }
    }


    // 访问过程节点
    // 创建过程符号，将过程符号保存到当前scope的符号表中
    // 创建一个更深scope的符号表，把它作为当前的scope
    // 将过程参数保存到该符号表中
    def visitProcedureDeclarationNode(node: ASTNode): String = {
        val procedureDeclarationNode = node match {
            case procedure: ProcedureDeclarationNode => procedure
            case _ => throw new Exception("Unknown node type")
        }
        val currentNode = this.getNodeName(procedureDeclarationNode.getClass.getSimpleName) + s"_${procedureDeclarationNode.name}"

        val childNodes: List[String] = procedureDeclarationNode.formalParameters.map(node => {
            val childNode = this.visit(node)
            childNode
        })

        childNodes.foreach(childNode => {
            ASTree.node(childNode)
            ASTree.edge(currentNode, childNode)
        })

        val childString =this.visit(procedureDeclarationNode.block)
        ASTree.node(childString)
        ASTree.node(currentNode)
        ASTree.edge(currentNode, childString)
        currentNode
        // 提取过程名
        // val procedureName = procedureDeclarationNode.name

        // // 创建过程符号
        // val procedureSymbol = ProcedureSymbol(procedureName)
        // procedureSymbol.blockNode = procedureDeclarationNode.block // 将过程体节点保存到过程符号中，方便解释器找到过程体

        // // 将过程符号存入符号表中
        // this.currentScopeSymbolTable.defineNewSymbol(procedureSymbol)

        // val procedureScopeSymbolTable = new ScopedSymbolTable(
        //     procedureName, 
        //     this.currentScopeSymbolTable.scopeLevel + 1, 
        //     this.currentScopeSymbolTable)
        
        // // 进入更深一级的scope
        // this.currentScopeSymbolTable = procedureScopeSymbolTable

        // // 将全部参数节点，提取他们的符号，存入符号表中
        // for (parameter <- procedureDeclarationNode.formalParameters) {
        //     val newParam = parameter.asInstanceOf[ParameterNode]
        //     val paramTypeSymbol = this.currentScopeSymbolTable.lookupSymbol(newParam.typeNode.asInstanceOf[TypeNode].token.tokenValue, currentScopeOnly = false)
        //     val paramName: String = newParam.varNode.asInstanceOf[VariableNode].token.tokenValue
        //     val paramSymbol = VariableSymbol(paramName, paramTypeSymbol)
        //     this.currentScopeSymbolTable.defineNewSymbol(paramSymbol)
        //     procedureSymbol.addParameter(paramSymbol)
        // }

        // this.currentScopeSymbolTable = this.currentScopeSymbolTable.fatherScope
        
    }

    def visitProcedureCallNode(node: ASTNode): String = {
        val procedureCallNode = node match {
            case procedureCall: ProcedureCallNode => procedureCall
            case _ => throw new Exception("Unknown node type")
        }

        var childNodes: List[String] = List()
        procedureCallNode.actualParameters.foreach(node => {
            val childNode = this.visit(node)
            childNodes = childNodes :+ childNode
        }) // 递归访问参数节点，计入符号表

        val currentNode = this.getNodeName(procedureCallNode.getClass.getSimpleName) + s"_${procedureCallNode.procedureName}"
        childNodes.foreach(childNode => {
            ASTree.node(childNode)
            ASTree.edge(currentNode, childNode)
        })
        currentNode
        // // 找出过程中的符号，里面有参数
        // val procedureSymbol = this.currentScopeSymbolTable.lookupSymbol(procedureCallNode.procedureName) 
        // procedureCallNode.procedureSymbol = procedureSymbol.asInstanceOf[ProcedureSymbol]
    }
    def visitParameterNode(node: ASTNode): String = {
        val parameterNode = node match {
            case procedureCall: ParameterNode => procedureCall
            case _ => throw new Exception("Unknown node type")
        }
        val currentNode = this.getNodeName(parameterNode.getClass.getSimpleName)
        val childNode1 = this.visit(parameterNode.varNode)
        val childNode2 = this.visit(parameterNode.typeNode)
        // println()
        ASTree.node(childNode1)
        ASTree.node(childNode2)
        ASTree.node(currentNode)
        ASTree.edge(currentNode, childNode1)
        ASTree.edge(currentNode, childNode2)
        currentNode
    }

    def visitTypeNode(node: ASTNode): String = {
        val typeNode = node match {
            case typeNode: TypeNode => typeNode
            case _ => throw new Exception("Unknown node type")
        }
        this.getNodeName(typeNode.getClass.getSimpleName) + s"_${typeNode.token.tokenValue.toUpperCase}"
    }

    def view(rootNode: ASTNode): Unit = {
        this.visit(rootNode)
        this.ASTree.view(fileName = "astree.gv", directory = ".")
    }

}

