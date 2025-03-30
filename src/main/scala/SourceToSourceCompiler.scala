package SourceToSourceCompiler
// import Symbol._
// import AbstractTree._
object SourceToSourceCompiler
{

}

// class SourceToSourceCompiler extends NodeVisitor
// {
//     var currentScopeSymbolTable: ScopedSymbolTable = null
//     var output: String = "" 

//     def visitBlockNode(node: ASTNode): String = {
//         val blockNode = node match {
//             case block: BlockNode => block
//             case _ => throw new Exception("Unknown node type")
//         }

//         var results = List[String]()
//         for (declaration <- blockNode.declarations) {
//             val declarationString = this.visit(declaration)
//             results = results :+ declarationString
//         }

//         results = results :+ "\nbegin"
//         val compoundStatementString = "  " + this.visit(blockNode.compoundStatement)
//         results = results :+ compoundStatementString
//         results = results :+ "end"
//         results.mkString("\n")  // 用换行符将所有字符串连接起来
//     }


//     def visitProgramNode(node: ASTNode): Unit = {
//         val programNode = node match {
//             case program: ProgramNode => program
//             case _ => throw new Exception("Unknown node type")
//         }

//         val programName = programNode.name
//         var resultString = "program %s0;\n".format(programName)
//         val globalScopeSymbolTable = new ScopedSymbolTable("global", 1, this.currentScopeSymbolTable)
//         this.currentScopeSymbolTable = globalScopeSymbolTable
//         resultString = resultString + this.visit(programNode.block)
//         resultString = resultString + "."
//         resultString = resultString + "{END OF %s}".format(programName)
//         this.output = resultString
//         this.currentScopeSymbolTable = this.currentScopeSymbolTable.fatherScope
//     }

//     def visitCompoundStatementNode(node: ASTNode): String = {
//         val compoundStatementNode = node match {
//             case compound: CompoundStatementNode => compound
//             case _ => throw new Exception("Unknown node type")
//         }

//         var results = List[String]()
//         for (child <- compoundStatementNode.children) {
//             val childString = this.visit(child)
//             if (childString != "")  results = results :+ childString // 有可能是null
//         }
//         results.mkString("\n")  // 用换行符将所有字符串连接起来
//     }

//     def visitEmptyOperationNode(node: ASTNode): String = ""

//     def visitBinaryOperationNode(node: ASTNode): String = {
//         val binaryOperationNode = node match {
//             case binary: BinaryOperationNode => binary
//             case _ => throw new Exception("Unknown node type")
//         }

//         val leftString = this.visit(binaryOperationNode.left)
//         val rightString = this.visit(binaryOperationNode.right)
//         val operator = binaryOperationNode.operator.tokenType
//         "%s %s %s".format(leftString, operator, rightString)
//     }

//     def visitProcedureDeclarationNode(node: ASTNode): String = {
//         val procedureDeclarationNode = node match {
//             case procedure: ProcedureDeclarationNode => procedure
//             case _ => throw new Exception("Unknown node type")
//         }

//         val procedureName = procedureDeclarationNode.name
//         val procedureSymbol = new ProcedureSymbol(procedureName)
//         this.currentScopeSymbolTable.defineNewSymbol(procedureSymbol)

//         var resultString = "procedure %s%s".format(procedureName, this.currentScopeSymbolTable.scopeLevel)

//         val procedureScopeSymbolTable = new ScopedSymbolTable(
//             procedureName, 
//             this.currentScopeSymbolTable.scopeLevel + 1, 
//             this.currentScopeSymbolTable
//         )

//         this.currentScopeSymbolTable = procedureScopeSymbolTable

//         if (procedureDeclarationNode.parameters != null) resultString = resultString + "("

//         var formalParameters = List[String]()
//         for (parameter <- procedureDeclarationNode.parameters) {
//             // 将过程参数都加入符号表
//             val newParam = parameter.asInstanceOf[ParameterNode]
//             val paramTypeSymbol = this.currentScopeSymbolTable.lookupSymbol(newParam.typeNode.asInstanceOf[TypeNode].token.tokenValue)
//             val paramName = newParam.varNode.asInstanceOf[VariableNode].token.tokenValue
//             val paramSymbol = VariableSymbol(paramName, paramTypeSymbol)
//             this.currentScopeSymbolTable.defineNewSymbol(paramSymbol)
//             procedureSymbol.addParameter(paramSymbol)

//             formalParameters = formalParameters :+ "%s : %s".format(
//                 paramName + this.currentScopeSymbolTable.scopeLevel.toString, 
//                 paramTypeSymbol.symbolName
//             )
//         }

//         // TODO 过于指令式，优化成函数式风格
//         resultString = resultString + formalParameters.mkString(";")
//         if (procedureDeclarationNode.parameters!= null) resultString = resultString + ")"
//         resultString = resultString + ";" + "\n"
//         resultString = resultString + this.visit(procedureDeclarationNode.block)
//         resultString = resultString + "; {END OF %s}".format(procedureName)
//         resultString = resultString.split("\n").map("  " + _).mkString("\n")
//         this.currentScopeSymbolTable = this.currentScopeSymbolTable.fatherScope
//         resultString
//     }

//     def visitVarDeclarationNode(node: ASTNode): String = {
//         val varDeclarationNode = node match {
//             case varDecl: VarDeclarationNode => varDecl
//             case _ => throw new Exception("Unknown node type")
//         }

//         val varType = varDeclarationNode.typeNode.asInstanceOf[TypeNode].token.tokenValue
//         val typeSymbol = this.currentScopeSymbolTable.lookupSymbol(varType)
        
//         val varName = varDeclarationNode.varNode.asInstanceOf[VariableNode].token.tokenValue
        
//         this.currentScopeSymbolTable.lookupSymbol(varName, currentScopeOnly = true) match {
//             case symbol: Symbol => throw new Exception(s"Variable $varName already defined")
//             case null => ()
//         }

//         val valSymbol = VariableSymbol(varName, typeSymbol)
//         this.currentScopeSymbolTable.defineNewSymbol(valSymbol)
//         "   var %s : %s;".format(varName + this.currentScopeSymbolTable.scopeLevel.toString, varType)
//     }

//     def visitAssignStatementNode(node: ASTNode): String = {
//         val assignStatementNode = node match {
//             case assign: AssignStatementNode => assign
//             case _ => throw new Exception("Unknown node type")
//         }
//         val leftString = this.visit(assignStatementNode.left)
//         val rightString = this.visit(assignStatementNode.right)
//         "%s := %s".format(leftString, rightString)
//     }

//     def visitVariableNode(node: ASTNode): String = {
//         val variableNode = node match {
//             case variable: VariableNode => variable
//             case _ => throw new Exception("Unknown node type")
//         }
//         val variableName = variableNode.token.tokenValue
//         // println(this.currentScopeSymbolTable)
//         val variableSymbol = this.currentScopeSymbolTable.lookupSymbol(variableName)
//         "<%s:%s>".format(variableName + this.currentScopeSymbolTable.scopeLevel.toString, variableSymbol.symbolType.symbolName)
//     }
// }