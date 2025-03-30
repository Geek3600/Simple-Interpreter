package Symbol
import Token._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import AbstractTree._

object SymbolLog {
    var isLogEnabled: Boolean = false
}
// 为何要追踪符号
// 1. 为了确保将一个值赋值给变量时，类型正确
// 2. 为了确保变量在使用之前已经被声明
abstract class Symbol(val symbolName: String, val symbolType: Symbol = null)
{
    var scopeLevel: Int = 0
    def str(): String 
}

// 内建的类型符号，只有符号名
class BuiltInTypeSymbol(override val symbolName: String) extends Symbol(symbolName) {
    def str(): String = this.symbolName
    override def toString(): String = "<BuiltInTypeSymbol(name='%s)>".format(this.symbolName.toUpperCase)
}

// 变量符号，有变量名和变量类型，不保存变量的值
// symbolType肯定是BuiltInTypeSymbol类型
class VariableSymbol(override val symbolName: String, override val symbolType: Symbol) extends Symbol(symbolName, symbolType) {
    def str(): String = {
        // println("VariableSymbol: " + this.symbolName + " ")

        val _type: String = this.symbolType.symbolName.toUpperCase
        s"<VariableSymbol(name='$symbolName', type='$_type')>"
    }

    override def toString(): String = str()
}

// 一个过程符号中包含的内容有：
// 1. 过程名
// 2. 过程所有参数符号
// 3. 过程体节点：用于查找过程体的位置
class ProcedureSymbol(override val symbolName: String, val parameters: ListBuffer[Symbol] = ListBuffer[Symbol]()) extends Symbol(symbolName) {
    
    var blockNode: ASTNode = null

    def str(): String = {
        val _parameters: String = parameters.map(_.toString()).mkString(", ")
        s"<ProcedureSymbol(name='$symbolName', params='$_parameters')>"
    }
    
    def addParameter(parameter: Symbol): Unit = {
        this.parameters.append(parameter)
    }

    override def toString(): String = str()
}

// 符号表
// key是符号名，value是符号类
class ScopedSymbolTable(val scopeName: String, val scopeLevel: Int, val fatherScope: ScopedSymbolTable = null)
{   
    val symbolTable: Map[String, Symbol] = Map()
    this.defineNewSymbol(BuiltInTypeSymbol("integer"))
    this.defineNewSymbol(BuiltInTypeSymbol("real"))

    def str() = {
        val symbolDisplay: Map[String, String] = symbolTable.map {case (key, value) => (key, value.toString())}
        val allHeader       = "\n\nSCOPE (SCOPED SYMBOL TABLE)\n"
        val head1           = "%-15s: %s\n".format("Scope name", this.scopeName)
        val head2           = "%-15s: %s\n".format("Scope level", this.scopeLevel)
        val head3           = this.fatherScope match {
            case null => "%-15s: %s\n".format("Enclosing scope", "None")
            case _    => "%-15s: %s\n".format("Enclosing scope", this.fatherScope.scopeName)
        }
        val serperator1     = "===========================\n"
        val serperator2     = "Scope (Scoped symbol table) contents\n"
        // val scopeHeader     = "                       Symbol Table                   \n"
        val topSeparator    = "-----------------------------------------------------------\n"
        val bottomSeparator = "-----------------------------------------------------------\n"
        val formattedRows   = symbolDisplay.map { case (key, value) => "%7s : %s".format(key, value)}.mkString("", "\n", "\n")
        // TODO优化
        allHeader+serperator1+head1+head2+head3+serperator2+ topSeparator + formattedRows + bottomSeparator
    }

    override def toString(): String = str()

    // 定义新的符号，将符号名和符号类存入符号表中
    def defineNewSymbol(symbol: Symbol) = {
        this.log("Define new symbol: %s (Scope name: %s)".format(symbol.symbolName, this.scopeName))
        symbol.scopeLevel = this.scopeLevel
        this.symbolTable(symbol.symbolName) = symbol
    }

    // 根据符号名查找符号表
    def lookupSymbol(symbolName: String, currentScopeOnly: Boolean = false): Symbol = {
        this.log("Lookup symbol: %s (Scope name: %s)".format(symbolName, this.scopeName))
        this.symbolTable.get(symbolName) match { // 查找当前作用域的符号表
            case Some(symbol) => symbol
            case None => 
                if (currentScopeOnly) null
                else {
                    this.fatherScope match { // 若当前作用域不存在，则递归向上查找父作用域的符号表
                        case null => null
                        case _    => this.fatherScope.lookupSymbol(symbolName, false)
                    }
                }
        }
    }

    def log(msg: String) = if (SymbolLog.isLogEnabled) println(msg)
}

