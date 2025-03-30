package CallStack
import scala.collection.mutable.Stack
import scala.collection.mutable.Map

object ActiveRecordType
{
    val PROGRAM    = "PROGRAM"
    val PROCEDURE  = "PROCEDURE "
}

class ActiveRecord(val name: String, val arType: String, val nestingLevel: Int)
{
    var memory: Map[String, Double] = Map()

    def setMember(key: String, value: Double): Unit = {
        this.memory(key) = value
    }

    def getMember(key: String): Option[Double] = {
        this.memory.get(key)
    }

    def str(): String = {
        val header1: String = "%s: %s %s\n".format(this.nestingLevel, this.arType, this.name)
        val contents: String = this.memory.map {case(key, value) => "%s: %s".format(key, value.toString)}.mkString("\n")
        header1 + contents
    }

    override def toString() = str()

}

class CallStack
{
    val activeRecordStack = Stack[ActiveRecord]()

    def push(activeRecord: ActiveRecord): Unit = {
        this.activeRecordStack.push(activeRecord)
    } 

    def pop(): ActiveRecord = {
        this.activeRecordStack.pop()
    }

    // 取出栈顶的活动记录，但是不出栈
    def peek(): ActiveRecord = {
        this.activeRecordStack.top
    }

    // 打印调用栈中的内容
    def str(): String = {
        this.activeRecordStack.mkString("CallStack:\n", "\n", "")
    }

    override def toString() = str()
}