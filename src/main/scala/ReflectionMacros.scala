package RelectionMacros
import scala.quoted._


object RuntimeReflect {
    inline def printObjectInfo[T](obj: T): Unit = ${ printObjectInfoImpl('obj) }

    def printObjectInfoImpl[T: Type](objExpr: Expr[T])(using Quotes): Expr[Unit] = {
        import quotes.reflect._

        val tpe = TypeRepr.of[T] // 获取类型的表示
        val symbol = tpe.typeSymbol // 获取类型的符号

        // 获取所有字段
        val fields = symbol.caseFields.map { field =>
            val fieldName = field.name
            val fieldType = field.tree match {
            case ValDef(_, tpt, _) => tpt.tpe.show
            case _ => "<unknown>"
            }
            s"$fieldName: $fieldType"
        }

        println(s"${symbol.name}(${fields.mkString(", ")})")

        '{ () } // 返回空表达式
    }
}
