package type_tests

import spinal.core._
import spinal.core.internals._

class DebugTypeInfo(message : String) extends Phase {
  override def impl(pc: PhaseContext) = {
    println(message)

    recComponent(pc.topLevel)

    def recComponent(c: Component): Unit = {
      println(
        s"""Component ${c.name}
           | classname: "${c.getClass.getName}"
           | location: ${c.getScalaLocationShort}
           |""".stripMargin.stripIndent())

      c.children.foreach(recComponent)
      c.dslBody.foreachStatements(recStatement)
    }

    def recStatement(s: Statement): Unit = {

      s match {
        case t: BaseType =>
          println(
            s"""type of ${t.name}
               | classname: "${t.getClass.getName}"
               | width: ${t.getBitsWidth}
               | location: ${t.getScalaLocationShort}
               | dir: ${t.dirString()}
               | source: ${t.getRealSource}
               |""".stripMargin.stripIndent())
        case _ => println(s"Unknown type ${s}")
      }

      s match {
        case ts: TreeStatement => {
          println(
            s"""Tree statement ${ts.toString}
               | classname: "${ts.getClass.getName}"
               | location: ${ts.getScalaLocationShort}
               |""".stripMargin.stripIndent())
          ts.foreachStatements(recStatement)
          s.foreachExpression(recExpression)
        }
        case ls: LeafStatement => {
          println(
            s"""Leaf statement ${ls.toString}
               | classname: "${ls.getClass.getName}"
               | location: ${ls.getScalaLocationShort}
               |""".stripMargin.stripIndent())
          s.foreachExpression(recExpression)
        }
      }
    }

    def recExpression(e: Expression): Unit = {
      e match {
        case op: Operator.BitVector.Add => println(s"Found ${op.left} + ${op.right}")
        case _ =>
      }
      e.foreachExpression(recExpression)
    }

  }
  override def hasNetlistImpact = false

  override def toString = s"${super.toString} - $message"
}
