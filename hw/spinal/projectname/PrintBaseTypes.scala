package projectname

import spinal.core._
import spinal.core.internals._

class PrintBaseTypes(message : String) extends Phase {
  override def impl(pc: PhaseContext) = {
    println(message)

    recComponent(pc.topLevel)

    def recComponent(c: Component): Unit = {
      c.children.foreach(recComponent)
      c.dslBody.foreachStatements(recStatement)
    }

    def recStatement(s: Statement): Unit = {
      s.foreachExpression(recExpression)
      s match {
        case ts: TreeStatement => ts.foreachStatements(recStatement)
        case _ =>
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
