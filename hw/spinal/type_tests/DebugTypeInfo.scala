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
        case ts: TreeStatement => exploreTreeStatement(ts)
        case ls: LeafStatement => exploreLeafStatement(ls)
      }
    }

    def exploreLeafStatement(ls: LeafStatement): Unit = {
      ls match {
        case as: AssignmentStatement =>
          println(
            s"""Assignment leaf statement ${as.toString}
               | source: ${as.source.toString}
               | target: ${as.target.toString}
               | final target: ${as.finalTarget.toString}
               | location: ${as.locationString}""".stripMargin.stripIndent())
        case bt: BaseType =>
          println(
            s"""BaseType leaf statement ${bt.name}
               | classname: "${bt.getClass.getName}"
               | width: ${bt.getBitsWidth}
               | location: ${bt.getScalaLocationShort}
               | dir: ${bt.dirString()}
               | source: ${bt.getRealSource}""".stripMargin.stripIndent())
        case as: AssertStatement =>
          println(
            s"""Assert leaf statement ${as.toString}
               | message: ${as.message}
               | condition: ${as.cond.toString}
               | location: ${as.loc.toString}""".stripMargin.stripIndent())
      }

      println(s" classname: \"${ls.getClass.getName}\"\n")
      ls.foreachExpression(recExpression)
    }

    def exploreTreeStatement(ts: TreeStatement): Unit = {
      ts match {
        case ws: WhenStatement =>
          println(
            s"""When tree statement ${ws.toString}
               | condition: ${ws.cond.toString}
               | when true: ${ws.whenTrue.toString}
               | when false: ${ws.whenFalse.toString}""".stripMargin.stripIndent())
        case ss: SwitchStatement =>
          println(
            s"""When tree statement ${ss.toString}
               | value: ${ss.value.toString}
               | elements: ${ss.elements.toString}""".stripMargin.stripIndent())
      }
      ts.foreachStatements(recStatement)
      ts.foreachExpression(recExpression)
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
