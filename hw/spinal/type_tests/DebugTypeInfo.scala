package type_tests

import spinal.core._
import spinal.core.internals._

import scala.collection.mutable

class DebugTypeInfo(message : String) extends Phase {
  def convertSourceLoc(s: String): String = s.replace("@ ", "").replace(" l", ":")

  val signalsMetaData = mutable.Set[Data]()

  override def impl(pc: PhaseContext) = {
    println(message)

    recComponent(pc.topLevel)

    def recComponent(c: Component): Unit = {
      println(
        s"""Component ${c.name}
           |  classname: "${c.getClass.getName}"
           |""".stripMargin.stripIndent())

      c.children.foreach(recComponent)
      c.dslBody.foreachStatements(recStatement)
      println(createHglddString(c))
    }

    def createHglddString(c: Component): String = {
      val allRootSignals = mutable.Set[Data]()
      // val structsToSpecify = mutable.Set[Data]()
      def getRootParent(that: Data): Data = if (that.parent == null) that else getRootParent(that.parent)
      val outputString = new StringBuilder

      c.dslBody.walkLeafStatements {
        case bt : BaseType =>
          allRootSignals += getRootParent(bt)
        case _ =>
      }

      outputString.append("""{"objects": [""")
      outputString.append(s"""{"kind": "module", "module_name": "${c.name}", "port_vars": [""")

      def processSignal(signal: Data): Unit = {
        val elements: mutable.ArrayBuffer[(String, Data)] = signal match {
          case b: Bundle => b.elements
          case v: Vec[_] => v.elements
          case _ => mutable.ArrayBuffer()
        }
        if (elements.nonEmpty) {
          outputString.append(s"""{"opcode":"'{","operands":[""")
          // Todo make recursive
          outputString.append(elements.map(e => s"""{"sig_name": "${e._2.getName}"}""").mkString(","))
          outputString.append("]}")
        } else {
          outputString.append(s"""{"sig_name": "${signal.getName}"}""")
        }
      }

      for (signal <- allRootSignals) {
        outputString.append(s"""{"var_name": "${signal.getName}", "value": """)
        processSignal(signal)
        outputString.append(s""", "type_name": "logic"},""")
      }
      outputString.setLength(outputString.length - 1)
      outputString.append("]}")
      outputString.append("]}")
      outputString.result()
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
               |  .source: ${as.source.toString}
               |  .target: ${as.target.toString}
               |  .final target: ${as.finalTarget.toString}
               |  .location(${convertSourceLoc(as.locationString)})""".stripMargin.stripIndent())
        case bt: BaseType =>
          println(
            s"""BaseType leaf statement ${bt.name}
               |  .width: ${bt.getBitsWidth}
               |  .dir: ${bt.dirString()}
               |  .source: ${bt.getRealSource}""".stripMargin.stripIndent())
        case as: AssertStatement =>
          println(
            s"""Assert leaf statement ${as.toString}
               |  .message: ${as.message}
               |  .condition: ${as.cond.toString}""".stripMargin.stripIndent())
      }

      println(s"  .classname: \"${ls.getClass.getName}\"\n")
      ls.foreachExpression(recExpression(_))
    }

    def exploreTreeStatement(ts: TreeStatement): Unit = {
      ts match {
        case ws: WhenStatement =>
          println(
            s"""When tree statement ${ws.toString}
               |  condition: ${ws.cond.toString}
               |  when true: ${ws.whenTrue.toString}
               |  when false: ${ws.whenFalse.toString}""".stripMargin.stripIndent())
        case ss: SwitchStatement =>
          println(
            s"""When tree statement ${ss.toString}
               |  value: ${ss.value.toString}
               |  elements: ${ss.elements.toString}""".stripMargin.stripIndent())
      }
      ts.foreachStatements(recStatement)
      ts.foreachExpression(recExpression(_))
    }

    def recExpression(e: Expression, depth: Int = 0): Unit = {
      e match {
        case op: Operator.BitVector.Add => println(s"Found ${op.left} + ${op.right}")
        case _ =>
      }
      println(s"${"  ".repeat(depth)}Expression: ${e.toString}")
      e.foreachExpression(recExpression(_, depth+1))
    }

  }
  override def hasNetlistImpact = false

  override def toString = s"${super.toString} - $message"
}
