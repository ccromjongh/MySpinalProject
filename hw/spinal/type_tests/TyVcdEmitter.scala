package type_tests

import spinal.core._
import spinal.core.internals._

import scala.annotation.tailrec
import scala.collection.mutable

class TyVcdEmitter(message : String) extends Phase {
  override def impl(pc: PhaseContext) = {
    println(message)
    val outputString = new StringBuilder

    // Open the top level and scopes
    outputString.append("""{
                          |    "$schema": "TyVcdSchema.json",
                          |    "scopes": {""".stripMargin)

    outputString.append(createTyVcdScopeString(pc.topLevel))

    // Close top level and scopes
    outputString.append(s"""\n    }\n}""")
    println(outputString.result)

    def createTyVcdScopeString(c: Component): String = {
      val scopeString = new StringBuilder
      val allRootSignals = mutable.Set[Data]()
      // val structsToSpecify = mutable.Set[Data]()
      @tailrec
      def getRootParent(that: Data): Data = if (that.parent == null) that else getRootParent(that.parent)

      c.dslBody.walkLeafStatements {
        case bt : BaseType =>
          allRootSignals += getRootParent(bt)
        case _ =>
      }

      def signalToVariableString(signal: Data): String = {
        val variableStringBuilder = new StringBuilder
        val (kind, elements): (String, mutable.ArrayBuffer[(String, Data)]) = signal match {
          case b: Bundle => ("struct", b.elements)
          case v: Vec[_] => ("vector", v.elements)
          case _ => ("ground", mutable.ArrayBuffer())
        }
        // Open variable and kind
        variableStringBuilder.append(
          s"""{
             |  "name": "${signal.getName}",
             |  ${highLevelTypeInfoString(signal)},
             |  "kind": {
             |    "kind": "${kind}",
             |    """.stripMargin)

        if (elements.nonEmpty) {
          // Open and close fields
          variableStringBuilder.append(s""""fields": [ """)
          variableStringBuilder.append(elements.map(e => signalToVariableString(e._2)).mkString(", "))
          variableStringBuilder.append(" ]")
        } else {
          variableStringBuilder.append(s""""width": ${signal.getBitsWidth}""")
        }
        // Close variable and kind
        variableStringBuilder.append(" } }")
        variableStringBuilder.result
      }

      // Open scope object and variables
      scopeString.append(s""""${c.getName}": { "name": "${c.getName}", ${highLevelTypeInfoString(c)}, "variables": [""")
      scopeString.append(allRootSignals.map(e => signalToVariableString(e)).mkString(", "))
      // Close variables and scope object
      scopeString.append(s"""], "subscopes": { ${c.children.map(createTyVcdScopeString).mkString(", ")} } }""")
      scopeString.result
    }

    def highLevelTypeInfoString[T](item: T): String = {
      // Todo: add parameters via reflection
      // Open and close high_level_info
      s""""high_level_info": {
         |    "type_name": "${item.getClass.getSimpleName}",
         |    "params": []
         |}""".stripMargin
    }

  }
  override def hasNetlistImpact = false

  override def toString = s"${super.toString} - $message"
}
