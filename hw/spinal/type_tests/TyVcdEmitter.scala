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
                          |    "$schema": "schemas/TyVcdSchema.json",
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

    @tailrec
    def valueToJson(value: Any): String = {
      value match {
        case _: Int | _: BigInt | _: Long | _: Short | _: Boolean => value.toString
        // case v: Seq[_] => "[" + v.map(valueToJson).mkString(", ") + " ]"
        case Some(x) => valueToJson(x)
        case None | null => "null"
        case _ => s"\"${value.toString}\""
      }
    }

    //────────────────────────────────────────────────────────────────────────────────//
    // Code copied from TywavesAnnotation.scala by Raffaele Meloni in his Chisel fork //
    //────────────────────────────────────────────────────────────────────────────────//

    /** Represent the parameters of a class constructor:
     *  {{{
     *   class A(
     *        a: Int,                  // parameter without val
     *        val b: String,           // parameter with val
     *        protected val c: Char,   // parameter with protected val
     *        private val d: Boolean,  // parameter with private val
     *        val o: OtherClass        // parameter with complex type (another class)
     *       )
     *  }}}
     *
     *  The ClassParam stores for each parameter the name, the type and optionally the value of it.
     *
     * @param name The name of the parameter
     * @param typeName The type of the parameter
     * @param value The value of the parameter. It is `None` when the annotator is not able to retrieve the value
     */
    case class ClassParam(name: String, typeName: String, value: Option[String]) {
      def toJson: String = s"""{ "name": "$name", "tpe": "$typeName", "value": ${valueToJson(value)} }"""
    }

    /** Get the parameters ([[ClassParam]]) in the constructor of a given scala class.
     *
     * @param target The instance of the class. It can be any class instance.
     * @return A list of [[ClassParam]] that contains the name, type and value* of the parameters in the constructor.
     *         The name and the type are always returned for any class and any kind of parameter.
     *         An actual value of the parameter instead is returned only for `case classes` and for `val`/`var`
     *         parameters (actual fields of a class, i.e. `class A(val a: Int)` and `case class A(a: Int)`.
     *         None is returned for simply parameters (i.e. `class A(a: Int)`).
     *
     *         It ignores fields in the body of the class (i.e. `class A(a: Int) { val b = 10 }`). It is something
     *         certainly possible but it is not implemented since we assume the "type" of a [[chisel3.Module]] and
     *         [[chisel3.Data]] is given by its constructor.
     *
     *         For parameters of complex types (i.e. other classes), the value of the instance class is a string
     *         including recursively the values of the parameters of the nested class.
     * @example {{{
     * class BaseClass (val a: Int)
     * class OtherClass(val a: Int, val b: BaseClass)
     * // Example of nested class in parameters
     * class TopClass  (a: Int, val b: String, protected val c: Char, private val d: Boolean, val o: OtherClass)
     *
     * case class CaseClassExample(a: Int, o: OtherClass)
     *
     * val baseClass = new BaseClass(1)
     * val otherClass = new OtherClass(1, baseClass)
     * val topClass = new TopClass(1, "hello", 'c', true, otherClass)
     * val caseClass = new CaseClassExample(1, otherClass)
     *
     * getConstructorParams(baseClass)  // List(ClassParam("a", "Int", Some(1)))
     * getConstructorParams(otherClass) // List(ClassParam("a", "Int", Some(1)),
     *                                  //      ClassParam("b", "BaseClass", Some("BaseClass(a: 1)")))
     * getConstructorParams(topClass)   // List(ClassParam("a", "Int", None),
     *                                  //      ClassParam("b", "String", Some("hello")),
     *                                  //      ClassParam("c", "Char", Some('c')),
     *                                  //      ClassParam("d", "Boolean", Some(true)),
     *                                  //      ClassParam("o", "OtherClass", Some("OtherClass(a: 1, b: BaseClass(a: 1))"))
     * getConstructorParams(caseClass)  // List(ClassParam("a", "Int", Some(1)),
     *                                  //      ClassParam("o", "OtherClass", Some("OtherClass(a: 1, b: BaseClass(a: 1))"))
     *
     * }}}
     */
    def getConstructorParams(target: Any): Seq[ClassParam] = {
      import scala.reflect.runtime.universe._
      import scala.reflect.api.{Mirror, TypeCreator, Universe}
      def getTypeTag[T](target: T) = {
        val c = target.getClass
        val mirror = runtimeMirror(c.getClassLoader) // obtain runtime mirror
        val sym = mirror.staticClass(c.getName) // obtain class symbol for `c`
        val tpe = sym.selfType // obtain type object for `c`
        // create a type tag which contains above type object
        TypeTag(
          mirror,
          new TypeCreator {
            def apply[U <: Universe with Singleton](m: Mirror[U]) =
              if (m eq mirror) tpe.asInstanceOf[U#Type]
              else
                throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
          }
        )
      }
      val tt = getTypeTag(target)

      def hasParams(target: Any): Boolean = {
        val tt = getTypeTag(target)
        val im = runtimeMirror(target.getClass.getClassLoader).reflect(target)
        tt.tpe.members.collect {
            case m: MethodSymbol if m.isConstructor => m
          } // Get the constructor
          .flatMap(_.paramLists.flatten)
          .exists { a =>
            try {
              im.reflectField(a.asTerm).get // if it can be reflected it has fields
              true
            } catch { case e: Exception => false } // Otherwise, it does not have fields: complex type
          }
      }

      // Get the instance mirror
      val im = runtimeMirror(target.getClass.getClassLoader).reflect(target)

      // Collect all the parameters in the primary constructor
      // 1. Get all the members of this type
      // 2. Filter the method symbol that is the primary constructor
      // 3. Get the list of params in this method

      val l = tt.tpe.members.collect {
        case m: MethodSymbol if m.isConstructor =>
          m.paramLists.flatten.collect {
            // Filter the object itself??
            case a if !a.name.toString.contains("$outer") =>
              val t = a.info.toString.split("\\$")
              val typeName = (if (t.length > 1) t(1) else t(0)).split("\\.").last // Remove the package name
              val paramName = a.name.toString // Get the name of the parameter
              val value =
                try {
                  // Try to extract the value of the parameter
                  val term =
                    try { tt.tpe.decl(a.name).asTerm.accessed.asTerm }
                    catch { case _: Throwable => a.asTerm }
                  val valueTerm = im.reflectField(term).get

                  val finalValueTerm =
                    // Recursive base case
                    if (!hasParams(valueTerm)) {
                      // If the the parameter is a Data
                      //    class Top(val param1: UInt) extends Bundle
                      // then simplify the value of the parameter itself.
                      // This prevents from having something like "Top.param1: IO[UInt<8>]"
                      // and instead it will be simply "IO[UInt<8>]" (the value of the parameter)
                      valueTerm match {
                        // case v: Data => dataToTypeName(v)
                        case _ => valueTerm.toString
                      }
                    }
                    // Recursive call
                    else {
                      val params = getConstructorParams(valueTerm).map { p =>
                        p.value.fold(p.name)(v => s"${p.name}: $v")
                      }
                      // Format the parameters in this way: Type(param1: value1, param2: value2, ...)
                      s"$typeName(${params.mkString(", ")})"
                    }
                  Some(finalValueTerm)
                } catch {
                  case _: Throwable => None // Ignore the exception if the value cannot be extracted (not included)
                }
              ClassParam(paramName, typeName, value)
          }
      }.toList.flatten
      l
    }


    //──────────//
    // End copy //
    //──────────//

    def highLevelTypeInfoString[T](item: T): String = {
      val params = getConstructorParams(item)
      // Open and close high_level_info
      s""""high_level_info": {
         |    "type_name": "${item.getClass.getSimpleName}",
         |    "params": [${params.map(_.toJson).mkString(", ")}]
         |}""".stripMargin
    }

  }
  override def hasNetlistImpact = false

  override def toString = s"${super.toString} - $message"
}
