package type_tests

import spinal.core._

import scala.collection.mutable

/// Example circuit and statement walker code provided by @Dolu1990
object Miaou1882_B extends App {
  case class BundleA() extends Bundle {
    val aa_0 = Bool()
    val aa_1 = Bool()
  }
  case class BundleB() extends Bundle {
    val bb_0 = Bool()
    val bb_1 = BundleA()
  }

  val sc = SpinalConfig()

  sc.generateVerilog(new Component{
    val x,y,z = in Bool()
    val bundleAInst = in port BundleA()
    val bundleBInst = in port BundleB()

    val area = new Area {
      val l,m,n = in Bool()
    }

    val allRootData = mutable.Set[Data]()

    this.dslBody.walkLeafStatements{
      case bt : BaseType => {
        def getRootParent(that: Data): Data = if (that.parent == null) that else getRootParent(that.parent)
        allRootData += getRootParent(bt)
      }
      case _ =>
    }
    val allRootDataInOrder = allRootData.toSeq.sortBy(_.getInstanceCounter)
    for (e <- allRootDataInOrder) {
      println(s"- ${e.getName()} : ${e.getClass.getSimpleName}")
    }
  })
}