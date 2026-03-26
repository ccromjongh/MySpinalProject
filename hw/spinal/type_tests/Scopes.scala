package type_tests

import spinal.core._
import spinal.core.internals._
import spinal.core.sim._

import scala.language.postfixOps

case class NamedBundle() extends Bundle {
  val tempOutput = out UInt(32 bits)
  val finalOutput = out SInt(32 bits)
}

case class SubComponent(outputWidth: Int) extends Component {
  val io = new Bundle {
    val input = in UInt(8 bits)
    val output = out UInt(outputWidth bits)
  }

  io.output := (io.input << 5).resize(32) ^ U(0xdedbeef)
}

case class Scopes(baseWidth: Int = 32) extends Component {
  val io = new Bundle {
    val primaryInputs = new Bundle {
      val a = in UInt(baseWidth bits)
      val b = in UInt(baseWidth bits)
      val subtractConstant = in Bool()
    }
    val tempOutput = out UInt(baseWidth bits)
    val finalOutput = out SInt(baseWidth bits)
    val subOutput = out UInt(baseWidth bits)
  }

  val subComponent = SubComponent(baseWidth)
  subComponent.io.input := io.primaryInputs.a.resized
  io.subOutput := subComponent.io.output

  val unnamedBundleIO = new Bundle {
    val tempOutput = out UInt(32 bits)
    val finalOutput = out SInt(32 bits)
  }

  val namedBundleIO = NamedBundle()

  private val signal = io.primaryInputs.a + io.primaryInputs.b
  io.tempOutput := signal

  private val myArea = new Area {
    val signal2 = SInt()
    when (io.primaryInputs.subtractConstant === True) {
      signal2 := signal.asSInt - S"000011"
    } otherwise {
      signal2 := signal.asSInt
    }
  }

  io.finalOutput := myArea.signal2
  unnamedBundleIO.finalOutput := io.finalOutput
  unnamedBundleIO.tempOutput := io.tempOutput
  namedBundleIO.finalOutput := io.finalOutput
  namedBundleIO.tempOutput := io.tempOutput
}

object ScopesVerilog extends App {
  Config.spinal.generateVerilog(Scopes())
}

object ScopesSim extends App {
  Config.sim.compile(Scopes()).doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)
    dut.io.primaryInputs.a #= 5
    dut.io.primaryInputs.b #= 10
    println(s"temp = ${dut.io.tempOutput.toLong}")
    println(s"final = ${dut.io.finalOutput.toLong}")
  }
}