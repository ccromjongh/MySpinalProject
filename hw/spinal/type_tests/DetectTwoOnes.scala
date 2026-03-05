package type_tests

import spinal.core._
import spinal.core.internals._
import spinal.core.sim._

import scala.language.postfixOps

case class DetectTwoOnes() extends Component {
  val io = new Bundle {
    val in_val      = in Bool()
    val out_val     = out Bool()
  }

  object State extends SpinalEnum { val sNone, sOne1, sTwo1s = newElement() }
  val state = RegInit(State.sNone)

  // Tmp signal 1
  val isOne = Bool()
  isOne := io.in_val
  // Tmp signal 2
  val willBeTwo1s = io.in_val && (state === State.sOne1 || state === State.sTwo1s)

  io.out_val := (state === State.sTwo1s)

  switch(state) {
    is(State.sNone) { when(isOne) { state := State.sOne1 } }
    is(State.sOne1) {
      when(isOne) { state := State.sTwo1s }.otherwise { state := State.sNone }
    }
    is(State.sTwo1s) { when(!isOne) { state := State.sNone } }
  }
}

object DetectTwoOnesVerilog extends App {
  Config.spinal.generateVerilog(DetectTwoOnes())
}

object DetectTwoOnesSim extends App {
  Config.sim.compile(DetectTwoOnes()).doSim { dut =>
    // Inputs and expected results
    val inputs   = Seq(0, 0, 1, 0, 1, 1, 0, 1, 1, 1)
    val expected = Seq(0, 0, 0, 0, 0, 1, 0, 0, 1, 1)

    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)
    // Wait for reset to be done
    dut.clockDomain.waitSampling

    dut.io.in_val #= false
    dut.clockDomain.waitActiveEdge

    for (i <- inputs.indices) {
      dut.io.in_val #= inputs(i).toBoolean
      dut.clockDomain.waitRisingEdge
      sleep(0) // Required to let the register update
      assert(dut.io.out_val.toBoolean == expected(i).toBoolean, s"Result was ${dut.io.out_val.toBoolean}, expected ${expected(i)}")
    }
    dut.clockDomain.waitRisingEdge
  }
}
