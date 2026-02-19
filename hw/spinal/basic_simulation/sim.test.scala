package basic_simulation

import spinal.core._

// Identity takes n bits in a and gives them back in z
case class Identity(n: Int) extends Component {
  val io = new Bundle {
    val a = in Bits(n bits)
    val z = out Bits(n bits)
  }

  io.z := io.a
}

import spinal.core.sim._

object TestIdentity extends App {
  val config = SpinalConfig()

  config.generateVerilog(Identity(3))

  // Use the component with n = 3 bits as "dut" (device under test)
  SimConfig.withWave.withConfig(config).compile(Identity(3)).doSim{ dut =>
    // For each number from 3'b000 to 3'b111 included
    for (a <- 0 to 7) {
      // Apply input
      dut.io.a #= a
      // Wait for a simulation time unit
      sleep(1)
      // Read output
      val z = dut.io.z.toInt
      // Check result
      assert(z == a, s"Got $z, expected $a")
    }
  }
}
