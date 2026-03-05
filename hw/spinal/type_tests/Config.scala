package type_tests

import spinal.core._
import spinal.core.internals.Phase
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    ),
    onlyStdLogicVectorAtTopLevelIo = false,
    transformationPhases = {
      val phases = new ArrayBuffer[Phase]()
      phases.append(new DebugTypeInfo("Early"))
      phases
    }
  )

  def sim = SimConfig.withConfig(spinal).withVcdWave
}
