package type_tests

import spinal.core._
import spinal.core.internals._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    ),
    onlyStdLogicVectorAtTopLevelIo = false,
    phasesInserters = ArrayBuffer[(ArrayBuffer[Phase]) => Unit](
      { phases => phases.insert(phases.indexWhere(_.isInstanceOf[PhaseInferWidth]), new DebugTypeInfo("Late")) }
    )
  )

  def sim = SimConfig.withConfig(spinal).withVcdWave
}
