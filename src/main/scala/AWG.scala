// SPDX-License-Identifier: Apache-2.0

package awg

import chisel3._
import chisel3.experimental._
import chisel3.util._
import dspblocks._
import dsptools._
import dsptools.numbers._
import dspjunctions._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import plfg._
import nco._


class AWG[T <: Data : Real : BinaryRepresentation]
(
  paramsPLFG: Seq[PLFGParams[T]],
  paramsNCO: Seq[NCOParams[T]],
  csrAddressSetsPLFG: Seq[AddressSet],
  csrAddressSetsNCO: Seq[AddressSet],
  ramAddressSets: Seq[AddressSet],
  numOfAWGs: Int,
  queueSize: Int,
  beatBytes: Int
) extends LazyModule()(Parameters.empty) {
  require(numOfAWGs > 0)
  require(queueSize > 0)
  require(paramsPLFG.length == numOfAWGs)
  require(paramsNCO.length == numOfAWGs)
  require(csrAddressSetsPLFG.length == numOfAWGs)
  require(csrAddressSetsNCO.length == numOfAWGs)
  require(ramAddressSets.length == numOfAWGs)

  val slaveNodes = (0 until numOfAWGs).map(e => AXI4StreamSlaveNode(AXI4StreamSlaveParameters())).toSeq.toArray
  val streamNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("out", n = beatBytes)))))

  val PLFGs = (0 until numOfAWGs).map(e => LazyModule(new PLFGDspBlockMem(csrAddressSetsPLFG(e), ramAddressSets(e), paramsPLFG(e), beatBytes))).toSeq.toArray
  val NCOs = (0 until numOfAWGs).map(e => LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO(e), csrAddressSetsNCO(e), beatBytes))).toSeq.toArray
  
  for (i <- 0 to (numOfAWGs-1)) {
    NCOs(i).freq.get := PLFGs(i).streamNode
    slaveNodes(i) := NCOs(i).streamNode
  }
  
  val topXbar = AXI4Xbar()
  for (i <- 0 to (numOfAWGs-1)) {
    PLFGs(i).mem.get := topXbar
    if (paramsNCO(i).useMultiplier) NCOs(i).mem.get := topXbar
  }
  val mem = Some(AXI4IdentityNode())
  topXbar := mem.get

  lazy val module = new LazyModuleImp(this) {
  
    val ioout = streamNode.out(0)._1
  
    val queues = (0 until numOfAWGs).map(e => Module(new Queue(UInt((beatBytes * 8).W), queueSize))).toSeq.toArray
    val queueCounters = (0 until numOfAWGs).map(e => RegInit(0.U(log2Ceil(queueSize).W))).toSeq.toArray
    
    val queueLasts = (0 until numOfAWGs).map(e => Module(new Queue(Bool(), queueSize))).toSeq.toArray
    
    val outputValids = (0 until numOfAWGs).map(e => queues(e).io.deq.valid).toSeq.toArray
    val outputDataSin = (0 until numOfAWGs).map(e => queues(e).io.deq.bits(4*beatBytes-1, 0)).toSeq.toArray // .asSInt
    val outputDataCos = (0 until numOfAWGs).map(e => queues(e).io.deq.bits(8*beatBytes-1, 4*beatBytes)).toSeq.toArray //.asSInt
    val outputLasts = (0 until numOfAWGs).map(e => queueLasts(e).io.deq.bits).toSeq.toArray
    
    for (i <- 0 to (numOfAWGs-1)) {
      queueCounters(i) := queueCounters(i) +& slaveNodes(i).in(0)._1.fire() -& ioout.fire()
      slaveNodes(i).in(0)._1.ready := (queueCounters(i) < queueSize.U)
      queues(i).io.enq.valid := slaveNodes(i).in(0)._1.fire()
      queues(i).io.enq.bits := slaveNodes(i).in(0)._1.bits.data
      queues(i).io.deq.ready := outputValids.foldLeft(ioout.ready)(_ && _)
      
      queueLasts(i).io.enq.valid := slaveNodes(i).in(0)._1.fire()
      queueLasts(i).io.enq.bits := slaveNodes(i).in(0)._1.bits.last
      queueLasts(i).io.deq.ready := outputValids.foldLeft(ioout.ready)(_ && _)
    }

    val outputValid = RegInit(Bool(), false.B)
    when (ioout.ready) {outputValid := outputValids.foldLeft(ioout.ready)(_ && _)}
    ioout.valid := outputValid
    
    val outputValue = RegInit(UInt((beatBytes * 8).W), false.B)
    when (ioout.ready) {outputValue := Cat((outputDataCos.foldLeft(0.U)(_ +& _)).asTypeOf(UInt((4*beatBytes).W)), (outputDataSin.foldLeft(0.U)(_ +& _)).asTypeOf(UInt((4*beatBytes).W)))}
    ioout.bits.data := outputValue
    
    val outputLast = RegInit(Bool(), false.B)
    when (ioout.ready) {outputLast := outputLasts.foldLeft(true.B)(_ && _)}
    ioout.bits.last := outputLast
  }
}


object AWGApp extends App {
  
  val beatBytes = 4
  val numOfAWGs = 2
  val queueSize = 64

  val paramsNCO1 = FixedNCOParams( // pinc 16
    tableSize = 256,
    tableWidth = 16,
    phaseWidth = 10,
    rasterizedMode = false,
    nInterpolationTerms = 0,
    ditherEnable = false,
    syncROMEnable = false,
    phaseAccEnable = true,
    roundingMode = RoundHalfUp,
    pincType = Streaming,
    poffType = Fixed,
    useMultiplier = false,
    numMulPipes = 1
  )
  val paramsNCO2 = FixedNCOParams( // pinc 16
    tableSize = 64,
    tableWidth = 16,
    phaseWidth = 8,
    rasterizedMode = false,
    nInterpolationTerms = 0,
    ditherEnable = false,
    syncROMEnable = false,
    phaseAccEnable = false,
    roundingMode = RoundHalfUp,
    pincType = Streaming,
    poffType = Fixed,
    useMultiplier = true,
    numMulPipes = 2
  )
  val paramsPLFG1 = FixedPLFGParams(
    maxNumOfSegments = 4,
    maxNumOfDifferentChirps = 8,
    maxNumOfRepeatedChirps = 8,
    maxChirpOrdinalNum = 4,
    maxNumOfFrames = 4,
    maxNumOfSamplesWidth = 12,
    outputWidthInt = 16,
    outputWidthFrac = 0
  )
  val paramsPLFG2 = FixedPLFGParams(
    maxNumOfSegments = 2,
    maxNumOfDifferentChirps = 4,
    maxNumOfRepeatedChirps = 4,
    maxChirpOrdinalNum = 2,
    maxNumOfFrames = 2,
    maxNumOfSamplesWidth = 10,
    outputWidthInt = 16,
    outputWidthFrac = 0
  )
  
  val parametersForPLFGs = Seq(paramsPLFG1, paramsPLFG2)
  val parametersForNCOs = Seq(paramsNCO1, paramsNCO2)
  val csrAddressSetsForPLFGs = Seq(AddressSet(0x001000, 0xFF), AddressSet(0x001100, 0xFF))
  val csrAddressSetsForNCOs = Seq(AddressSet(0x001200, 0xFF), AddressSet(0x001300, 0xFF))
  val ramAddressSets = Seq(AddressSet(0x000000, 0x03FF), AddressSet(0x000400, 0x03FF))

  val chainModule = LazyModule(new AWG(parametersForPLFGs, parametersForNCOs, csrAddressSetsForPLFGs, csrAddressSetsForNCOs, ramAddressSets, numOfAWGs, queueSize, beatBytes) with AXI4BlockIO {
  })
  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "AWGApp"), ()=> chainModule.module) // generate verilog code
}
