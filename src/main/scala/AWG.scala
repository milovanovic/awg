package nco

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


class AWG[T <: Data : Real : BinaryRepresentation]
(
  paramsPLFG1: PLFGParams[T],
  paramsPLFG2: PLFGParams[T],
  paramsPLFG3: PLFGParams[T],
  paramsNCO1: NCOParams[T],
  paramsNCO2: NCOParams[T],
  paramsNCO3: NCOParams[T],
  csrAddressPLFG1: AddressSet,
  ramAddress1: AddressSet,
  csrAddressPLFG2: AddressSet,
  ramAddress2: AddressSet,
  csrAddressPLFG3: AddressSet,
  ramAddress3: AddressSet,
  csrAddressNCO: AddressSet,
  csrAddress: AddressSet,
  beatBytes: Int
) extends LazyModule()(Parameters.empty) {
  
  val slaveNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val streamNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("out", n = beatBytes)))))
  
  val PLFGModule1 = LazyModule(new PLFGDspBlockMem(csrAddressPLFG1, ramAddress1, paramsPLFG1, beatBytes) {
  })
  val PLFGModule2 = LazyModule(new PLFGDspBlockMem(csrAddressPLFG2, ramAddress2, paramsPLFG2, beatBytes) {
  })
  val PLFGModule3 = LazyModule(new PLFGDspBlockMem(csrAddressPLFG3, ramAddress3, paramsPLFG3, beatBytes) {
  })
  val ncoModule1 = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO1, csrAddressNCO, beatBytes) {
  })
  val ncoModule2 = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO2, csrAddressNCO, beatBytes) {
  })
  val ncoModule3 = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO3, csrAddressNCO, beatBytes) {
  })
  val axi3to1 = LazyModule(new AXIBridge3to1(paramsNCO1.protoOut.getWidth, csrAddress, beatBytes))
  
  val buffer1 = AXI4StreamBuffer()
  val buffer2 = AXI4StreamBuffer()
  val buffer3 = AXI4StreamBuffer()

  ncoModule1.freq.get := PLFGModule1.streamNode
  ncoModule2.freq.get := PLFGModule2.streamNode
  ncoModule3.freq.get := PLFGModule3.streamNode
  //axi3to1.slaveNode1 := ncoModule1.streamNode
  //axi3to1.slaveNode2 := ncoModule2.streamNode
  //axi3to1.slaveNode3 := ncoModule3.streamNode
  axi3to1.slaveNode1 := buffer1 := ncoModule1.streamNode
  axi3to1.slaveNode2 := buffer2 := ncoModule2.streamNode
  axi3to1.slaveNode3 := buffer3 := ncoModule3.streamNode
  slaveNode := axi3to1.streamNode

  val topXbar = AXI4Xbar()
  PLFGModule1.mem.get := topXbar
  PLFGModule2.mem.get := topXbar
  PLFGModule3.mem.get := topXbar
  axi3to1.mem.get := topXbar
  val mem = Some(AXI4IdentityNode())
  topXbar := mem.get

  lazy val module = new LazyModuleImp(this) {
    val ioout = streamNode.out(0)._1
    val ioin = slaveNode.in(0)._1
    ioout.bits.data := ioin.bits.data
    ioout.bits.last := ioin.bits.last
    ioout.valid := ioin.valid
    ioin.ready := ioout.ready
  }
}


object AWGApp extends App {
  val beatBytes = 4

  val paramsNCO1 = FixedNCOParams( // pinc 16
    tableSize = 256,
    tableWidth = 14,
    phaseWidth = 10,
    rasterizedMode = false,
    nInterpolationTerms = 0,
    ditherEnable = false,
    syncROMEnable = false,
    phaseAccEnable = true,
    roundingMode = RoundHalfUp,
    pincType = Streaming,
    poffType = Fixed
  )
  val paramsNCO2 = FixedNCOParams( // pinc 8
    tableSize = 256,
    tableWidth = 14,
    phaseWidth = 10,
    rasterizedMode = false,
    nInterpolationTerms = 0,
    ditherEnable = false,
    syncROMEnable = false,
    phaseAccEnable = true,
    roundingMode = RoundHalfUp,
    pincType = Streaming,
    poffType = Fixed
  )
  val paramsNCO3 = FixedNCOParams( // pinc 1
    tableSize = 256,
    tableWidth = 14,
    phaseWidth = 10,
    rasterizedMode = false,
    nInterpolationTerms = 0,
    ditherEnable = false,
    syncROMEnable = false,
    phaseAccEnable = false,
    roundingMode = RoundHalfUp,
    pincType = Streaming,
    poffType = Fixed
  )
  val paramsPLFG = FixedPLFGParams(
    maxNumOfSegments = 4,
    maxNumOfDifferentChirps = 8,
    maxNumOfRepeatedChirps = 8,
    maxChirpOrdinalNum = 4,
    maxNumOfFrames = 4,
    maxNumOfSamplesWidth = 12,
    outputWidthInt = 16,
    outputWidthFrac = 0
  )

  val chainModule = LazyModule(new AWG(paramsPLFG, paramsPLFG, paramsPLFG, paramsNCO1, paramsNCO2, paramsNCO3, AddressSet(0x001000, 0xFF), AddressSet(0x000000, 0x03FF), AddressSet(0x001100, 0xFF), AddressSet(0x000400, 0x03FF), AddressSet(0x001200, 0xFF), AddressSet(0x000800, 0x03FF), AddressSet(0x001300, 0xFF), AddressSet(0x001400, 0xFF), beatBytes) with AXI4BlockIO {
  })
  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "AWGApp"), ()=> chainModule.module) // generate verilog code
}
