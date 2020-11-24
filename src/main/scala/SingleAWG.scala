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


class SingleAWG[T <: Data : Real : BinaryRepresentation]
(
  paramsPLFG1: PLFGParams[T],
  paramsNCO1: NCOParams[T],
  csrAddressPLFG1: AddressSet,
  ramAddress1: AddressSet,
  csrAddressNCO: AddressSet,
  csrAddress: AddressSet,
  beatBytes: Int
) extends LazyModule()(Parameters.empty) {
  
  val slaveNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val streamNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("out", n = beatBytes)))))
  
  val PLFGModule1 = LazyModule(new PLFGDspBlockMem(csrAddressPLFG1, ramAddress1, paramsPLFG1, beatBytes) {
  })
  val ncoModule1 = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO1, csrAddressNCO, beatBytes) {
  })
  val axi1to1 = LazyModule(new AXIBridge1to1Multiplying(paramsNCO1.protoOut.getWidth, csrAddress, beatBytes))
  
  val buffer = AXI4StreamBuffer()
  
  ncoModule1.freq.get := PLFGModule1.streamNode
  axi1to1.slaveNode1 := ncoModule1.streamNode
  //slaveNode := axi1to1.streamNode
  slaveNode := buffer := axi1to1.streamNode

  val topXbar = AXI4Xbar()
  PLFGModule1.mem.get := topXbar
  axi1to1.mem.get := topXbar
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


trait AXI4BlockIO extends DspBlock[
  AXI4MasterPortParameters,
  AXI4SlavePortParameters,
  AXI4EdgeParameters,
  AXI4EdgeParameters,
  AXI4Bundle] {
    def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    val ioMem = mem.map { 
      m => {
        val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
        m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
        val ioMem = InModuleBody { ioMemNode.makeIO() }
        ioMem
      }
    }
    // generate out stream
    val ioStreamNode = BundleBridgeSink[AXI4StreamBundle]()
    ioStreamNode := 
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode
    val outStream = InModuleBody { ioStreamNode.makeIO() }
}


object SingleAWGApp extends App {
  
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

  val chainModule = LazyModule(new SingleAWG(paramsPLFG, paramsNCO1, AddressSet(0x001000, 0xFF), AddressSet(0x000000, 0x03FF), AddressSet(0x001100, 0xFF), AddressSet(0x001200, 0xFF), beatBytes) with AXI4BlockIO {
  })
  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "SingleAWGApp"), ()=> chainModule.module) // generate verilog code
}
