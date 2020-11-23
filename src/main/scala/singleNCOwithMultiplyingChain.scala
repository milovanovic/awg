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
import fft._
import plfg._


class AXIBridge1to1Multiplying(dataWidth: Int, csrAddress: AddressSet, beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val streamNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("out", n = beatBytes)))))

  val slaveNode1 = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())

  val mem = Some(AXI4RegisterNode(address = csrAddress, beatBytes = beatBytes))

  lazy val module = new LazyModuleImp(this) {
    val ioout = streamNode.out(0)._1
    val ioin1 = slaveNode1.in(0)._1

    //val multiplyingFactor1 = RegInit(UInt(14.W), 0.U)
    val multiplyingFactor1 = RegInit(UInt(dataWidth.W), 0.U)

    val fields = Seq(
      //RegField(14, multiplyingFactor1,
      RegField(dataWidth, multiplyingFactor1,
        RegFieldDesc(name = "multiplyingFactor1", desc = "multiplying factor for first nco output")),
    )

    mem.get.regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)


    val interfaceOut1 = Wire(DecoupledIO(UInt(32.W)))

    val interfaceIn1 = Wire(Flipped(DecoupledIO(UInt(32.W))))

    ioin1.ready := interfaceIn1.ready

    interfaceIn1.valid := ioin1.valid

    interfaceIn1.bits := ioin1.bits.data.asUInt()


    interfaceOut1.ready := ioout.ready && interfaceOut1.valid

    ioout.valid := interfaceOut1.valid

    when (interfaceOut1.fire()) {

      DspContext.withBinaryPointGrowth(0) {
        //val nco_imag1 = (multiplyingFactor1.asTypeOf(FixedPoint(16.W, 12.BP)) context_* interfaceOut1.bits(13, 0).asTypeOf(FixedPoint(14.W, 12.BP))).asSInt
        //val nco_real1 = (multiplyingFactor1.asTypeOf(FixedPoint(16.W, 12.BP)) context_* interfaceOut1.bits(27, 14).asTypeOf(FixedPoint(14.W, 12.BP))).asSInt
        val nco_imag1 = (multiplyingFactor1.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut1.bits((dataWidth-1), 0).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        val nco_real1 = (multiplyingFactor1.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut1.bits((2*dataWidth-1), dataWidth).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt

        ioout.bits.data := Cat(nco_real1.asTypeOf(UInt(16.W)), nco_imag1.asTypeOf(UInt(16.W)))
      }
    }


    decoupled_queue(1024, interfaceIn1, interfaceOut1) := interfaceIn1.bits
    
    val last1 = RegInit(false.B)
    when(ioin1.bits.last) {last1 := true.B}.elsewhen(RegNext(ioout.bits.last)) {last1 := false.B}
    
    when(last1 && ioout.valid){ioout.bits.last := true.B}.otherwise {ioout.bits.last := false.B}

    /*val outValidCount = RegInit(UInt(12.W), 0.U)
    when(ioout.valid) {outValidCount := outValidCount + 1.U}
    when (outValidCount === 1023.U) {ioout.bits.last := true.B}
    .otherwise {ioout.bits.last := false.B}*/

  }
}



class singleNCOwithMultiplyingChain[T <: Data : Real : BinaryRepresentation]
(
  paramsPLFG1: PLFGParams[T],
  paramsNCO1: NCOParams[T],
  csrAddressPLFG1: AddressSet,
  ramAddress1: AddressSet,
  csrAddressNCO: AddressSet,
  csrAddress: AddressSet,
  beatBytes: Int
) extends LazyModule()(Parameters.empty) {

  val PLFGModule1 = LazyModule(new PLFGDspBlockMem(csrAddressPLFG1, ramAddress1, paramsPLFG1, beatBytes) {
  })

  val ncoModule1 = LazyModule(new AXI4NCOLazyModuleBlock(paramsNCO1, csrAddressNCO, beatBytes) {
  })

  val axi1to1 = LazyModule(new AXIBridge1to1Multiplying(paramsNCO1.protoOut.getWidth, csrAddress, beatBytes))

  ncoModule1.freq.get := PLFGModule1.streamNode
  axi1to1.slaveNode1 := ncoModule1.streamNode


  val ioStreamNode = BundleBridgeSink[AXI4StreamBundle]()
  ioStreamNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := axi1to1.streamNode
  val outStream = InModuleBody { ioStreamNode.makeIO() }

  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)

  val topXbar = AXI4Xbar()
  PLFGModule1.mem.get := topXbar
  axi1to1.mem.get := topXbar

  val mem = Some(AXI4IdentityNode())
  topXbar := mem.get

  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
    BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
    ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  lazy val module = new LazyModuleImp(this)

}


object singleNCOwithMultiplyingChainApp extends App {

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


  val chainModule = LazyModule(new singleNCOwithMultiplyingChain(paramsPLFG, paramsNCO1, AddressSet(0x001000, 0xFF), AddressSet(0x000000, 0x03FF), AddressSet(0x001100, 0xFF), AddressSet(0x001200, 0xFF), beatBytes) {
  })

  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "multipleNCOsChainApp"), ()=> chainModule.module) // generate verilog code
}
