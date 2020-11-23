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


object decoupled_queue {
  def apply[T <: Data](size: Int, in: DecoupledIO[_ <: Data], out: DecoupledIO[T], en: Bool = true.B): T = {
    requireIsHardware(in)
    requireIsHardware(out)


    val queue = Module(new Queue(chiselTypeOf(out.bits), size))
    val queueCounter = RegInit(0.U(log2Ceil(size).W))
    queueCounter := queueCounter +& in.fire() -& out.fire()

    queue.io.enq.valid := in.fire()
    //assert(!queue.io.enq.valid || queue.io.enq.ready) // we control in.ready such that the queue can't fill up!

    // it can shift in one datum and shift out one datum at the same time
    in.ready := (queueCounter < size.U)
    queue.io.deq.ready := out.ready
    out.valid := queue.io.deq.valid
    out.bits := queue.io.deq.bits

    TransitName(queue.io.enq.bits, out)
  }
}


class AXIBridge3to1(dataWidth: Int, csrAddress: AddressSet,beatBytes: Int) extends LazyModule()(Parameters.empty) {
  
  require(dataWidth <= 16)
  
  val streamNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("out", n = beatBytes)))))

  val slaveNode1 = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val slaveNode2 = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val slaveNode3 = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())

  val mem = Some(AXI4RegisterNode(address = csrAddress, beatBytes = beatBytes))

  lazy val module = new LazyModuleImp(this) {
    val ioout = streamNode.out(0)._1
    val ioin1 = slaveNode1.in(0)._1
    val ioin2 = slaveNode2.in(0)._1
    val ioin3 = slaveNode3.in(0)._1
    
    val multiplyingFactor1 = RegInit(UInt(dataWidth.W), 0.U)
    val multiplyingFactor2 = RegInit(UInt(dataWidth.W), 0.U)
    val multiplyingFactor3 = RegInit(UInt(dataWidth.W), 0.U)

    val fields = Seq(
      RegField(dataWidth, multiplyingFactor1,
        RegFieldDesc(name = "multiplyingFactor1", desc = "multiplying factor for first nco output")),
      RegField(dataWidth, multiplyingFactor2,
        RegFieldDesc(name = "multiplyingFactor2", desc = "multiplying factor for first nco output")),
      RegField(dataWidth, multiplyingFactor3,
        RegFieldDesc(name = "multiplyingFactor3", desc = "multiplying factor for first nco output")),
    )

    mem.get.regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)


    val interfaceOut1 = Wire(DecoupledIO(UInt(32.W)))
    val interfaceOut2 = Wire(DecoupledIO(UInt(32.W)))
    val interfaceOut3 = Wire(DecoupledIO(UInt(32.W)))

    val interfaceIn1 = Wire(Flipped(DecoupledIO(UInt(32.W))))
    val interfaceIn2 = Wire(Flipped(DecoupledIO(UInt(32.W))))
    val interfaceIn3 = Wire(Flipped(DecoupledIO(UInt(32.W))))

    ioin1.ready := interfaceIn1.ready
    ioin2.ready := interfaceIn2.ready
    ioin3.ready := interfaceIn3.ready

    interfaceIn1.valid := ioin1.valid
    interfaceIn2.valid := ioin2.valid
    interfaceIn3.valid := ioin3.valid

    interfaceIn1.bits := ioin1.bits.data.asUInt()
    interfaceIn2.bits := ioin2.bits.data.asUInt()
    interfaceIn3.bits := ioin3.bits.data.asUInt()


    interfaceOut1.ready := ioout.ready && interfaceOut1.valid && interfaceOut2.valid && interfaceOut3.valid
    interfaceOut2.ready := ioout.ready && interfaceOut1.valid && interfaceOut2.valid && interfaceOut3.valid
    interfaceOut3.ready := ioout.ready && interfaceOut1.valid && interfaceOut2.valid && interfaceOut3.valid

    ioout.valid := interfaceOut1.valid && interfaceOut2.valid && interfaceOut3.valid

    when (interfaceOut1.fire() && interfaceOut2.fire() && interfaceOut3.fire()) {

      DspContext.withBinaryPointGrowth(0) {
        val nco_imag1 = (multiplyingFactor1.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut1.bits((dataWidth-1), 0).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        val nco_real1 = (multiplyingFactor1.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut1.bits((2*dataWidth-1), dataWidth).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        val nco_imag2 = (multiplyingFactor2.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut2.bits((dataWidth-1), 0).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        val nco_real2 = (multiplyingFactor2.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut2.bits((2*dataWidth-1), dataWidth).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        val nco_imag3 = (multiplyingFactor3.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut3.bits((dataWidth-1), 0).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        val nco_real3 = (multiplyingFactor3.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut3.bits((2*dataWidth-1), dataWidth).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt

        val nco_imag_out = (nco_imag1 +& nco_imag2) +& nco_imag3
        val nco_real_out = (nco_real1 +& nco_real2) +& nco_real3
        ioout.bits.data := Cat(nco_real_out.asTypeOf(UInt(16.W)), nco_imag_out.asTypeOf(UInt(16.W)))
      }
    }


    decoupled_queue(1024, interfaceIn1, interfaceOut1) := interfaceIn1.bits
    decoupled_queue(1024, interfaceIn2, interfaceOut2) := interfaceIn2.bits
    decoupled_queue(1024, interfaceIn3, interfaceOut3) := interfaceIn3.bits

    val last1 = RegInit(false.B)
    val last2 = RegInit(false.B)
    val last3 = RegInit(false.B)
    when(ioin1.bits.last) {last1 := true.B}.elsewhen(RegNext(ioout.bits.last)) {last1 := false.B}
    when(ioin2.bits.last) {last2 := true.B}.elsewhen(RegNext(ioout.bits.last)) {last2 := false.B}
    when(ioin3.bits.last) {last3 := true.B}.elsewhen(RegNext(ioout.bits.last)) {last3 := false.B}

    when(last1 && last2 && last3 && ioout.valid){ioout.bits.last := true.B}.otherwise {ioout.bits.last := false.B}

  }
}



class AWGNonParameterized[T <: Data : Real : BinaryRepresentation]
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

  ncoModule1.freq.get := PLFGModule1.streamNode
  ncoModule2.freq.get := PLFGModule2.streamNode
  ncoModule3.freq.get := PLFGModule3.streamNode
  axi3to1.slaveNode1 := ncoModule1.streamNode
  axi3to1.slaveNode2 := ncoModule2.streamNode
  axi3to1.slaveNode3 := ncoModule3.streamNode


  val ioStreamNode = BundleBridgeSink[AXI4StreamBundle]()
  ioStreamNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := axi3to1.streamNode
  val outStream = InModuleBody { ioStreamNode.makeIO() }

  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)

  val topXbar = AXI4Xbar()
  PLFGModule1.mem.get := topXbar
  PLFGModule2.mem.get := topXbar
  PLFGModule3.mem.get := topXbar
  axi3to1.mem.get := topXbar

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


object AWGNonParameterizedApp extends App {

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

  val chainModule = LazyModule(new AWGNonParameterized(paramsPLFG, paramsPLFG, paramsPLFG, paramsNCO1, paramsNCO2, paramsNCO3, AddressSet(0x001000, 0xFF), AddressSet(0x000000, 0x03FF), AddressSet(0x001100, 0xFF), AddressSet(0x000400, 0x03FF), AddressSet(0x001200, 0xFF), AddressSet(0x000800, 0x03FF), AddressSet(0x001300, 0xFF), AddressSet(0x001400, 0xFF), beatBytes) {
  })

  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "multipleNCOsChainApp"), ()=> chainModule.module) // generate verilog code
}
