package nco

import chisel3._
import chisel3.experimental._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import dspblocks._
import dspjunctions._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._


object DecoupledQueue {
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
        RegFieldDesc(name = "multiplyingFactor2", desc = "multiplying factor for second nco output")),
      RegField(dataWidth, multiplyingFactor3,
        RegFieldDesc(name = "multiplyingFactor3", desc = "multiplying factor for third nco output")),
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
    ioout.valid := RegNext(interfaceOut1.valid && interfaceOut2.valid && interfaceOut3.valid) && ioout.ready

    val nco_imag1 = Reg(SInt(16.W))
    val nco_real1 = Reg(SInt(16.W))
    val nco_imag2 = Reg(SInt(16.W))
    val nco_real2 = Reg(SInt(16.W))
    val nco_imag3 = Reg(SInt(16.W))
    val nco_real3 = Reg(SInt(16.W))
    
    when (interfaceOut1.fire() && interfaceOut2.fire() && interfaceOut3.fire()) {
      DspContext.withBinaryPointGrowth(0) {
        nco_imag1 := (multiplyingFactor1.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut1.bits((dataWidth-1), 0).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        nco_real1 := (multiplyingFactor1.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut1.bits((2*dataWidth-1), dataWidth).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        nco_imag2 := (multiplyingFactor2.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut2.bits((dataWidth-1), 0).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        nco_real2 := (multiplyingFactor2.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut2.bits((2*dataWidth-1), dataWidth).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        nco_imag3 := (multiplyingFactor3.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut3.bits((dataWidth-1), 0).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        nco_real3 := (multiplyingFactor3.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut3.bits((2*dataWidth-1), dataWidth).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
      }
    }

    val nco_imag_out = (nco_imag1 +& nco_imag2) +& nco_imag3
    val nco_real_out = (nco_real1 +& nco_real2) +& nco_real3
    ioout.bits.data := Cat(nco_real_out.asTypeOf(UInt(16.W)), nco_imag_out.asTypeOf(UInt(16.W)))

    DecoupledQueue(1024, interfaceIn1, interfaceOut1) := interfaceIn1.bits
    DecoupledQueue(1024, interfaceIn2, interfaceOut2) := interfaceIn2.bits
    DecoupledQueue(1024, interfaceIn3, interfaceOut3) := interfaceIn3.bits
    
    val valid1 = RegInit(false.B)
    val valid2 = RegInit(false.B)
    val valid3 = RegInit(false.B)
    when(ioin1.fire()) {valid1 := true.B}
    when(ioin2.fire()) {valid2 := true.B}
    when(ioin3.fire()) {valid3 := true.B}
    val started = valid1 && valid2 && valid3
    
    val lastCnt = RegInit(UInt(11.W), 0.U)
    val last1 = RegInit(false.B)
    val last2 = RegInit(false.B)
    val last3 = RegInit(false.B)
    when(ioin1.bits.last) {last1 := true.B}.elsewhen((lastCnt === 0.U) && ioout.valid) {last1 := false.B}
    when(ioin2.bits.last) {last2 := true.B}.elsewhen((lastCnt === 0.U) && ioout.valid) {last2 := false.B}
    when(ioin3.bits.last) {last3 := true.B}.elsewhen((lastCnt === 0.U) && ioout.valid) {last3 := false.B}
    
    when(!(last1 && last2 && last3) && started && !ioout.fire()) {
      lastCnt := lastCnt + 1.U
    }.elsewhen((last1 && last2 && last3) && started && ioout.fire()) {
      lastCnt := lastCnt - 1.U
    }
    when(last1 && last2 && last3 && ioout.valid && (lastCnt === 0.U)){ioout.bits.last := true.B}.otherwise {ioout.bits.last := false.B}
  }
}


class AXIBridge1to1Multiplying(dataWidth: Int, csrAddress: AddressSet, beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val streamNode = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters("out", n = beatBytes)))))
  val slaveNode1 = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val mem = Some(AXI4RegisterNode(address = csrAddress, beatBytes = beatBytes))

  lazy val module = new LazyModuleImp(this) {
    val ioout = streamNode.out(0)._1
    val ioin1 = slaveNode1.in(0)._1

    val multiplyingFactor1 = RegInit(UInt(dataWidth.W), 0.U)
    val fields = Seq(
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
    ioout.valid := RegNext(interfaceOut1.valid) && ioout.ready
  
    val nco_imag1 = Reg(SInt(16.W))
    val nco_real1 = Reg(SInt(16.W))
  
    when (interfaceOut1.fire()) {
      DspContext.withBinaryPointGrowth(0) {
        nco_imag1 := (multiplyingFactor1.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut1.bits((dataWidth-1), 0).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
        nco_real1 := (multiplyingFactor1.asTypeOf(FixedPoint(16.W, (dataWidth-2).BP)) context_* interfaceOut1.bits((2*dataWidth-1), dataWidth).asTypeOf(FixedPoint(dataWidth.W, (dataWidth-2).BP))).asSInt
      }
    }
    ioout.bits.data := Cat(nco_real1.asTypeOf(UInt(16.W)), nco_imag1.asTypeOf(UInt(16.W)))

    DecoupledQueue(1024, interfaceIn1, interfaceOut1) := interfaceIn1.bits
    
    val started = RegInit(false.B)
    when(ioin1.fire()) {started := true.B}
    
    val lastCnt = RegInit(UInt(11.W), 0.U)
    val last1 = RegInit(false.B)
    when(ioin1.bits.last) {last1 := true.B}.elsewhen((lastCnt === 0.U) && ioout.valid) {last1 := false.B}
    
    when(!last1 && started && !ioout.fire()) {
      lastCnt := lastCnt + 1.U
    }.elsewhen(last1 && started && ioout.fire()) {
      lastCnt := lastCnt - 1.U
    }
    when(last1 && ioout.valid && (lastCnt === 0.U)){ioout.bits.last := true.B}.otherwise {ioout.bits.last := false.B}
  }
}
