package nco

import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import scala.math.{pow, sqrt}
import breeze.plot._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import chisel3.iotesters.Driver
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{FlatSpec, Matchers}
import fft._
import plfg._


class singleNCOwithMultiplyingChainTester(   
  dut: singleNCOwithMultiplyingChain[FixedPoint],
  csrAddressPLFG: AddressSet,
  ramAddress: AddressSet,
  csrAddressNCO: AddressSet,
  csrAddress: AddressSet,
  beatBytes: Int
)  extends PeekPokeTester(dut.module) with AXI4MasterModel {

    
  override def memAXI: AXI4Bundle = dut.ioMem.get.getWrappedValue
  val segmentNumsArrayOffset = 6 * beatBytes
  val repeatedChirpNumsArrayOffset = segmentNumsArrayOffset + 4 * beatBytes
  val chirpOrdinalNumsArrayOffset = repeatedChirpNumsArrayOffset + 8 * beatBytes
  
  val returnVal = new Array[BigInt](1024)
  val returnVal1 = new Array[Int](1024)
  val real = new Array[Double](1024)
  val imag = new Array[Double](1024)
  //val absVals = new Array[Double](1024)
  
  memWriteWord(ramAddress.base, 0x24000000)
  step(1)
  memWriteWord(csrAddressPLFG.base + 2*beatBytes, 1) // frameNum
  step(1)
  memWriteWord(csrAddressPLFG.base + 4*beatBytes, 1) // differentChirpsNum
  step(1)
  memWriteWord(csrAddressPLFG.base + 5*beatBytes, 16) // startingPoint
  step(1)
  memWriteWord(csrAddressPLFG.base + segmentNumsArrayOffset, 1) // segmentNums
  step(1)
  memWriteWord(csrAddressPLFG.base + repeatedChirpNumsArrayOffset, 1)
  step(1)
  memWriteWord(csrAddressPLFG.base + chirpOrdinalNumsArrayOffset, 0)
  step(1)

  
  memWriteWord(csrAddress.base, 0x0400) //0x1000
  step(1)
  
  memWriteWord(csrAddressPLFG.base + beatBytes, 0) // reset_bit
  step(1)
  memWriteWord(csrAddressPLFG.base, 1) // enable
  step(1)
  poke(dut.outStream.ready, 1)
  step(1)
  //step(4500)
  
  var idx = 0
  var ii = 0
  while ((ii < 4500) && (idx < 1024)) {
    if((peek(dut.outStream.valid) > 0) && peek(dut.outStream.ready) > 0) {
      returnVal(idx) = peek(dut.outStream.bits.data)
      returnVal1(idx) = returnVal(idx).toInt
      real(idx) = ((returnVal1(idx) / pow(2,16)).toShort).toDouble
      imag(idx) = ((returnVal1(idx) - (real(idx).toInt * pow(2,16))).toShort).toDouble
      //absVals(idx) = sqrt(pow(real(idx), 2) + pow(imag(idx), 2)).toDouble
      idx += 1
    }
    step(1)
    ii +=1
  }
  
  val f1 = Figure("Single NCO with Multiplying Chain Output")
  val p1 = f1.subplot(1,1,0)
  p1.legend_= (true)
  val xaxis1 = (0 until real.length).map(e => e.toDouble).toSeq.toArray
  p1.setXAxisIntegerTickUnits()
  p1 += plot(xaxis1, real.toArray, name = "Real value")
  p1 += plot(xaxis1, imag.toArray, name = "Imag value")
  p1.ylim(real.min, real.max)
  //p1.ylim(returnVal1.min, returnVal1.max)
  p1.xlabel = "Time"
  p1.ylabel = "Output values"
  f1.saveas(s"test_run_dir/single_nco_multiplying_chain.pdf")

}

object decoupled_queue {
  def apply[T <: Data](size: Int, in: DecoupledIO[_ <: Data], out: DecoupledIO[T], en: Bool = true.B): T = {
    requireIsHardware(in)
    requireIsHardware(out)


    val queue = Module(new Queue(chiselTypeOf(out.bits), size))
    val queueCounter = RegInit(0.U(log2Ceil(size).W))
    queueCounter := queueCounter +& in.fire() -& out.fire()

    queue.io.enq.valid := in.fire()
    assert(!queue.io.enq.valid || queue.io.enq.ready) // we control in.ready such that the queue can't fill up!

    // it can shift in one datum and shift out one datum at the same time
    in.ready := (queueCounter < size.U)
    queue.io.deq.ready := out.ready
    out.valid := queue.io.deq.valid
    out.bits := queue.io.deq.bits

    TransitName(queue.io.enq.bits, out)
  }
}


class singleNCOwithMultiplyingChainSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty

  val beatBytes = 4

  val paramsNCO = FixedNCOParams( // pinc 16
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
    
    
  it should "Test single NCO with Multiplying Chain" in {
    val lazyDut = LazyModule(new singleNCOwithMultiplyingChain(paramsPLFG, paramsNCO, AddressSet(0x001000, 0xFF), AddressSet(0x000000, 0x0FFF), AddressSet(0x001100, 0xFF), AddressSet(0x001200, 0xFF), beatBytes) {
    })
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new singleNCOwithMultiplyingChainTester(lazyDut, AddressSet(0x001000, 0xFF), AddressSet(0x000000, 0x0FFF), AddressSet(0x001100, 0xFF), AddressSet(0x001200, 0xFF), beatBytes)
    } should be (true)
  }

}
