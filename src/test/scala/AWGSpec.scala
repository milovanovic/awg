// SPDX-License-Identifier: Apache-2.0

package awg

import chisel3._
import chisel3.util._
import chisel3.experimental._
import dsptools._
import dsptools.numbers._
import scala.io.Source
import java.io._
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
import plfg._
import nco._


class AWGTester(   
  dut: AWG[FixedPoint]  with AXI4BlockIO,
  csrAddressSetsPLFG: Seq[AddressSet],
  csrAddressSetsNCO: Seq[AddressSet],
  ramAddressSets: Seq[AddressSet],
  beatBytes: Int
)  extends PeekPokeTester(dut.module) with AXI4MasterModel {

  override def memAXI: AXI4Bundle = dut.ioMem.get.getWrappedValue
  val segmentNumsArrayOffset = 6 * beatBytes
  val repeatedChirpNumsArrayOffset = segmentNumsArrayOffset + 2 * beatBytes
  val chirpOrdinalNumsArrayOffset = repeatedChirpNumsArrayOffset + 2 * beatBytes
  
  val returnVal = new Array[BigInt](1024)
  val returnVal1 = new Array[Int](1024)
  val real = new Array[Double](1024)
  val imag = new Array[Double](1024)
  
  memWriteWord(ramAddressSets(0).base, 0x24000000)
  memWriteWord(csrAddressSetsPLFG(0).base + 2*beatBytes, 1) // frameNum
  memWriteWord(csrAddressSetsPLFG(0).base + 4*beatBytes, 1) // differentChirpsNum
  memWriteWord(csrAddressSetsPLFG(0).base + 5*beatBytes, 8) // startingPoint
  memWriteWord(csrAddressSetsPLFG(0).base + segmentNumsArrayOffset, 1) // segmentNums
  memWriteWord(csrAddressSetsPLFG(0).base + repeatedChirpNumsArrayOffset, 1)
  memWriteWord(csrAddressSetsPLFG(0).base + chirpOrdinalNumsArrayOffset, 0)
  
  memWriteWord(ramAddressSets(1).base, 0x24000000)
  memWriteWord(csrAddressSetsPLFG(1).base + 2*beatBytes, 1) // frameNum
  memWriteWord(csrAddressSetsPLFG(1).base + 4*beatBytes, 1) // differentChirpsNum
  memWriteWord(csrAddressSetsPLFG(1).base + 5*beatBytes, 2) // startingPoint
  memWriteWord(csrAddressSetsPLFG(1).base + segmentNumsArrayOffset, 1) // segmentNums
  memWriteWord(csrAddressSetsPLFG(1).base + repeatedChirpNumsArrayOffset, 1)
  memWriteWord(csrAddressSetsPLFG(1).base + chirpOrdinalNumsArrayOffset, 0)
  
  memWriteWord(csrAddressSetsNCO(0).base, 1)
  memWriteWord(csrAddressSetsNCO(0).base + beatBytes, 0x1000)
  memWriteWord(csrAddressSetsNCO(1).base, 1)
  memWriteWord(csrAddressSetsNCO(1).base + beatBytes, 0x0800)
  
  memWriteWord(csrAddressSetsPLFG(0).base + beatBytes, 0) // reset_bit
  memWriteWord(csrAddressSetsPLFG(1).base + beatBytes, 0) // reset_bit
  memWriteWord(csrAddressSetsPLFG(0).base, 1) // enable
  memWriteWord(csrAddressSetsPLFG(1).base, 1) // enable
  
  poke(dut.outStream.ready, 1)
  step(1)
  //step(4500)
  
  var idx = 0
  var ii = 0
  
  /*while ((ii < 4500) && (idx < 1024)) {
    if((peek(dut.outStream.valid) > 0) && peek(dut.outStream.ready) > 0) {
      returnVal(idx) = peek(dut.outStream.bits.data)
      returnVal1(idx) = returnVal(idx).toInt
      real(idx) = ((returnVal1(idx) / pow(2,16)).toShort).toDouble
      imag(idx) = ((returnVal1(idx) - (real(idx).toInt * pow(2,16))).toShort).toDouble
      idx += 1
    }
    step(1)
    ii +=1
  }*/

  while ((ii < 4500) && (idx < 512)) {
    if((peek(dut.outStream.valid) > 0) && peek(dut.outStream.ready) > 0) {
      returnVal(idx) = peek(dut.outStream.bits.data)
      returnVal1(idx) = returnVal(idx).toInt
      real(idx) = ((returnVal1(idx) / pow(2,16)).toShort).toDouble
      imag(idx) = ((returnVal1(idx) - (real(idx).toInt * pow(2,16))).toShort).toDouble
      idx += 1
    }
    step(1)
    ii +=1
  }
  poke(dut.outStream.ready, 0)
  step(2)
  poke(dut.outStream.ready, 1)
  step(1)
  poke(dut.outStream.ready, 0)
  step(1)
  poke(dut.outStream.ready, 1)
  step(1)
  poke(dut.outStream.ready, 0)
  step(10)
  poke(dut.outStream.ready, 1)
  step(1)
  while ((ii < 4500) && (idx < 1024)) {
    if((peek(dut.outStream.valid) > 0) && peek(dut.outStream.ready) > 0) {
      returnVal(idx) = peek(dut.outStream.bits.data)
      returnVal1(idx) = returnVal(idx).toInt
      real(idx) = ((returnVal1(idx) / pow(2,16)).toShort).toDouble
      imag(idx) = ((returnVal1(idx) - (real(idx).toInt * pow(2,16))).toShort).toDouble
      idx += 1
    }
    step(1)
    ii +=1
  }
  
  val f1 = Figure("AWG Output")
  val p1 = f1.subplot(1,1,0)
  p1.legend_= (true)
  val xaxis1 = (0 until real.length).map(e => e.toDouble).toSeq.toArray
  p1.setXAxisIntegerTickUnits()
  p1 += plot(xaxis1, real.toArray, name = "Real value")
  p1 += plot(xaxis1, imag.toArray, name = "Imag value")
  p1.ylim(real.min, real.max)
  p1.xlabel = "Samples"
  p1.ylabel = "Output values"
  f1.saveas(s"test_run_dir/awg.pdf")
}


class AWGSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty
  val beatBytes = 4
  val numOfAWGs = 2
  val queueSize = 64

  val paramsNCO = FixedNCOParams( // pinc 8
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
    useMultiplier = true,
    numMulPipes = 1
  )
  val paramsPLFG = FixedPLFGParams(
    maxNumOfSegments = 2,
    maxNumOfDifferentChirps = 2,
    maxNumOfRepeatedChirps = 2,
    maxChirpOrdinalNum = 2,
    maxNumOfFrames = 2,
    maxNumOfSamplesWidth = 12,
    outputWidthInt = 16,
    outputWidthFrac = 0
  )
  
  val parametersForPLFGs = Seq(paramsPLFG, paramsPLFG)
  val parametersForNCOs = Seq(paramsNCO, paramsNCO)
  val csrAddressSetsForPLFGs = Seq(AddressSet(0x001000, 0xFF), AddressSet(0x001100, 0xFF))
  val csrAddressSetsForNCOs = Seq(AddressSet(0x001200, 0xFF), AddressSet(0x001300, 0xFF))
  val ramAddressSets = Seq(AddressSet(0x000000, 0x03FF), AddressSet(0x000400, 0x03FF))

  it should "Test AWG with three NCOs" in {
    val lazyDut = LazyModule(new AWG(parametersForPLFGs, parametersForNCOs, csrAddressSetsForPLFGs, csrAddressSetsForNCOs, ramAddressSets, numOfAWGs, queueSize, beatBytes) with AXI4BlockIO {
    })
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new AWGTester(lazyDut, csrAddressSetsForPLFGs, csrAddressSetsForNCOs, ramAddressSets, beatBytes)
    } should be (true)
  }
}
