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
import fft._
import plfg._
import nco._


class AWGParameterizedFFTChainTester(   
  dut: AWGParameterizedFFTChain[FixedPoint],
  csrAddressAWG: AddressSet,
  ramAddress: AddressSet,
  csrAddressFFT: AddressSet,
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
  val absVals = new Array[Double](1024)
  
  
  memWriteWord(ramAddress.base, 0x24000000)
  memWriteWord(csrAddressAWG.base + 2*beatBytes, 1) // frameNum
  memWriteWord(csrAddressAWG.base + 4*beatBytes, 1) // differentChirpsNum
  memWriteWord(csrAddressAWG.base + 5*beatBytes, 8) // startingPoint
  memWriteWord(csrAddressAWG.base + segmentNumsArrayOffset, 1) // segmentNums
  memWriteWord(csrAddressAWG.base + repeatedChirpNumsArrayOffset, 1)
  memWriteWord(csrAddressAWG.base + chirpOrdinalNumsArrayOffset, 0)
  
  memWriteWord(ramAddress.base + 0x0400, 0x24000000)
  memWriteWord(csrAddressAWG.base + 0x0200 + 2*beatBytes, 1) // frameNum
  memWriteWord(csrAddressAWG.base + 0x0200 + 4*beatBytes, 1) // differentChirpsNum
  memWriteWord(csrAddressAWG.base + 0x0200 + 5*beatBytes, 2) // startingPoint
  memWriteWord(csrAddressAWG.base + 0x0200 + segmentNumsArrayOffset, 1) // segmentNums
  memWriteWord(csrAddressAWG.base + 0x0200 + repeatedChirpNumsArrayOffset, 1)
  memWriteWord(csrAddressAWG.base + 0x0200 + chirpOrdinalNumsArrayOffset, 0)
  
  memWriteWord(ramAddress.base + 0x0800, 0x24000000)
  memWriteWord(csrAddressAWG.base + 0x0400 + 2*beatBytes, 1) // frameNum
  memWriteWord(csrAddressAWG.base + 0x0400 + 4*beatBytes, 1) // differentChirpsNum
  memWriteWord(csrAddressAWG.base + 0x0400 + 5*beatBytes, 16) // startingPoint
  memWriteWord(csrAddressAWG.base + 0x0400 + segmentNumsArrayOffset, 1) // segmentNums
  memWriteWord(csrAddressAWG.base + 0x0400 + repeatedChirpNumsArrayOffset, 1)
  memWriteWord(csrAddressAWG.base + 0x0400 + chirpOrdinalNumsArrayOffset, 0)
  
  memWriteWord(csrAddressFFT.base, 1024)
  memWriteWord(csrAddressFFT.base + beatBytes, 1)

  memWriteWord(csrAddressAWG.base + 0x0100, 1)
  memWriteWord(csrAddressAWG.base + 0x0100 + beatBytes, 0x1000)
  memWriteWord(csrAddressAWG.base + 0x0300, 1)
  memWriteWord(csrAddressAWG.base + 0x0300 + beatBytes, 0x0800)
  memWriteWord(csrAddressAWG.base + 0x0500, 1)
  memWriteWord(csrAddressAWG.base + 0x0500 + beatBytes, 0x0400)
  
  memWriteWord(csrAddressAWG.base + beatBytes, 0) // reset_bit
  memWriteWord(csrAddressAWG.base + 0x0200 + beatBytes, 0) // reset_bit
  memWriteWord(csrAddressAWG.base + 0x0400 + beatBytes, 0) // reset_bit
  memWriteWord(csrAddressAWG.base, 1) // enable
  memWriteWord(csrAddressAWG.base + 0x0200, 1) // enable
  memWriteWord(csrAddressAWG.base + 0x0400, 1) // enable
  
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
      absVals(idx) = sqrt(pow(real(idx), 2) + pow(imag(idx), 2)).toDouble
      idx += 1
    }
    step(1)
    ii +=1
  }
  
  val f1 = Figure("AWG Parameterized FFT Chain Output")
  val p1 = f1.subplot(1,1,0)
  p1.legend_= (true)
  val xaxis1 = (0 until absVals.length).map(e => e.toDouble).toSeq.toArray
  p1.setXAxisIntegerTickUnits()
  p1 += plot(xaxis1, absVals.toArray, name = "FFT Absolute value")
  p1.ylim(absVals.min, absVals.max)
  //p1.ylim(returnVal1.min, returnVal1.max)
  p1.xlabel = "Frequency"
  p1.ylabel = "FFT values"
  f1.saveas(s"test_run_dir/awg_parameterized_fft_chain.pdf")
}


class AWGParameterizedFFTChainSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty
  val beatBytes = 4
  val numOfAWGs = 3
  val queueSize = 64

  val paramsNCO = FixedNCOParams( // pinc 16
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
    maxNumOfSegments = 4,
    maxNumOfDifferentChirps = 8,
    maxNumOfRepeatedChirps = 8,
    maxChirpOrdinalNum = 4,
    maxNumOfFrames = 4,
    maxNumOfSamplesWidth = 12,
    outputWidthInt = 16,
    outputWidthFrac = 0
  )
  val paramsFFT = FFTParams.fixed(
    dataWidth = 16,
    twiddleWidth = 16,
    binPoint = 14,
    numPoints = 1024,
    numMulPipes = 1,
    numAddPipes = 1,
    decimType = DIFDecimType,
    useBitReverse = true,
    expandLogic = Array.fill(log2Up(1024))(0),
    keepMSBorLSB = Array.fill(log2Up(1024))(true),
    sdfRadix = "2^2"
  )

  it should "Test AWG FFT Chain" in {
    val lazyDut = LazyModule(new AWGParameterizedFFTChain(paramsPLFG, paramsNCO, paramsFFT, AddressSet(0x000100, 0xFF), AddressSet(0x002000, 0x03FF), AddressSet(0x000000, 0xFF), numOfAWGs, queueSize, beatBytes) {
    })
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new AWGParameterizedFFTChainTester(lazyDut, AddressSet(0x000100, 0xFF), AddressSet(0x002000, 0x03FF), AddressSet(0x000000, 0xFF), beatBytes)
    } should be (true)
  }
}
