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


class AWGFFTChainTester(   
  dut: AWGFFTChain[FixedPoint],
  csrAddressSetsPLFG: Seq[AddressSet],
  csrAddressSetsNCO: Seq[AddressSet],
  ramAddressSets: Seq[AddressSet],
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
  
  
  memWriteWord(ramAddressSets(0).base, 0x24000000)
  memWriteWord(csrAddressSetsPLFG(0).base + 2*beatBytes, 1) // frameNum
  memWriteWord(csrAddressSetsPLFG(0).base + 4*beatBytes, 1) // differentChirpsNum
  memWriteWord(csrAddressSetsPLFG(0).base + 5*beatBytes, 16) // startingPoint
  memWriteWord(csrAddressSetsPLFG(0).base + segmentNumsArrayOffset, 1) // segmentNums
  memWriteWord(csrAddressSetsPLFG(0).base + repeatedChirpNumsArrayOffset, 1)
  memWriteWord(csrAddressSetsPLFG(0).base + chirpOrdinalNumsArrayOffset, 0)
  
  memWriteWord(ramAddressSets(1).base, 0x24000000)
  memWriteWord(csrAddressSetsPLFG(1).base + 2*beatBytes, 1) // frameNum
  memWriteWord(csrAddressSetsPLFG(1).base + 4*beatBytes, 1) // differentChirpsNum
  memWriteWord(csrAddressSetsPLFG(1).base + 5*beatBytes, 8) // startingPoint
  memWriteWord(csrAddressSetsPLFG(1).base + segmentNumsArrayOffset, 1) // segmentNums
  memWriteWord(csrAddressSetsPLFG(1).base + repeatedChirpNumsArrayOffset, 1)
  memWriteWord(csrAddressSetsPLFG(1).base + chirpOrdinalNumsArrayOffset, 0)
  
  memWriteWord(ramAddressSets(2).base, 0x14000001) //0x24000000
  memWriteWord(csrAddressSetsPLFG(2).base + 2*beatBytes, 1) // frameNum
  memWriteWord(csrAddressSetsPLFG(2).base + 4*beatBytes, 1) // differentChirpsNum
  memWriteWord(csrAddressSetsPLFG(2).base + 5*beatBytes, 0) // startingPoint // 16
  memWriteWord(csrAddressSetsPLFG(2).base + segmentNumsArrayOffset, 1) // segmentNums
  memWriteWord(csrAddressSetsPLFG(2).base + repeatedChirpNumsArrayOffset, 1)
  memWriteWord(csrAddressSetsPLFG(2).base + chirpOrdinalNumsArrayOffset, 0)
  
  memWriteWord(csrAddressFFT.base, 1024)
  memWriteWord(csrAddressFFT.base + beatBytes, 1)

  memWriteWord(csrAddressSetsNCO(0).base, 1)
  memWriteWord(csrAddressSetsNCO(0).base + beatBytes, 0x1000)
  memWriteWord(csrAddressSetsNCO(1).base, 1)
  memWriteWord(csrAddressSetsNCO(1).base + beatBytes, 0x0800)
  memWriteWord(csrAddressSetsNCO(2).base, 1)
  memWriteWord(csrAddressSetsNCO(2).base + beatBytes, 0x0400)
  
  memWriteWord(csrAddressSetsPLFG(0).base + beatBytes, 0) // reset_bit
  memWriteWord(csrAddressSetsPLFG(1).base + beatBytes, 0) // reset_bit
  memWriteWord(csrAddressSetsPLFG(2).base + beatBytes, 0) // reset_bit
  memWriteWord(csrAddressSetsPLFG(0).base, 1) // enable
  memWriteWord(csrAddressSetsPLFG(1).base, 1) // enable
  memWriteWord(csrAddressSetsPLFG(2).base, 1) // enable
  
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
  
  val f1 = Figure("AWG FFT Chain Output")
  val p1 = f1.subplot(1,1,0)
  p1.legend_= (true)
  val xaxis1 = (0 until absVals.length).map(e => e.toDouble).toSeq.toArray
  p1.setXAxisIntegerTickUnits()
  p1 += plot(xaxis1, absVals.toArray, name = "FFT Absolute value")
  p1.ylim(absVals.min, absVals.max)
  p1.xlabel = "Frequency"
  p1.ylabel = "FFT values"
  f1.saveas(s"test_run_dir/awg_fft_chain.pdf")
}


class AWGFFTChainSpec extends FlatSpec with Matchers {
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
  
  val parametersForPLFGs = Seq(paramsPLFG, paramsPLFG, paramsPLFG)
  val parametersForNCOs = Seq(paramsNCO, paramsNCO, paramsNCO2)
  val csrAddressSetsForPLFGs = Seq(AddressSet(0x001000, 0xFF), AddressSet(0x001100, 0xFF), AddressSet(0x001200, 0xFF))
  val csrAddressSetsForNCOs = Seq(AddressSet(0x001300, 0xFF), AddressSet(0x001400, 0xFF), AddressSet(0x001500, 0xFF))
  val ramAddressSets = Seq(AddressSet(0x000000, 0x03FF), AddressSet(0x000400, 0x03FF), AddressSet(0x000800, 0x03FF))
  val csrAddressFFT = AddressSet(0x001600, 0xFF)

  it should "Test AWG FFT Chain" in {
    val lazyDut = LazyModule(new AWGFFTChain(parametersForPLFGs, parametersForNCOs, paramsFFT, csrAddressSetsForPLFGs, csrAddressSetsForNCOs, ramAddressSets, csrAddressFFT, numOfAWGs, queueSize, beatBytes) {
    })
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new AWGFFTChainTester(lazyDut, csrAddressSetsForPLFGs, csrAddressSetsForNCOs, ramAddressSets, csrAddressFFT, beatBytes)
    } should be (true)
  }
}
