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
import fft._
import nco._


class AWGParameterizedFFTChain[T <: Data : Real : BinaryRepresentation]
(
  paramsPLFG: PLFGParams[T],
  paramsNCO: NCOParams[T],
  paramsFFT: FFTParams[T],
  csrAddressAWG: AddressSet,
  ramAddressAWG: AddressSet,
  csrAddressFFT: AddressSet,
  numOfAWGs: Int,
  queueSize: Int,
  beatBytes: Int
) extends LazyModule()(Parameters.empty) {

  val awgModule = LazyModule(new AWGParameterized(paramsPLFG, paramsNCO, csrAddressAWG, ramAddressAWG, numOfAWGs, queueSize, beatBytes) {
  })
  val fftModule = LazyModule(new AXI4FFTBlock(paramsFFT, csrAddressFFT, beatBytes))

  fftModule.streamNode := awgModule.streamNode

  val ioStreamNode = BundleBridgeSink[AXI4StreamBundle]()
  ioStreamNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := fftModule.streamNode
  val outStream = InModuleBody { ioStreamNode.makeIO() }
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)

  val topXbar = AXI4Xbar()
  awgModule.mem.get := topXbar
  fftModule.mem.get := topXbar
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


object AWGParameterizedFFTChainApp extends App {
  val beatBytes = 4
  val numOfAWGs = 2
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
    useMultiplier = false,
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

  val chainModule = LazyModule(new AWGParameterizedFFTChain(paramsPLFG, paramsNCO, paramsFFT, AddressSet(0x000100, 0xFF), AddressSet(0x002000, 0x03FF), AddressSet(0x000000, 0xFF), numOfAWGs, queueSize, beatBytes) {
  })
  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "AWGParameterizedFFTChainApp"), ()=> chainModule.module) // generate verilog code
}
