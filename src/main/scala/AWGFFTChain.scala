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


class AWGFFTChain[T <: Data : Real : BinaryRepresentation]
(
  paramsPLFG1: PLFGParams[T],
  paramsPLFG2: PLFGParams[T],
  paramsPLFG3: PLFGParams[T],
  paramsNCO1: NCOParams[T],
  paramsNCO2: NCOParams[T],
  paramsNCO3: NCOParams[T],
  paramsFFT: FFTParams[T],
  csrAddressPLFG1: AddressSet,
  ramAddress1: AddressSet,
  csrAddressPLFG2: AddressSet,
  ramAddress2: AddressSet,
  csrAddressPLFG3: AddressSet,
  ramAddress3: AddressSet,
  csrAddressNCO: AddressSet,
  csrAddress: AddressSet,
  csrAddressFFT: AddressSet,
  beatBytes: Int
) extends LazyModule()(Parameters.empty) {

  val awgModule = LazyModule(new AWG(paramsPLFG1, paramsPLFG2, paramsPLFG3, paramsNCO1, paramsNCO2, paramsNCO3, csrAddressPLFG1, ramAddress1, csrAddressPLFG2, ramAddress2, csrAddressPLFG3, ramAddress3, csrAddressNCO, csrAddress, beatBytes) {
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


object AWGFFTChainApp extends App {
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

  val chainModule = LazyModule(new AWGFFTChain(paramsPLFG, paramsPLFG, paramsPLFG, paramsNCO1, paramsNCO2, paramsNCO3, paramsFFT, AddressSet(0x001000, 0xFF), AddressSet(0x000000, 0x03FF), AddressSet(0x001100, 0xFF), AddressSet(0x000400, 0x03FF), AddressSet(0x001200, 0xFF), AddressSet(0x000800, 0x03FF), AddressSet(0x001300, 0xFF), AddressSet(0x001400, 0xFF), AddressSet(0x001500, 0xFF), beatBytes) {
  })
  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "AWGFFTChainApp"), ()=> chainModule.module) // generate verilog code
}
