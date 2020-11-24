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
import fft._


class SingleAWGFFTChain[T <: Data : Real : BinaryRepresentation]
(
  paramsPLFG1: PLFGParams[T],
  paramsNCO1: NCOParams[T],
  paramsFFT: FFTParams[T],
  csrAddressPLFG1: AddressSet,
  ramAddress1: AddressSet,
  csrAddressNCO: AddressSet,
  csrAddress: AddressSet,
  csrAddressFFT: AddressSet,
  beatBytes: Int
) extends LazyModule()(Parameters.empty) {

  val awgModule = LazyModule(new SingleAWG(paramsPLFG1, paramsNCO1, csrAddressPLFG1, ramAddress1, csrAddressNCO, csrAddress, beatBytes))
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


object SingleAWGFFTChainApp extends App {

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

  val chainModule = LazyModule(new SingleAWGFFTChain(paramsPLFG, paramsNCO1, paramsFFT, AddressSet(0x001000, 0xFF), AddressSet(0x000000, 0x03FF), AddressSet(0x001100, 0xFF), AddressSet(0x001200, 0xFF), AddressSet(0x001300, 0xFF), beatBytes) {
  })
  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "SingleAWGFFTChainApp"), ()=> chainModule.module) // generate verilog code
}
