import spinal.core._
import spinal.lib._

class adder(totalWidth: Int, adderWidth: Int) extends Component {
  require(totalWidth % adderWidth == 0 && totalWidth > 0 && totalWidth != adderWidth)

  val io = new Bundle {
    val operand = slave Flow new Bundle {
      val a = UInt(totalWidth bits)
      val b = UInt(totalWidth bits)
      val cIn = Bool()
    }

    val sum = master Flow new Bundle {
      val s = UInt(totalWidth bits)
      val cOut = Bool()
    }
  }

  val sumReg = Reg(UInt(totalWidth - adderWidth bits))
  val cOutReg = Reg(Bool())
  val cnt = Counter(totalWidth / adderWidth)

  val sum = io.operand.a.subdivideIn(adderWidth bits)(cnt) +^ io.operand.b.subdivideIn(adderWidth bits)(cnt) + Mux(cnt === 0, io.operand.cIn, cOutReg).asUInt
  io.sum.s := (sum @@ sumReg).resized
  io.sum.cOut := sum.msb
  io.sum.valid := cnt.willOverflow
  when(io.operand.valid) {
    cnt.increment()
    sumReg.subdivideIn(adderWidth bits)(cnt.resized) := sum.resized
    cOutReg := sum.msb
  }
}

import spinal.core.sim._

object adder extends App {
  SimConfig.withWave.compile(new adder(16, 4)).doSimUntilVoid { dut =>
    dut.clockDomain.forkStimulus(10)

    fork {
      dut.io.operand.valid #= false
      dut.clockDomain.waitSampling(10)
      for (i <- 0 until 100) {
        dut.io.operand.valid #= true
        dut.io.operand.payload.randomize()
        do {
          dut.clockDomain.waitSampling()
        } while (!dut.io.sum.valid.toBoolean)
        val sum = (if (dut.io.sum.cOut.toBoolean) BigInt(1) << 16 else BigInt(0)) | dut.io.sum.s.toBigInt
        val a = dut.io.operand.a.toBigInt
        val b = dut.io.operand.b.toBigInt
        val cIn = if (dut.io.operand.cIn.toBoolean) 1 else 0
        if (sum != a + b + cIn) {
          print(s"出现错误，输入为${a}、${b}和${cIn}，输出为${sum}，但正确结果应该是${a + b + cIn}。\n")
          simFailure()
        }
      }
      simSuccess()
    }
  }
}