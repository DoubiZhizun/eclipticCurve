import spinal.core._
import spinal.lib._

class multiplier(totalWidth: Int, adderWidth: Int) extends Component {
  require(totalWidth % adderWidth == 0 && totalWidth > 1 && totalWidth != adderWidth)

  val io = new Bundle {
    val operand = slave Flow new Bundle {
      val a = UInt(totalWidth bits)
      val b = UInt(totalWidth bits)
    }

    val product = master Flow UInt(2 * totalWidth bits)
  }

  val productReg = Reg(UInt(2 * totalWidth bits))
  val prodCnt = Counter(totalWidth)
  val sumCnt = Counter(totalWidth / adderWidth)

  val sel = Mux(io.operand.b(prodCnt), io.operand.a, U(0, totalWidth bits))
  val sum = (productReg >> totalWidth).subdivideIn(adderWidth bits)(sumCnt) +^ sel.subdivideIn(adderWidth bits)(sumCnt) + Mux(sumCnt === 0, False, productReg(totalWidth - 1 + sumCnt * adderWidth)).asUInt
  io.product.payload := sum @@ productReg(totalWidth * 2 - 2 - adderWidth downto 0)
  io.product.valid := prodCnt.willOverflow
  when(io.operand.valid) {
    when(prodCnt === 0) {
      productReg(2 * totalWidth - 1 downto totalWidth - 1) := sel.resized
      prodCnt.increment()
    }.otherwise {
      when(sumCnt === 0) {
        productReg(totalWidth - 2 downto 0) := productReg(totalWidth - 1 downto 1)
      }
      productReg(sumCnt * adderWidth + totalWidth - 1, adderWidth + 1 bits) := sum
      sumCnt.increment()
      when(sumCnt.willOverflowIfInc) {
        prodCnt.increment()
      }
    }
  }
}

import spinal.core.sim._

object multiplier extends App {
  SimConfig.withWave.compile(new multiplier(8, 4)).doSimUntilVoid { dut =>
    dut.clockDomain.forkStimulus(10)

    fork {
      dut.io.operand.valid #= false
      dut.clockDomain.waitSampling(10)
      for (i <- 0 until 100) {
        dut.io.operand.valid #= true
        dut.io.operand.payload.randomize()
        do {
          dut.clockDomain.waitSampling()
        } while (!dut.io.product.valid.toBoolean)
        val prod = dut.io.product.payload.toBigInt
        val a = dut.io.operand.payload.a.toBigInt
        val b = dut.io.operand.payload.b.toBigInt
        if (prod != a * b) {
          print(s"出现错误，输入为${a}和${b}，输出为${prod}，但正确结果应该是${a * b}。\n")
          simFailure()
        }
      }
      simSuccess()
    }
  }
}