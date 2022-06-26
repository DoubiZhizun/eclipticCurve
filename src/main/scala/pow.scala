import spinal.core._
import spinal.lib._

class pow(totalWidth: Int, p: BigInt) extends Component {
  require(p % 2 == 1)

  val io = new Bundle {
    val operand = slave Flow new Bundle {
      val a = UInt(totalWidth bits)
      val b = UInt(totalWidth bits)
    }

    val pow = master Flow UInt(totalWidth bits)
  }

  val outputReg = Reg(UInt(totalWidth bits))
  val tempReg = Reg(UInt(totalWidth bits))
  val cnt = Counter(totalWidth + 1)

  io.pow.payload := Mux(io.operand.b.msb, (outputReg * tempReg % p).resized, outputReg)
  io.pow.valid := cnt.willOverflow

  when(io.operand.valid) {
    cnt.increment()
    when(cnt === 0) {
      tempReg := io.operand.payload.a
      outputReg := 1
    }.otherwise {
      tempReg := (tempReg * tempReg % p).resized
      when(io.operand.b((cnt - 1).resized)) {
        outputReg := (outputReg * tempReg % p).resized
      }
    }
  }


  def getBits: Array[Boolean] = {
    var temp = p
    val output = new Array[Boolean](log2Up(temp))
    var finalTrue = 0
    for (i <- output.indices) {
      if (temp % 2 == 1) {
        output(i) = true
        finalTrue = i + 1
      } else {
        output(i) = false
      }
      temp >>= 1
    }
    output.slice(0, finalTrue)
  }
}

import spinal.core.sim._

object pow extends App {
  SimConfig.withWave.compile(new pow(6, 61)).doSimUntilVoid { dut =>
    dut.clockDomain.forkStimulus(10)

    fork {
      dut.io.operand.valid #= false
      dut.clockDomain.waitSampling(10)
      for (i <- 0 until 100) {
        dut.io.operand.valid #= true
        dut.io.operand.payload.a #= scala.util.Random.nextInt(61)
        dut.io.operand.payload.b #= scala.util.Random.nextInt(61)
        do {
          dut.clockDomain.waitSampling()
        } while (!dut.io.pow.valid.toBoolean)
        val a = dut.io.operand.payload.a.toBigInt
        val b = dut.io.operand.payload.b.toBigInt
        val gold = a.modPow(b, 61)
        val pow = dut.io.pow.payload.toBigInt
        if (pow != gold) {
          print(s"出现错误，输入为${a}和${b}，输出为${pow}，但正确结果应该是${gold}。\n")
          simFailure()
        }
      }
      simSuccess()
    }
  }
}