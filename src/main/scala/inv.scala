import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class inv(totalWidth: Int, p: BigInt) extends Component {
  require(totalWidth > 0 && p % 2 == 1)

  val io = new Bundle {
    val operand = slave Flow UInt(totalWidth bits)

    val inv = master Flow UInt(totalWidth bits)
  }

  val stack = Mem(UInt(totalWidth bits), 1 << log2Up(totalWidth))
  val stackPtr = CounterUpDown(1 << log2Up(totalWidth))

  val operandReg = Reg(Vec(UInt(totalWidth bits), 2)) //y、x或a、b

  io.inv.valid := False
  io.inv.payload := (operandReg(0) + p) % p

  val fsm = new StateMachine {
    val waitForData = new State with EntryPoint
    val goIn = new State
    val goOut = new State

    waitForData.whenIsActive {
      when(io.operand.valid) {
        operandReg := Vec(io.operand.payload, U(p, totalWidth bits))
        goto(goIn)
      }
    }

    goIn.whenIsActive {
      when(io.operand.valid) {
        when(operandReg(0) === 0) {
          operandReg(0) := 0
          operandReg(1) := 1
          goto(goOut)
        }.otherwise {
          operandReg(0) := operandReg(1) % operandReg(0)
          operandReg(1) := operandReg(0)
          stack(stackPtr) := operandReg(1) / operandReg(0)
          stackPtr.increment()
        }
      }
    }

    goOut.whenIsActive {
      when(io.operand.valid) {
        when(stackPtr =/= 0) {
          operandReg(0) := operandReg(1) - (stack(stackPtr - 1) * operandReg(0)).resize(totalWidth)
          operandReg(1) := operandReg(0)
          stackPtr.decrement()
        }.otherwise {
          io.inv.valid := True
          goto(waitForData)
        }
      }
    }
  }
}

import spinal.core.sim._

object inv extends App {
  SimConfig.withWave.compile(new inv(8, 61)).doSimUntilVoid { dut =>
    dut.clockDomain.forkStimulus(10)

    fork {
      dut.io.operand.valid #= false
      dut.clockDomain.waitSampling(10)
      for (i <- 0 until 100) {
        dut.io.operand.valid #= true
        dut.io.operand.payload #= scala.util.Random.nextInt(60) + 1
        do {
          dut.clockDomain.waitSampling()
        } while (!dut.io.inv.valid.toBoolean)
        val a = dut.io.operand.payload.toBigInt
        val gold = reference.inv(a, 61)
        if (gold._2) {
          val inv = dut.io.inv.payload.toBigInt
          if (inv != gold._1) {
            print(s"出现错误，输入为${a}，输出为${inv}，但正确结果应该是${gold._1}。\n")
            simFailure()
          }
        }
      }
      simSuccess()
    }
  }
}