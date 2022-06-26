import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class sqrt(totalWidth: Int, p: BigInt) extends Component {
  require(p % 2 == 1)

  val io = new Bundle {
    val operand = slave Flow UInt(totalWidth bits)

    val sqrt = master Flow UInt(totalWidth bits)
  }

  val (s, t) = reference.getST(p)

  val pow = new pow(totalWidth, p)

  if (t == 1) {
    pow.io.operand.a := io.operand.payload
    pow.io.operand.b := (s + 1) / 2
    pow.io.operand.valid := io.operand.valid
    io.sqrt << pow.io.pow
  }

  val sqrtArea = (t != 1) generate new Area {
    val x = Reg(UInt(totalWidth bits))
    val w = Reg(UInt(totalWidth bits))
    val cnt = Counter(t - 1)

    val b = reference.getB(p)

    pow.io.operand.setIdle()
    io.sqrt.setIdle()

    val fsm = new StateMachine {
      val calculateX = new State with EntryPoint
      val calculateW = new State
      val calculateWi = new State
      val renewXW = new State
      val output = new State

      calculateX.whenIsActive {
        when(io.operand.valid) {
          pow.io.operand.valid := True
          pow.io.operand.payload.a := io.operand.payload
          pow.io.operand.payload.b := (s + 1) / 2
          when(pow.io.pow.valid) {
            x := pow.io.pow.payload
            goto(calculateW)
          }
        }
      }

      calculateW.whenIsActive {
        when(io.operand.valid) {
          pow.io.operand.valid := True
          pow.io.operand.payload.a := io.operand.payload
          pow.io.operand.payload.b := s
          when(pow.io.pow.valid) {
            w := pow.io.pow.payload
            goto(calculateWi)
          }
        }
      }

      calculateWi.whenIsActive {
        when(io.operand.valid) {
          pow.io.operand.valid := True
          pow.io.operand.payload.a := w
          switch(cnt.value) {
            for (i <- 0 until t - 1) {
              is(i) {
                pow.io.operand.payload.b := BigInt(1) << (t - 2 - i)
              }
            }
          }
          when(pow.io.pow.valid) {
            when(pow.io.pow.payload === 1) {
              cnt.increment()
              when(cnt.willOverflowIfInc) {
                goto(output)
              }
            }.otherwise {
              goto(renewXW)
            }
          }
        }
      }

      renewXW.whenIsActive {
        when(io.operand.valid) {
          switch(cnt.value) {
            for (i <- 0 until t - 1) {
              is(i) {
                val lambda = b.modPow((1 << i) * s, p) % p
                x := (x * lambda % p).resized
                w := (w * lambda * lambda % p).resized
              }
            }
          }
          cnt.increment()
          when(cnt.willOverflowIfInc) {
            goto(output)
          }.otherwise {
            goto(calculateWi)
          }
        }
      }

      output.whenIsActive {
        when(io.operand.valid) {
          io.sqrt.valid := True
          io.sqrt.payload := x
          goto(calculateX)
        }
      }
    }
  }
}

import spinal.core.sim._

object sqrt extends App {
  val p = BigInt(61)

  SimConfig.withWave.compile(new sqrt(6, p)).doSimUntilVoid { dut =>
    dut.clockDomain.forkStimulus(10)

    fork {
      dut.io.operand.valid #= false
      dut.clockDomain.waitSampling(10)
      for (i <- 0 until 100) {
        dut.io.operand.valid #= true
        var a = BigInt(log2Up(p), scala.util.Random)
        while (a >= p) {
          a = BigInt(log2Up(p), scala.util.Random)
        }
        dut.io.operand.payload #= a
        do {
          dut.clockDomain.waitSampling()
        } while (!dut.io.sqrt.valid.toBoolean)
        val gold = reference.sqrt(a, p)
        if (gold._2) {
          val sqrt = dut.io.sqrt.payload.toBigInt
          if (sqrt != gold._1) {
            print(s"出现错误，输入为${a}，输出为${sqrt}，但正确结果应该是${gold._1}。\n")
            simFailure()
          }
        }
      }
      simSuccess()
    }
  }
}