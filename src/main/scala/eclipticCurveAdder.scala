import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class eclipticCurveAdder(totalWidth: Int, adderWidth: Int, p: BigInt) extends Component {

  val io = new Bundle {
    val operand = slave Flow new Bundle {
      val x1 = UInt(totalWidth bits)
      val x2 = UInt(totalWidth bits)
    }

    val sum = master Flow new Bundle {
      val y1 = UInt(totalWidth bits)
      val y2 = UInt(totalWidth bits)
      val x3 = UInt(totalWidth bits)
      val y3 = UInt(totalWidth bits)
    }
  }

  val y1Reg = Reg(UInt(totalWidth bits))
  val y2Reg = Reg(UInt(totalWidth bits))
  val x3Reg = Reg(UInt(totalWidth bits))
  val y3Reg = Reg(UInt(totalWidth bits))
  val sReg = Reg(UInt(totalWidth bits))
  val yCnt = Counter(2)

  io.sum.valid := False
  io.sum.y1 := y1Reg
  io.sum.y2 := y2Reg
  io.sum.x3 := x3Reg
  io.sum.y3.assignDontCare()

  val multiplier = new multiplier(totalWidth, adderWidth)
  val adder = new adder(totalWidth, adderWidth)
  val inv = new inv(totalWidth, p)
  val sqrt = new sqrt(totalWidth, p)

  multiplier.io.operand.setIdle()
  adder.io.operand.setIdle()
  inv.io.operand.setIdle()
  sqrt.io.operand.setIdle()

  val fsm = new StateMachine {
    val calculateXSquare = new State with EntryPoint
    val calculateXCube = new State
    val calculateYSquare = new State
    val calculateY = new State
    val calculateX2SubX1 = new State
    val calculateInvX2SubX1 = new State
    val calculateY2SubY1 = new State
    val calculateS = new State
    val calculateSSquare = new State
    val calculateSSquareSubX2 = new State
    val calculateX3 = new State
    val calculateX2SubX3 = new State
    val calculateSX2SubX3 = new State
    val calculateY3 = new State

    calculateXSquare.whenIsActive {
      when(io.operand.valid) {
        multiplier.io.operand.a := Mux(yCnt.lsb, io.operand.x2, io.operand.x1)
        multiplier.io.operand.b := Mux(yCnt.lsb, io.operand.x2, io.operand.x1)
        multiplier.io.operand.valid := True
        when(multiplier.io.product.valid) {
          when(yCnt.lsb) {
            y2Reg := (multiplier.io.product.payload % p).resized
          }.otherwise {
            y1Reg := (multiplier.io.product.payload % p).resized
          }
          goto(calculateXCube)
        }
      }
    }

    calculateXCube.whenIsActive {
      when(io.operand.valid) {
        multiplier.io.operand.a := Mux(yCnt.lsb, y2Reg, y1Reg)
        multiplier.io.operand.b := Mux(yCnt.lsb, io.operand.x2, io.operand.x1)
        multiplier.io.operand.valid := True
        when(multiplier.io.product.valid) {
          when(yCnt.lsb) {
            y2Reg := (multiplier.io.product.payload % p).resized
          }.otherwise {
            y1Reg := (multiplier.io.product.payload % p).resized
          }
          goto(calculateYSquare)
        }
      }
    }

    calculateYSquare.whenIsActive {
      when(io.operand.valid) {
        adder.io.operand.a := Mux(yCnt.lsb, y2Reg, y1Reg)
        adder.io.operand.b := 1
        adder.io.operand.cIn := False
        adder.io.operand.valid := True
        when(adder.io.sum.valid) {
          when(yCnt.lsb) {
            y2Reg := adder.io.sum.s % p
          }.otherwise {
            y1Reg := adder.io.sum.s % p
          }
          goto(calculateY)
        }
      }
    }

    calculateY.whenIsActive {
      when(io.operand.valid) {
        sqrt.io.operand.payload := Mux(yCnt.lsb, y2Reg, y1Reg)
        sqrt.io.operand.valid := True
        when(sqrt.io.sqrt.valid) {
          when(yCnt.lsb) {
            y2Reg := sqrt.io.sqrt.payload
            goto(calculateX2SubX1)
          }.otherwise {
            y1Reg := sqrt.io.sqrt.payload
            goto(calculateXSquare)
          }
          yCnt.increment()
        }
      }
    }

    calculateX2SubX1.whenIsActive {
      when(io.operand.valid) {
        adder.io.operand.a := io.operand.x2
        adder.io.operand.b := ~io.operand.x1
        adder.io.operand.cIn := True
        adder.io.operand.valid := True
        when(adder.io.sum.valid) {
          x3Reg := (adder.io.sum.s + p) % p
          goto(calculateInvX2SubX1)
        }
      }
    }

    calculateInvX2SubX1.whenIsActive {
      when(io.operand.valid) {
        inv.io.operand.payload := x3Reg
        inv.io.operand.valid := True
        when(inv.io.inv.valid) {
          x3Reg := inv.io.inv.payload
          goto(calculateY2SubY1)
        }
      }
    }

    calculateY2SubY1.whenIsActive {
      when(io.operand.valid) {
        adder.io.operand.a := y2Reg
        adder.io.operand.b := ~y1Reg
        adder.io.operand.cIn := True
        adder.io.operand.valid := True
        when(adder.io.sum.valid) {
          sReg := (adder.io.sum.s + p) % p
          goto(calculateS)
        }
      }
    }

    calculateS.whenIsActive {
      when(io.operand.valid) {
        multiplier.io.operand.a := x3Reg
        multiplier.io.operand.b := sReg
        multiplier.io.operand.valid := True
        when(multiplier.io.product.valid) {
          sReg := (multiplier.io.product.payload % p).resized
          goto(calculateSSquare)
        }
      }
    }

    calculateSSquare.whenIsActive {
      when(io.operand.valid) {
        multiplier.io.operand.a := sReg
        multiplier.io.operand.b := sReg
        multiplier.io.operand.valid := True
        when(multiplier.io.product.valid) {
          x3Reg := (multiplier.io.product.payload % p).resized
          goto(calculateSSquareSubX2)
        }
      }
    }

    calculateSSquareSubX2.whenIsActive {
      when(io.operand.valid) {
        adder.io.operand.a := x3Reg
        adder.io.operand.b := ~io.operand.x2
        adder.io.operand.cIn := True
        adder.io.operand.valid := True
        when(adder.io.sum.valid) {
          x3Reg := (adder.io.sum.s + p) % p
          goto(calculateX3)
        }
      }
    }

    calculateX3.whenIsActive {
      when(io.operand.valid) {
        adder.io.operand.a := x3Reg
        adder.io.operand.b := ~io.operand.x1
        adder.io.operand.cIn := True
        adder.io.operand.valid := True
        when(adder.io.sum.valid) {
          x3Reg := (adder.io.sum.s + p) % p
          goto(calculateX2SubX3)
        }
      }
    }

    calculateX2SubX3.whenIsActive {
      when(io.operand.valid) {
        adder.io.operand.a := io.operand.x2
        adder.io.operand.b := ~x3Reg
        adder.io.operand.cIn := True
        adder.io.operand.valid := True
        when(adder.io.sum.valid) {
          y3Reg := (adder.io.sum.s + p) % p
          goto(calculateSX2SubX3)
        }
      }
    }

    calculateSX2SubX3.whenIsActive {
      when(io.operand.valid) {
        multiplier.io.operand.a := sReg
        multiplier.io.operand.b := y3Reg
        multiplier.io.operand.valid := True
        when(multiplier.io.product.valid) {
          y3Reg := (multiplier.io.product.payload % p).resized
          goto(calculateY3)
        }
      }
    }

    calculateY3.whenIsActive {
      when(io.operand.valid) {
        adder.io.operand.a := y3Reg
        adder.io.operand.b := ~y2Reg
        adder.io.operand.cIn := True
        adder.io.operand.valid := True
        when(adder.io.sum.valid) {
          io.sum.valid := True
          io.sum.y3 := (adder.io.sum.s + p) % p
          goto(calculateXSquare)
        }
      }
    }
  }
}

import spinal.core.sim._

object eclipticCurveAdder extends App {
  //val p = BigInt("01ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000001", 16)
  val p = BigInt(1073741825)
  //超过这个数字仿真器就有崩掉的可能

  SimConfig.withWave.compile(new eclipticCurveAdder(384, 64, p)).doSimUntilVoid { dut =>
    dut.clockDomain.forkStimulus(10)

    fork {
      dut.io.operand.valid #= false
      dut.clockDomain.waitSampling(10)
      for (i <- 0 until 100) {
        dut.io.operand.valid #= true
        var x1 = BigInt(log2Up(p), scala.util.Random)
        while (x1 >= p) {
          x1 = BigInt(log2Up(p), scala.util.Random)
        }
        var x2 = BigInt(log2Up(p), scala.util.Random)
        while (x2 >= p) {
          x2 = BigInt(log2Up(p), scala.util.Random)
        }
        dut.io.operand.x1 #= x1
        dut.io.operand.x2 #= x2
        do {
          dut.clockDomain.waitSampling()
        } while (!dut.io.sum.valid.toBoolean)
        val gold = reference(x1, x2, p)
        if (gold._5) {
          val y1 = dut.io.sum.y1.toBigInt
          val y2 = dut.io.sum.y2.toBigInt
          val x3 = dut.io.sum.x3.toBigInt
          val y3 = dut.io.sum.y3.toBigInt
          if (gold._1 != y1 || gold._2 != y2 || gold._3 != x3 || gold._4 != y3) {
            print("出现错误，输入为：\n" +
              s"x1 = ${x1}\n" +
              s"x2 = ${x2}\n" +
              "输出为：\n" +
              s"y1 = ${y1}\n" +
              s"y2 = ${y2}\n" +
              s"x3 = ${x3}\n" +
              s"y3 = ${y3}\n" +
              "但正确结果应该是：\n" +
              s"y1 = ${gold._1}\n" +
              s"y2 = ${gold._2}\n" +
              s"x3 = ${gold._3}\n" +
              s"y3 = ${gold._4}\n"
            )
            simFailure()
          }
        }
      }
      simSuccess()
    }
  }
}