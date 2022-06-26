

object reference {
  def inv(a: BigInt, m: BigInt): (BigInt, Boolean) = {
    //1 / a mod m
    def gcd(a: BigInt, b: BigInt): (BigInt, BigInt, Boolean) = {
      if (a == 0) {
        (0, 1, b == 1)
      } else {
        val (y, x, valid) = gcd(b.mod(a), a)
        if (!valid) {
          (0, 0, false)
        } else {
          (x - b / a * y, y, true)
        }
      }
    }

    val (x, _, valid) = gcd(a, m)
    if (!valid) {
      (0, false)
    } else {
      (x.mod(m), valid)
    }
  }

  def getST(a: BigInt): (BigInt, Int) = {
    //2 ^ t * s = a - 1
    var s = a - 1
    var t = 0
    while (s % 2 != 1) {
      s >>= 1
      t += 1
    }
    (s, t)
  }

  def getB(p: BigInt): BigInt = {
    var b = BigInt(1)
    while (b.modPow((p - 1) / 2, p) == 1) {
      b += 1
    }
    b
  }

  def sqrt(a: BigInt, p: BigInt): (BigInt, Boolean) = {
    if (p % 2 != 1 || a.modPow((p - 1) / 2, p) != 1) {
      return (0, false)
    }
    val (s, t) = getST(p)
    var x = a.modPow((s + 1) / 2, p)
    var w = a.modPow(s, p)
    val b = getB(p)
    for (i <- 0 until t - 1) {
      if (w.modPow(BigInt(1) << (t - 2 - i), p) != 1) {
        val lambda = b.modPow((1 << i) * s, p).mod(p)
        x = (x * lambda).mod(p)
        w = (w * lambda * lambda).mod(p)
      }
    }
    (x, true)
  }

  def apply(x1: BigInt, x2: BigInt, p: BigInt): (BigInt, BigInt, BigInt, BigInt, Boolean) = {

    if (x1 == x2) {
      return (0, 0, 0, 0, false)
    }

    val y1 = sqrt((x1.modPow(3, p) + 1).mod(p), p)
    if (!y1._2) {
      return (0, 0, 0, 0, false)
    }
    val y2 = sqrt((x2.modPow(3, p) + 1).mod(p), p)
    if (!y2._2) {
      return (0, 0, 0, 0, false)
    }

    val i = inv((x2 - x1).mod(p), p)
    if (!i._2) {
      return (0, 0, 0, 0, false)
    }
    val s = ((y2._1 - y1._1).mod(p) * i._1).mod(p)

    val x3 = (s * s - x2 - x1).mod(p)

    val y3 = (s * (x2 - x3) - y2._1).mod(p)

    (y1._1, y2._1, x3, y3, true)
  }
}

object invTest extends App {
  val m = 61
  for (i <- 0 until 1000) {
    var x = scala.util.Random.nextInt(m)
    var output = reference.inv(x, m)
    while (!output._2) {
      x = scala.util.Random.nextInt(m)
      output = reference.inv(x, m)
    }
    val gold = BigInt(x).modInverse(m)
    if (gold != output._1) {
      print(s"出现错误，输入为${x},输出为${output._1}，但正确结果应该是${gold}。\n")
    }
  }
}

object sqrtTest extends App {
  val m = 61
  for (i <- 0 until 10) {
    var x = scala.util.Random.nextInt(m)
    var output = reference.sqrt(x, m)
    while (!output._2) {
      x = scala.util.Random.nextInt(m)
      output = reference.sqrt(x, m)
    }
    if (output._1 * output._1 % m != x) {
      print(s"出现错误，输入为${x},输出为${output._1}，不是正确结果。\n")
    }
  }
}

object referenceTest extends App {
  val output = reference(6, 8, 61)
  print(output)
}