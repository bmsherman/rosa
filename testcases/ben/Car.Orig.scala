import leon.real._
import RealOps._


object Car {

  def a_go(x: Real, v: Real): Real = {
    require(-10 <= x && x <= 10 && -10 <= v && v <= 10)

    val eps: Real = 0.01
    val w: Real = 1
    val T: Real = 1

    val ans: Real = 2 * (w + eps - x - v * T) / (T * T)
    if(ans < 0) {
      0
    } else {
      ans
    }
  }

  def a_stop(x: Real, v: Real): Real = {
    require(-10 <= x && x < -0.02 && -10 <= v && v <= 10)

    val eps: Real = 0.01

    v * v / (2 * (x + eps))
  }


  def accel(x: Real, v: Real): Real = {
    require(-10 <= x && x <= 10 && -10 <= v && v <= 10)

    val a_max: Real = 10
    // val a_min: Real = -10

    if(a_go(x, v) < a_max) {
      a_go(x, v)
    } else {
      a_stop(x, v)
    }
  } ensuring (res => res +/- 1e-1)

}