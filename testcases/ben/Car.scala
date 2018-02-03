import leon.real._
import RealOps._


object Car {

  def a_go(x: Real, v: Real): Real = {
    require(-10 <= x && x <= 10 && -10 <= v && v <= 10)

    if(1.01 - x - v < 0) {
      0
    } else {
      2 * (1.01 - x - v)
    }
  }

  def a_stop(x: Real, v: Real): Real = {
    require(-10 <= x && x < -0.02 && -10 <= v && v <= 10)

    v * v / (2 * (x + 0.01))
  }


  def accel(x: Real, v: Real): Real = {
    require(-10 <= x && x <= 10 && -10 <= v && v <= 10)

    if(a_go(x, v) < 10) {
      a_go(x, v)
    } else {
      a_stop(x, v)
    }
  } ensuring (res => res +/- 1e-1)

}