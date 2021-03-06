

import leon.real._
import RealOps._

/* From:
  Numerical Methods with Worked Examples, C. Woodford and C. Phillips, 2012
*/
object Turbine {

  def turbine1(v: Real, w: Real, r: Real): Real = {
    require(v >< (-4.5, -0.3) && w >< (0.4, 0.9) && r >< (3.8, 7.8) && v +/- 1e-7 && w +/- 1e-12 && r +/- 1e-8)

    3 + 2/(r*r) - 0.125*(3-2*v)*(w*w*r*r)/(1-v) - 4.5
  }

  def turbine2(v: Real, w: Real, r: Real): Real = {
    require(v >< (-4.5, -0.3) && w >< (0.4, 0.9) && r >< (3.8, 7.8) && v +/- 1e-7 && w +/- 1e-12 && r +/- 1e-8)

    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  }

  def turbine3(v: Real, w: Real, r: Real): Real = {
    require(v >< (-4.5, -0.3) && w >< (0.4, 0.9) && r >< (3.8, 7.8) && v +/- 1e-7 && w +/- 1e-12 && r +/- 1e-8)

    3 - 2/(r*r) - 0.125 * (1+2*v) * (w*w*r*r) / (1-v) - 0.5
  }

  def turbine1_roundoff(v: Real, w: Real, r: Real): Real = {
    require(v >< (-4.5, -0.3) && w >< (0.4, 0.9) && r >< (3.8, 7.8))

    3 + 2/(r*r) - 0.125*(3-2*v)*(w*w*r*r)/(1-v) - 4.5
  }

  def turbine2_roundoff(v: Real, w: Real, r: Real): Real = {
    require(v >< (-4.5, -0.3) && w >< (0.4, 0.9) && r >< (3.8, 7.8))

    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  }

  def turbine3_roundoff(v: Real, w: Real, r: Real): Real = {
    require(v >< (-4.5, -0.3) && w >< (0.4, 0.9) && r >< (3.8, 7.8))

    3 - 2/(r*r) - 0.125 * (1+2*v) * (w*w*r*r) / (1-v) - 0.5
  }

}