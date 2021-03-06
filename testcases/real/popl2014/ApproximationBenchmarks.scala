import leon.real._
import RealOps._


object ApproximationBenchmarks {

  def doppler1Star(u: Real, v: Real, T: Real): Real = {
    require(-100.0 <= u && u <= 100 && 20 <= v && v <= 20000 && -30 <= T && T <= 50 &&
            u +/- 1e-7 && v +/- 1e-9 && T +/- 1e-6)
    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))
  }

  def doppler1(u: Real, v: Real, T: Real): Real = {
    require(-100.0 <= u && u <= 100 && 20 <= v && v <= 20000 && -30 <= T && T <= 50)
    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))
  } ensuring(res => res +/- 1e-12)

  def doppler2Star(u: Real, v: Real, T: Real): Real = {
    require(-125.0 <= u && u <= 125.0 && 15.0 <= v && v <= 25000 && -40 <= T && T <= 60 &&
            u +/- 1e-12 && v +/- 1e-3 && T +/- 1e-5)
    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))
  }

  def doppler2(u: Real, v: Real, T: Real): Real = {
    require(-125.0 <= u && u <= 125.0 && 15.0 <= v && v <= 25000 && -40 <= T && T <= 60)
    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))
  }

  def doppler3Star(u: Real, v: Real, T: Real): Real = {
    require(-30 <= u && u <= 120 && 320.0 <= v && v <= 20300 && -50 <= T && T <= 30 &&
            u +/- 1e-4 && v +/- 1e-5 && T +/- 1e-9)
    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))
  }

  def doppler3(u: Real, v: Real, T: Real): Real = {
    require(-30 <= u && u <= 120 && 320.0 <= v && v <= 20300 && -50 <= T && T <= 30)
    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))
  }

  def rigidBody1Star(x1: Real, x2: Real, x3: Real): Real = {
    require(-15.0 <= x1 && x1 <= 15 && -15.0 <= x2 && x2 <= 15.0 && -15.0 <= x3 && x3 <= 15 &&
            x1 +/- 1e-8 && x2 +/- 1e-8 && x3 +/- 1e-8)
    -x1*x2 - 2*x2*x3 - x1 - x3
  }

  def rigidBody1(x1: Real, x2: Real, x3: Real): Real = {
    require(-15.0 <= x1 && x1 <= 15 && -15.0 <= x2 && x2 <= 15.0 && -15.0 <= x3 && x3 <= 15)

    -x1*x2 - 2*x2*x3 - x1 - x3
  }

  def rigidBody2Star(x1: Real, x2: Real, x3: Real): Real = {
    require(-15.0 <= x1 && x1 <= 15 && -15.0 <= x2 && x2 <= 15.0 && -15.0 <= x3 && x3 <= 15 &&
            x1 +/- 1e-8 && x2 +/- 1e-8 && x3 +/- 1e-8)

    2*x1*x2*x3 + 3*x3*x3 - x2*x1*x2*x3 + 3*x3*x3 - x2
  }

  def rigidBody2(x1: Real, x2: Real, x3: Real): Real = {
    require(-15.0 <= x1 && x1 <= 15 && -15.0 <= x2 && x2 <= 15.0 && -15.0 <= x3 && x3 <= 15)

    2*x1*x2*x3 + 3*x3*x3 - x2*x1*x2*x3 + 3*x3*x3 - x2
  }

  def jetEngineStar(x1: Real, x2: Real): Real = {
    require(-5 <= x1 && x1 <= 5 && -20 <= x2 && x2 <= 5 && x1 +/- 1e-8 && x2 +/- 1e-8)

    val t = (3*x1*x1 + 2*x2 - x1)

    x1 + ((2*x1*(t/(x1*x1 + 1))*
    (t/(x1*x1 + 1) - 3) + x1*x1*(4*(t/(x1*x1 + 1))-6))*
    (x1*x1 + 1) + 3*x1*x1*(t/(x1*x1 + 1)) + x1*x1*x1 + x1 +
    3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)))
  }

  def jetEngine(x1: Real, x2: Real): Real = {
    require(-5 <= x1 && x1 <= 5 && -20 <= x2 && x2 <= 5)

    val t = (3*x1*x1 + 2*x2 - x1)

    x1 + ((2*x1*(t/(x1*x1 + 1))*
    (t/(x1*x1 + 1) - 3) + x1*x1*(4*(t/(x1*x1 + 1))-6))*
    (x1*x1 + 1) + 3*x1*x1*(t/(x1*x1 + 1)) + x1*x1*x1 + x1 +
    3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)))
  }

  def turbine1Star(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8 && v +/- 1e-7 && w +/- 1e-12 && r +/- 1e-8)

    3 + 2/(r*r) - 0.125*(3-2*v)*(w*w*r*r)/(1-v) - 4.5
  }

  def turbine1(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)

    3 + 2/(r*r) - 0.125*(3-2*v)*(w*w*r*r)/(1-v) - 4.5
  }

  def turbine2Star(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8 &&
            v +/- 1e-7 && w +/- 1e-12 && r +/- 1e-8)

    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  }

  def turbine2(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)

    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  }

  def turbine3Star(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8 &&
              v +/- 1e-7 && w +/- 1e-12 && r +/- 1e-8)

    3 - 2/(r*r) - 0.125 * (1+2*v) * (w*w*r*r) / (1-v) - 0.5
  }

  def turbine3(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)

    3 - 2/(r*r) - 0.125 * (1+2*v) * (w*w*r*r) / (1-v) - 0.5
  }

  def verhulstStar(r: Real, K: Real, x: Real): Real = {
    require(r >= 4.0 && r <= 4.0 && K >= 1.11 && K <= 1.11 && 0.1 <= x && x <= 0.3 &&
              r +/- 0.001 && K +/- 1e-5 && x +/- 1e-6)

    (r*x) / (1 + (x/K))

  }

  def verhulst(r: Real, K: Real, x: Real): Real = {
    require(r >= 4.0 && r <= 4.0 && K >= 1.11 && K <= 1.11 && 0.1 <= x && x <= 0.3)

    (r*x) / (1 + (x/K))

  }

  def predatorPreyStar(r: Real, K: Real, x: Real): Real = {
    require(r >= 4.0 && r <= 4.0 && K >= 1.11 && K <= 1.11 && 0.1 <= x && x <= 0.3 && r +/- 0.001 && K +/- 1e-5 && x +/- 1e-6)

    (r*x*x) / (1 + (x/K)*(x/K))

  }

  def predatorPrey(r: Real, K: Real, x: Real): Real = {
    require(r >= 4.0 && r <= 4.0 && K >= 1.11 && K <= 1.11 && 0.1 <= x && x <= 0.3)

    (r*x*x) / (1 + (x/K)*(x/K))

  }

  def carbonGasStar(T: Real, a: Real, b: Real, N: Real, p: Real, V: Real): Real = {
    require(T >= 300 && T <= 300 && a >= 0.401 && a <= 0.401 && b >= 42.7e-6 && b <= 42.7e-6 && N >= 1000 && N <= 1000 &&
    p >= 3.5e7 && p <= 3.5e7 && 0.1 < V && V < 0.5 && T +/- 0.01 && a +/- 1e-6 && b +/- 1e-10 && N +/- 5 && p +/- 1e-13 && V +/- 0.005)

    val k = 1.3806503e-23
    (p + a * (N / V) * (N / V)) * (V - N * b) - k * N * T

  }

  def carbonGas(T: Real, a: Real, b: Real, N: Real, p: Real, V: Real): Real = {
    require(T >= 300 && T <= 300 && a >= 0.401 && a <= 0.401 && b >= 42.7e-6 && b <= 42.7e-6 && N >= 1000 && N <= 1000 &&
    p >= 3.5e7 && p <= 3.5e7 && 0.1 < V && V < 0.5)

    val k = 1.3806503e-23
    (p + a * (N / V) * (N / V)) * (V - N * b) - k * N * T

  }

  def sine(x: Real): Real = {
    require(x > -1.57079632679 && x < 1.57079632679)
    x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0 
  }

  def sqroot(x: Real): Real = {
    require(x >= 0.0 && x < 1.0)
    1.0 + 0.5*x - 0.125*x*x + 0.0625*x*x*x - 0.0390625*x*x*x*x
  }

  def sineOrder3(x: Real): Real = {
    require(-2.0 < x && x < 2.0)
    0.954929658551372 * x -  0.12900613773279798*(x*x*x)
  }

}