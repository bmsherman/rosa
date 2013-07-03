

import leon.Real
import Real._

object Physics {

  def verhulst(r: Real, K: Real, x: Real): Real = {
    require(r >= 4.0 && r <= 4.0 && K >= 1.11 && K <= 1.11 && x.in(0.1, 0.3) &&
      noise(r, 0.001) && noise(K, 1e-5) && noise(x, 1e-6))

    (r*x) / (1 + (x/K))

  } ensuring (res => res <= 0.0)

  def predatorPrey(r: Real, K: Real, x: Real): Real = {
    require(r >= 4.0 && r <= 4.0 && K >= 1.11 && K <= 1.11 && x.in(0.1, 0.3) &&
      noise(r, 0.001) && noise(K, 1e-5) && noise(x, 1e-6))

    (r*x*x) / (1 + (x/K)*(x/K))

  }  ensuring (res => res <= 0.0)


  def carbonGas(T: Real, a: Real, b: Real, N: Real, p: Real, V: Real): Real = {
    require(T >= 300 && T <= 300 && a >= 0.401 && a <= 0.401 && b >= 42.7e-6 && b <= 42.7e-6 && N >= 1000 && N <= 1000 &&
    p >= 3.5e7 && p <= 3.5e7 && V.in(0.1, 0.5) &&
    noise(T, 0.01) && noise(a, 1e-6) && noise(b, 1e-10) && noise(N, 5) &&
    noise(p, 1e-13) && noise(V, 0.005))

    val k = 1.3806503e-23
    (p + a * (N / V) * (N / V)) * (V - N * b) - k * N * T

  }  ensuring (res => res <= 0.0)


}