/* Original file:
https://github.com/malyzajko/rosa/blob/master/testcases/real-testcases/popl2014/ApproximationBenchmarks.scala
*/
import leon.Real
import Real._


object ApproximationBenchmarks {
  def doppler1(u: Real, v: Real, T: Real): Real = {
    require(u >< (-100, 100) && v >< (20, 20000) && T >< (-30, 50))
    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))
  } ensuring(res => res +/- 1e-12)

  def doppler2(u: Real, v: Real, T: Real): Real = {
    require(u >< (-125, 125) && v >< (15, 25000) && T >< (-40, 60))
    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))
  } ensuring(res => res +/- 1e-1)

  def doppler3(u: Real, v: Real, T: Real): Real = {
    require(u >< (-30, 120) && v >< (320, 20300) && T >< (-50, 30))
    val t1 = 331.4 + 0.6 * T
    (- (t1) *v) / ((t1 + u)*(t1 + u))
  } ensuring(res => res +/- 1e-1)

  def rigidBody1(x1: Real, x2: Real, x3: Real): Real = {
    require(x1 >< (-15, 15) && x2 >< (-15, 15) && x3 >< (-15, 15))

    -x1*x2 - 2*x2*x3 - x1 - x3
  } ensuring(res => res +/- 1e-1)

  def rigidBody2(x1: Real, x2: Real, x3: Real): Real = {
    require(x1 >< (-15, 15) && x2 >< (-15, 15) && x3 >< (-15, 15))

    2*x1*x2*x3 + 3*x3*x3 - x2*x1*x2*x3 + 3*x3*x3 - x2
  } ensuring(res => res +/- 1e-1)

  def verhulst(r: Real, K: Real, x: Real): Real = {
    require(r >= 4.0 && r <= 4.0 && K >= 1.11 && K <= 1.11 && x >< (0.1, 0.3))

    (r*x) / (1 + (x/K))
  } ensuring(res => res +/- 1e-1)

  def predatorPrey(r: Real, K: Real, x: Real): Real = {
    require(r >= 4.0 && r <= 4.0 && K >= 1.11 && K <= 1.11 && x >< (0.1, 0.3))

    (r*x*x) / (1 + (x/K)*(x/K))
  } ensuring(res => res +/- 1e-1)

  def jetEngine(x1: Real, x2: Real): Real = {
    require(x1 >< (-5, 5) && x2 >< (-20, 5))

    val t = (3*x1*x1 + 2*x2 - x1)

    x1 + ((2*x1*(t/(x1*x1 + 1))*
    (t/(x1*x1 + 1) - 3) + x1*x1*(4*(t/(x1*x1 + 1))-6))*
    (x1*x1 + 1) + 3*x1*x1*(t/(x1*x1 + 1)) + x1*x1*x1 + x1 +
    3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)))
  } ensuring(res => res +/- 1e-1)

  def turbine1(v: Real, w: Real, r: Real): Real = {
    require(v >< (-4.5, -0.3) && w >< (0.4, 0.9) && r >< (3.8, 7.8))

    3 + 2/(r*r) - 0.125*(3-2*v)*(w*w*r*r)/(1-v) - 4.5
  } ensuring(res => res +/- 1e-1)

  def turbine2(v: Real, w: Real, r: Real): Real = {
    require(v >< (-4.5, -0.3) && w >< (0.4, 0.9) && r >< (3.8, 7.8))

    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 1e-1)

 def turbine3(v: Real, w: Real, r: Real): Real = {
    require(v >< (-4.5, -0.3) && w >< (0.4, 0.9) && r >< (3.8, 7.8))

    3 - 2/(r*r) - 0.125 * (1+2*v) * (w*w*r*r) / (1-v) - 0.5
  } ensuring(res => res +/- 1e-1)

  def carbonGas(T: Real, a: Real, b: Real, N: Real, p: Real, V: Real): Real = {
    require(T >= 300 && T <= 300 && a >= 0.401 && a <= 0.401 && b >= 42.7e-6 && b <= 42.7e-6 && N >= 1000 && N <= 1000 &&
    p >= 3.5e7 && p <= 3.5e7 && V >< (0.1, 0.5))

    val k = 1.3806503e-23
    (p + a * (N / V) * (N / V)) * (V - N * b) - k * N * T
  } ensuring(res => res +/- 1e-1)

  def sine(x: Real): Real = {
    require(x > -1.57079632679 && x < 1.57079632679)
    x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0 
  } ensuring(res => res +/- 1e-1)

  def sqroot(x: Real): Real = {
    require(x >= 0.0 && x < 1.0)
    1.0 + 0.5*x - 0.125*x*x + 0.0625*x*x*x - 0.0390625*x*x*x*x
  } ensuring(res => res +/- 1e-1)

  def sineOrder3(x: Real): Real = {
    require(-2.0 < x && x < 2.0)
    0.954929658551372 * x -  0.12900613773279798*(x*x*x)
  } ensuring(res => res +/- 1e-1)

}