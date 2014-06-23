import leon.Real
import Real._


object RosaBenchmarks {
  def approxConst(t: Real): Real = {
      require(t >< (0, 1000))
      0.1 * t
  } ensuring(res => res +/- 1e-12)

  def sum3(x0: Real, x1: Real, x2: Real): Real = {
      require(x0 >< (1, 2) && x1 >< (1, 2) && x2 >< (1, 2))
      val p0 = (x0 + x1) - x2
      val p1 = (x1 + x2) - x0
      val p2 = (x2 + x0) - x1
      (p0 + p1) + p2
  } ensuring(res => res +/- 1e-12)

  def nonlin1(t: Real): Real = {
      require(t >< (0.0, 999.0))
      t / (t + 1)
  } ensuring(res => res +/- 1e-7)

  def nonlin2(x: Real, y: Real): Real = {
      require(x >< (1.001, 2.0) && y >< (1.001, 2.0))
      val t = x * y
      (t - 1) / (t * t - 1)
  } ensuring(res => res +/- 1e-7)

}