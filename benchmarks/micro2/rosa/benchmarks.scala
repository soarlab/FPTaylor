import leon.Real
import Real._

object RosaBenchmarks {
  def f1(x: Real): Real = {
    require(x >< (1, 999))
    x / (x + 1)
  }

  def sqrt_sub(x: Real): Real = {
    require(x >< (1, 1000))
    sqrt(x + 1) - sqrt(x)
  }
}