import leon.Real
import Real._

object PathError {
  def test1(x: Real): Real = {
      require(-10 <= x && x <= 10)

      x * x * x
  }

  def test2(x: Real): Real = {
      require(-10 <= x && x <= 10)

      if (x < 0)
      	 x * x * x
      else
	 x * x * x
  }

  def test3(x: Real): Real = {
      require(-5.2 <= x && x <= 5.2)

      val c = 0.09651229728906250000
      c * x
  }

  def test2_full(a: Real, b: Real): Real = {
      require(0 <= a && a <= 1 && 0 <= b && b <= 1)

      if (b >= a)
      	 b / (b - a + 0.5)
      else
	 b / 0.5
  }

  def test2_then(a: Real, b: Real): Real = {
      require(0 <= a && a <= 1 && 0 <= b && b <= 1 && b >= a)
      
      b / (b - a + 0.5)
  }
}
