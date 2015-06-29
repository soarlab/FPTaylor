import leon.Real
import Real._

object PathError {
  def smartRoot(a: Real, b: Real, c: Real): Real = {
    require(3 <= a && a <= 3 && 3.5 <= b && b <= 3.5 && -2 < c && c < 2 &&
      b*b - a * c * 4.0 > 0.1)

    val discr = b*b - a * c * 4.0
    if(b*b - a*c > 10.0) {
      if(b > 0.0) c * 2.0 /(-b - sqrt(discr))
      else if(b < 0.0)  (-b + sqrt(discr))/(a * 2.0)
      else (-b + sqrt(discr))/(a * 2.0)
    }
    else {
      (-b + sqrt(discr))/(a * 2.0)
    }
  } ensuring (res => res +/- 6e-15)

  def cav10(x: Real): Real = {
    require(0 < x && x < 10)
    if (x*x - x >= 0)
      x/10
    else 
      x*x + 2
  } ensuring(res => 0 <= res && res <= 3.0 && res +/- 3.0)
  
  def squareRoot3(x: Real): Real = {
    require(0 < x && x < 10 && x +/- 1e-10 )
    if (x < 1e-5) 1 + 0.5 * x
    else sqrt(1 + x)
  } ensuring( res => res +/- 1e-10) //valid

  def squareRoot3Unknown(x: Real): Real = {
    require(0 < x && x < 10 && x +/- 1e-10 )
    if (x < 1e-4) 1 + 0.5 * x
    else sqrt(1 + x)
  } ensuring( res => res +/- 1e-10) //invalid

}
