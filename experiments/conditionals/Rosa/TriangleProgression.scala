import leon.Real
import Real._

object TriangleProgression {

  def triangle1(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 0.1 && a + c > b + 0.1 && b + c > a + 0.1)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def _triangle1(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 0.1 && a + c > b + 0.1 && b + c > a + 0.1)

    val s = (a + b + c)/2.0
    s * (s - a) * (s - b) * (s - c)
  }

  def triangle1_c(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0)
    val s = (a + b + c)/2.0
    s * (s - a) * (s - b) * (s - c)
  }

  def triangle2(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-2 && a + c > b + 1e-2 && b + c > a + 1e-2)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def _triangle2(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-2 && a + c > b + 1e-2 && b + c > a + 1e-2)

    val s = (a + b + c)/2.0
    s * (s - a) * (s - b) * (s - c)
  }


  def triangle3(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-3 && a + c > b + 1e-3 && b + c > a + 1e-3)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def _triangle3(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-3 && a + c > b + 1e-3 && b + c > a + 1e-3)

    val s = (a + b + c)/2.0
    s * (s - a) * (s - b) * (s - c)
  }


  def triangle4(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-4 && a + c > b + 1e-4 && b + c > a + 1e-4)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def triangle5(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-5 && a + c > b + 1e-5 && b + c > a + 1e-5)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def triangle6(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-6 && a + c > b + 1e-6 && b + c > a + 1e-6)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def triangle7(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-7 && a + c > b + 1e-7 && b + c > a + 1e-7)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def triangle8(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-8 && a + c > b + 1e-8 && b + c > a + 1e-8)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def triangle9(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-9 && a + c > b + 1e-9 && b + c > a + 1e-9)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def triangle10(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-10 && a + c > b + 1e-10 && b + c > a + 1e-10)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def triangle11(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-11 && a + c > b + 1e-11 && b + c > a + 1e-11)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def triangle12(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-12 && a + c > b + 1e-12 && b + c > a + 1e-12)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))
  }

  def _triangle12(a: Real, b: Real, c: Real): Real = {
    require(1.0 < a && a < 9.0 && 1.0 < b && b < 9.0 && 1.0 < c && c < 9.0 &&
      a + b > c + 1e-12 && a + c > b + 1e-12 && b + c > a + 1e-12)

    val s = (a + b + c)/2.0
    s * (s - a) * (s - b) * (s - c)
  }

}