package object ConjuntosDifusos {

  type ConjDifuso = Int => Double

  def pertenece(elem: Int, s: ConjDifuso): Double = {
    s(elem)
  }

  def grande(d: Int, e: Int): ConjDifuso = {

    def potencia(base: Double, exp: Int): Double = {
      if (exp == 0) 1
      else base * potencia(base, exp - 1)
    }

    (x: Int) => {
      val valor = x * 1.0 / (x + d)
      potencia(valor, e)
    }

  }

  def complemento(c: ConjDifuso): ConjDifuso = {
    def noPertenece(x: Int): Double = 1.0 - c(x)
    noPertenece
  }

  def union(cd1: ConjDifuso, cd2: ConjDifuso): ConjDifuso = {
    (x: Int) => {
      if (cd1(x) > cd2(x)) cd1(x)
      else cd2(x)
    }
  }

  def interseccion(cd1: ConjDifuso, cd2: ConjDifuso): ConjDifuso = {
    (x: Int) => {
      if (cd1(x) < cd2(x)) cd1(x)
      else cd2(x)
    }
  }

  def inclusion(cd1: ConjDifuso, cd2: ConjDifuso): Boolean = {

    def verificaInclusion(actual: Int, fin: Int): Boolean = {
      if (actual > fin) true
      else if (cd1(actual) > cd2(actual)) false
      else verificaInclusion(actual + 1, fin)
    }

    verificaInclusion(0, 1000)
  }

  def igualdad(cd1: ConjDifuso, cd2: ConjDifuso): Boolean = {
    inclusion(cd1, cd2) && inclusion(cd2, cd1)
  }

}