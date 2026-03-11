import ConjuntosDifusos._

def muchoMayorQue(a: Int, m: Int): ConjDifuso = {
  def mma(x: Int): Double = {
    if (x <= a) 0.0
    else if (x > a && x <= m) (x - a).toDouble / (m - a).toDouble
    else 1.0
  }
  mma
}

def cercanosA(a: Int, k: Int): ConjDifuso = {
  def ca(x: Int): Double = {
    val q = 1.0 + k * (x - a) * (x - a)
    1.0 / q
  }
  ca
}

// CONJUNTOS DIFUSOS
val muchoMayorQue4_10 = muchoMayorQue(4, 10)
val cercanosA1_2 = cercanosA(1, 2)
val muchoMayorQue5_14 = muchoMayorQue(5, 14)





/**
 * EJERCICIO COMPLEMENTO
 */
// COMPLEMENTOS
val complementoMuchoMayorQue4_10 = complemento(muchoMayorQue4_10)
val complementoCercanosA1_2 = complemento(cercanosA1_2)

// PRUEBAS DE COMPLEMENTO
complementoMuchoMayorQue4_10(5)
complementoMuchoMayorQue4_10(3)
complementoMuchoMayorQue4_10(1)
complementoCercanosA1_2(3)
complementoCercanosA1_2(4)



/**
 * EJERCICIO INCLUSION
 */
inclusion(muchoMayorQue4_10, muchoMayorQue4_10)
inclusion(muchoMayorQue4_10, muchoMayorQue5_14)
inclusion(muchoMayorQue5_14, muchoMayorQue4_10)
inclusion(complementoMuchoMayorQue4_10, complementoCercanosA1_2)
inclusion(muchoMayorQue4_10, complementoMuchoMayorQue4_10)



/**
 * EJERCICIO IGUALDAD
 */
igualdad(muchoMayorQue4_10, muchoMayorQue4_10)
igualdad(cercanosA1_2, cercanosA1_2)
igualdad(muchoMayorQue4_10, muchoMayorQue5_14)
igualdad(muchoMayorQue4_10, cercanosA1_2)
igualdad(complementoMuchoMayorQue4_10, complementoMuchoMayorQue4_10)