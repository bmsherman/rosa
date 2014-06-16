/* Copyright 2013 EPFL, Lausanne */

package leon
package real

import java.math.{BigInteger}
import real.Rational.{double2Fraction,zero}

object Precision {

  // Tests whether this rational can be represented without roundoff errors
  // Since we don't know which precision we may test, returns, for now, true only for integers
  /*def isExact(r: Rational, precision: Precision): Boolean = precision match {
    case FPPrecision(bits) => FixedPointFormat(bits)
  }  //r.isWhole
  */
  // Does not check for out of range, assuming this has been done before
  // is representable if the denominator is a power of two
  def isExactInFloats(r: Rational): Boolean = {
    if (r.isWhole) {
      true
    } else if (r.d.isValidInt) {
      val n = r.d.toInt
      (n > 0) && ((n & (n - 1)) == 0)
    } else {
      false
    }
  }

  def getUnitRoundoff(precision: Precision): Rational = (precision: @unchecked) match {
    case Float32 => Rational(new BigInt(new BigInteger("1")), new BigInt(new BigInteger("2")).pow(23))
    case Float64 => Rational(new BigInt(new BigInteger("1")), new BigInt(new BigInteger("2")).pow(53))
    case DoubleDouble => Rational(new BigInt(new BigInteger("1")), new BigInt(new BigInteger("2")).pow(105))
    case QuadDouble => Rational(new BigInt(new BigInteger("1")), new BigInt(new BigInteger("2")).pow(211))
  }

  def roundoff(r: Rational, machineEps: Rational): Rational = {
    machineEps * Rational.abs(r)
  }

  def roundoff(r: Rational, precision: Precision): Rational = precision match {
    case FPPrecision(bits) =>
      FixedPointFormat.getFormat(r, bits).quantError
    case _ => 
      val machineEps = getUnitRoundoff(precision)
      machineEps * Rational.abs(r)
  }

  def roundoff(range: RationalInterval, machineEps: Rational): Rational = {
    import Rational._
    val maxAbs = max(abs(range.xlo), abs(range.xhi))
    // Without scaling this can return fractions with very large numbers
    // TODO: try scaling the result
    val simplifiedMax = Rational.scaleToIntsUp(maxAbs)
    machineEps * simplifiedMax
  }
}


sealed abstract class Precision {
    def range: (Rational, Rational)
    def minNormal: Rational
  }
  case object Float32 extends Precision {
    val range: (Rational, Rational) = {
      val rationalMaxValue = double2Fraction(Float.MaxValue)
      (-Rational(rationalMaxValue._1, rationalMaxValue._2), Rational(rationalMaxValue._1, rationalMaxValue._2))
    }
    val minNormal: Rational = {
      val rationalMinNormal = double2Fraction(java.lang.Float.MIN_NORMAL)
      Rational(rationalMinNormal._1, rationalMinNormal._2)
    }
  }
  case object Float64 extends Precision {
    val range: (Rational, Rational) = {
      val rationalMaxValue = double2Fraction(Double.MaxValue)
      (-Rational(rationalMaxValue._1, rationalMaxValue._2), Rational(rationalMaxValue._1, rationalMaxValue._2))
    }
    val minNormal: Rational = {
      val rationalMinNormal = double2Fraction(java.lang.Double.MIN_NORMAL)
      Rational(rationalMinNormal._1, rationalMinNormal._2)
    }
  }

  case object DoubleDouble extends Precision {
    val range: (Rational, Rational) = {
      val rationalMaxValue = double2Fraction(Double.MaxValue)
      (-Rational(rationalMaxValue._1, rationalMaxValue._2), Rational(rationalMaxValue._1, rationalMaxValue._2))
    }
    val minNormal: Rational = {
      val rationalMinNormal = double2Fraction(math.pow(2, -969))
      Rational(rationalMinNormal._1, rationalMinNormal._2)
    }
    // 2.0041683600089728e-292;  // = 2^(-1022 + 53) = 2^(-969)
  }
  case object QuadDouble extends Precision {
    val range: (Rational, Rational) = {
      val rationalMaxValue = double2Fraction(Double.MaxValue)
      (-Rational(rationalMaxValue._1, rationalMaxValue._2), Rational(rationalMaxValue._1, rationalMaxValue._2))
    }
    val minNormal: Rational = {
      val rationalMinNormal = double2Fraction(math.pow(2, -863))
      Rational(rationalMinNormal._1, rationalMinNormal._2)
    }
     //1.6259745436952323e-260; // = 2^(-1022 + 3*53) = 2^(-863)
  }
  case class FPPrecision(bitlength: Int) extends Precision {
    val range: (Rational, Rational) = FixedPointFormat(true, bitlength, 0, false).range
    val minNormal: Rational = zero // dummy
  }

