package net.ollie.maths.numbers

import java.math.MathContext

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

/**
 * Precision object that allows specification to a number of significant figures, decimal places, or bytes.
 * Created by Ollie on 23/12/13.
 */
sealed trait Precision {

    require(digits >= 0)

    def digits: Int

    def apply(bd: BigDecimal)(implicit mode: RoundingMode = Precision.DEFAULT_ROUNDING): BigDecimal

    def increase: Precision = increaseBy(1)

    def increaseBy(value: Int): Precision

    def getType: Class[_ <: Precision]

    def >(that: Precision): Option[Boolean] = None

    def >=(that: Precision): Option[Boolean] = {
        if (this equals that) Some(true)
        else this > that
    }

    def equals(that: Precision): Boolean

}

object Precision {

    implicit final val DEFAULT_ROUNDING: RoundingMode = RoundingMode.HALF_UP

    implicit final val DEFAULT_PRECISION: Precision = DoublePrecision

    implicit class PrecisionApplier(val bd: BigDecimal)
            extends AnyRef {

        def to(precision: Precision): BigDecimal = precision(bd)

    }

    implicit class PrecisionConverter(val value: Int)
            extends AnyRef {

        def dp: DecimalPlaces = new DecimalPlaces(value)

        def decimalPlaces: DecimalPlaces = dp

        def sf: SignificantFigures = SignificantFigures(value)

        def significantFigures: SignificantFigures = sf

    }

    implicit def convert(mode: RoundingMode): java.math.RoundingMode = mode match {
        case RoundingMode.CEILING => java.math.RoundingMode.CEILING
        case RoundingMode.DOWN => java.math.RoundingMode.DOWN
        case RoundingMode.FLOOR => java.math.RoundingMode.FLOOR
        case RoundingMode.HALF_DOWN => java.math.RoundingMode.HALF_DOWN
        case RoundingMode.HALF_EVEN => java.math.RoundingMode.HALF_EVEN
        case RoundingMode.HALF_UP => java.math.RoundingMode.HALF_UP
        case RoundingMode.UNNECESSARY => java.math.RoundingMode.UNNECESSARY
        case RoundingMode.UP => java.math.RoundingMode.UP
    }

}

class DecimalPlaces(val digits: Int)
        extends AnyRef
        with Precision {

    require(digits >= 0)

    def apply(bd: BigDecimal)(implicit mode: RoundingMode = Precision.DEFAULT_ROUNDING) = bd.setScale(digits, mode)

    def increaseBy(value: Int) = new DecimalPlaces(this.digits + value)

    override def >(that: Precision) = that match {
        case d: DecimalPlaces => Some(this.digits > d.digits)
        case _ => super.>(that)
    }

    override def toString = digits.toString + " decimal places"

    def getType = classOf[DecimalPlaces]

    def equals(that: Precision) = that match {
        case d: DecimalPlaces => this.digits == d.digits
        case _ => false
    }

}

/**
 * Accurate to zero decimal places.
 */
object IntegerPrecision
        extends DecimalPlaces(0)

/**
 * Accurate to N significant figures.
 * @param digits
 */
class SignificantFigures(val digits: Int)
        extends AnyRef
        with Precision {

    import Precision._

    require(digits > 0)

    def apply(bd: BigDecimal)(implicit mode: RoundingMode = Precision.DEFAULT_ROUNDING) = {
        bd.setScale(digits, mode).round(new MathContext(digits, mode))
    }

    def increaseBy(value: Int) = new SignificantFigures(this.digits + value)

    override def >(that: Precision) = that match {
        case s: SignificantFigures => Some(this.digits > s.digits)
        case _ => super.>(that)
    }

    override def toString = digits.toString + " significant figures"

    def getType = classOf[SignificantFigures]

    def equals(that: Precision) = that match {
        case s: SignificantFigures => this.digits == s.digits
        case _ => false
    }

}

object SignificantFigures {

    def apply(digits: Int): SignificantFigures = new SignificantFigures(digits)

}

object SinglePrecision extends SignificantFigures(7)

object DoublePrecision extends SignificantFigures(16)

object QuadPrecision extends SignificantFigures(34)