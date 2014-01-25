package net.ollie.maths.numbers

import java.math.MathContext

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

/**
 * Precision object that allows specification to a number of significant figures, decimal places, or bytes.
 * Created by Ollie on 23/12/13.
 */
trait Precision {

    def apply(bd: BigDecimal)(implicit mode: RoundingMode = Precision.DEFAULT_ROUNDING): BigDecimal

    def increase: Precision

    def value: Int

    def getType: Class[_ <: Precision]

    def >(that: Precision): Option[Boolean] = None

}

object Precision {

    implicit final val DEFAULT_ROUNDING: RoundingMode = RoundingMode.HALF_UP

    implicit final val DEFAULT_PRECISION: Precision = DoublePrecision

    implicit class PrecisionApplier(val bd: BigDecimal) extends AnyRef {

        def to(precision: Precision): BigDecimal = precision(bd)

    }

    implicit class PrecisionConverter(val value: Int) extends AnyRef {

        def dp = new DecimalPlaces(value)

        def decimalPlaces = dp

        def sf = new SignificantFigures(value)

        def significantFigures = sf

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

class DecimalPlaces(val value: Int)
        extends AnyRef
        with Precision {

    require(value >= 0)

    def apply(bd: BigDecimal)(implicit mode: RoundingMode = Precision.DEFAULT_ROUNDING) = bd.setScale(value, mode)

    def increase = new DecimalPlaces(value + 1)

    override def >(that: Precision) = that match {
        case d: DecimalPlaces => Some(this.value > d.value)
        case _ => super.>(that)
    }

    override def toString = value.toString + " decimal places"

    def getType = classOf[DecimalPlaces]

}

object IntegerPrecision extends DecimalPlaces(0)

class SignificantFigures(val value: Int) extends AnyRef with Precision {

    import Precision._

    require(value > 0)

    def apply(bd: BigDecimal)(implicit mode: RoundingMode = Precision.DEFAULT_ROUNDING) = {
        bd.setScale(value, mode).round(new MathContext(value, mode))
    }

    def increase = new SignificantFigures(value + 1)

    override def >(that: Precision) = that match {
        case s: SignificantFigures => Some(this.value > s.value)
        case _ => super.>(that)
    }

    override def toString = value.toString + " significant figures"

    def getType = classOf[SignificantFigures]

}

object SinglePrecision extends SignificantFigures(7)

object DoublePrecision extends SignificantFigures(16)

object QuadPrecision extends SignificantFigures(34)