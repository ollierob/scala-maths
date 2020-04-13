package net.ollie.maths.numbers

import java.math.MathContext

import net.ollie.maths.numbers.Precision._
import net.ollie.utils.BigDecimals

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

/**
 * Precision object that allows specification to a number of significant figures, decimal places, or bytes.
 * Created by Ollie on 23/12/13.
 */
sealed trait Precision {

    def digits: Natural

    def apply(bd: BigDecimal)(implicit mode: RoundingMode = DEFAULT_ROUNDING): BigDecimal

    def applyTo(bd: BigDecimal, fn: (BigDecimal, MathContext) => BigDecimal)(implicit mode: RoundingMode = DEFAULT_ROUNDING): BigDecimal

    def increase: Precision = increaseBy(1)

    def increaseBy(value: Natural): Precision

    lazy val doubled = increaseBy(digits)

    def within(bd1: BigDecimal, bd2: BigDecimal): Boolean

    def >(that: Precision): Option[Boolean] = None

    def <(that: Precision): Option[Boolean] = that > this

    def >=(that: Precision): Option[Boolean] = {
        if (this equals that) Some(true)
        else this > that
    }

    override def equals(obj: Any) = obj.isInstanceOf[Precision] && this.equals(obj.asInstanceOf[Precision])

    override def hashCode = digits.hashCode

    def equals(that: Precision): Boolean

}

object Precision {

    implicit final val DEFAULT_ROUNDING: RoundingMode = RoundingMode.HALF_UP

    implicit final val DEFAULT_PRECISION: Precision = DoublePrecision

    implicit class PrecisionApplier(val bd: BigDecimal) {

        def to(precision: Precision): BigDecimal = precision(bd)

    }

    implicit class PrecisionConverter(val value: Int) {

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

class DecimalPlaces(val digits: Natural)
    extends Precision {

    private lazy val tolerance = BigDecimal(Math.pow(10, -digits.requireInt))
    private lazy val defaultContext = new MathContext(1 + digits.requireInt, DEFAULT_ROUNDING)

    def apply(bd: BigDecimal)(implicit mode: RoundingMode = DEFAULT_ROUNDING) = {
        bd.setScale(digits.toInt.get, mode)
    }

    override def applyTo(bd: BigDecimal, fn: (BigDecimal, MathContext) => BigDecimal)(implicit mode: RoundingMode) = {
        val firstAttempt = fn(bd, defaultContext)
        val d = BigDecimals.nonDecimalDigits(firstAttempt)
        if (d > 1) fn(bd, new MathContext(digits.requireInt + d, mode)) else firstAttempt
    }

    def increaseBy(value: Natural) = new DecimalPlaces(this.digits + value)

    override def within(bd1: BigDecimal, bd2: BigDecimal) = {
        (bd1 - bd2).abs < tolerance
    }

    override def >(that: Precision) = that match {
        case d: DecimalPlaces => Some(this.digits > d.digits)
        case _ => super.>(that)
    }

    override def toString = digits.toString + " decimal places"

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
 *
 * @param digits
 */
class SignificantFigures(val digits: Natural)
    extends Precision {

    require(digits.isPositive)

    private val digitsI = digits.toInt.get
    private lazy val tenPowI = BigDecimal(10 ^ digitsI)

    def apply(bd: BigDecimal)(implicit mode: RoundingMode = Precision.DEFAULT_ROUNDING) = {
        bd.setScale(digitsI, mode).round(toMathContext(mode))
    }

    override def applyTo(bd: BigDecimal, fn: (BigDecimal, MathContext) => BigDecimal)(implicit mode: RoundingMode) = {
        fn(bd, toMathContext(mode))
    }

    private def toMathContext(mode: RoundingMode) = new MathContext(digitsI, mode)

    def increaseBy(value: Natural) = new SignificantFigures(this.digits + value)

    override def within(bd1: BigDecimal, bd2: BigDecimal): Boolean = {
        val delta = (bd1 - bd2).abs
        digits(delta) < digits
    }

    private def digits(d: BigDecimal): Int = {
        val s = d.underlying().toPlainString
        val c = s.indexOf('.')
        if (c < 0) s.length
        else s.length - 1
    }

    override def >(that: Precision) = that match {
        case s: SignificantFigures => Some(this.digits > s.digits)
        case _ => super.>(that)
    }

    override def toString = digits.toString + " significant figures"

    def equals(that: Precision) = that match {
        case s: SignificantFigures => this.digits == s.digits
        case _ => false
    }

}

object SignificantFigures {

    def apply(digits: Int): SignificantFigures = new SignificantFigures(digits)

}

object SinglePrecision
    extends SignificantFigures(7)

object DoublePrecision
    extends SignificantFigures(16)

object QuadPrecision
    extends SignificantFigures(34)