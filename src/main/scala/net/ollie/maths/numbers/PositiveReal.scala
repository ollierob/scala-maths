package net.ollie.maths.numbers

import org.nevec.rjm.BigDecimalMath
import net.ollie.maths.{Number, Operation}

/**
 * Numbers known to be equal to or greater than zero at compile time.
 * Created by Ollie on 04/01/14.
 */
trait PositiveReal
        extends Real {

    override def abs = this

    override def inverse: PositiveReal = PositiveReal.inverse(this)

    def +(that: PositiveReal): PositiveReal = super.+(that).abs

    def *(that: PositiveReal): PositiveReal = super.*(that).abs

    def /(that: PositiveReal): PositiveReal = this * that.inverse

    def ^(that: Real): PositiveReal = that match {
        case i: Integer => this.^(that)
        case _ => PositiveReal.pow(this, that)
    }

    override def ^(that: Integer): PositiveReal = PositiveReal.pow(this, that)

    override def isStrictlyPositive = !this.isEmpty

}

object PositiveReal {

    def apply(n: Number): Option[PositiveReal] = Real(n) match {
        case Some(re) => apply(re)
        case _ => None
    }

    def apply(re: Real): Option[PositiveReal] = re match {
        case p: PositiveReal => Some(p)
        case _ if re.isEmpty || re.isStrictlyPositive => Some(re.abs)
        case _ => None
    }

    implicit def apply(int: Int): Natural = Natural(int)

    def inverse(re: PositiveReal): PositiveReal = new PositiveRealInverse(re)

    def pow(base: PositiveReal, power: Real): PositiveReal = new PositiveRealPower(base, power)

    implicit object Numeric
            extends scala.Numeric[PositiveReal] {

        def compare(x: PositiveReal, y: PositiveReal) = x compare y

        def toDouble(x: PositiveReal) = x.evaluate(DoublePrecision).toDouble

        def toFloat(x: PositiveReal) = x.evaluate(SinglePrecision).toFloat

        def toLong(x: PositiveReal) = x.evaluate(IntegerPrecision).toLong

        def toInt(x: PositiveReal) = x.evaluate(IntegerPrecision).toInt

        def fromInt(x: Int) = Natural(x)

        def negate(x: PositiveReal) = Operation.illegal(s"Cannot negate $x to a positive!")

        def times(x: PositiveReal, y: PositiveReal) = x * y

        def minus(x: PositiveReal, y: PositiveReal) = {
            val z: Real = x - y
            if (z.isStrictlyPositive) z.abs
            else Operation.illegal(s"$x -$y is not positive!")
        }

        def plus(x: PositiveReal, y: PositiveReal) = x + y

    }

}

class PositiveRealInverse(val re: PositiveReal)
        extends InverseReal(re)
        with PositiveReal

/**
 *
 * @param base
 * @param power
 * @see http://arxiv.org/abs/0908.3030v2
 */
class PositiveRealPower(val base: PositiveReal, val power: Real)
        extends PositiveReal
        with RealPower {

    protected[this] def doEvaluate(precision: Precision) = {
        println(s"EVALUATING $this AT $precision")
        BigDecimalMath.pow(base.evaluate(precision).underlying(), power.evaluate(precision).underlying())
    }

}