package net.ollie.maths.numbers

import net.ollie.maths._
import net.ollie.maths.numbers.RealExponent.{ZeroToPowerZeroConvention, ZeroToPowerZeroIsOne}
import net.ollie.maths.numbers.constants.{One, Zero}
import org.nevec.rjm.BigDecimalMath

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

    def ^(that: Real): RealExponent = RealExponent(this, that)

    override def ^(that: Integer): PositiveReal = PositiveReal.pow(this, that)

    override def isStrictlyPositive = !this.isEmpty

}

object PositiveReal {

    def apply(n: Constant): Option[PositiveReal] = Real(n) match {
        case Some(re) => apply(re)
        case _ => None
    }

    def apply(re: Real): Option[PositiveReal] = re match {
        case p: PositiveReal => Some(p)
        case _ if re.isEmpty || re.isStrictlyPositive => Some(re.abs)
        case _ => None
    }

    implicit def apply(int: Int): Natural = Natural(int)

    def inverse(re: PositiveReal with Evaluable): PositiveReal = new PositiveRealInverse(re)

    def pow(base: PositiveReal with Evaluable, power: Real with Evaluable)
            (implicit convention: ZeroToPowerZeroConvention = ZeroToPowerZeroIsOne): PositiveReal = {
        (base, power) match {
            case (Zero, Zero) => convention.value.abs
            case (_, Zero) => One
            case (_, One) => base
            case _ => new PrincipalPositiveRealPower(base, power)
        }
    }

    implicit object Numeric
            extends scala.Numeric[PositiveReal] {

        def compare(x: PositiveReal, y: PositiveReal) = x compare y

        def toDouble(x: PositiveReal) = x.tryEvaluate(DoublePrecision).get.toDouble

        def toFloat(x: PositiveReal) = x.tryEvaluate(SinglePrecision).get.toFloat

        def toLong(x: PositiveReal) = x.tryEvaluate(IntegerPrecision).get.toLong

        def toInt(x: PositiveReal) = x.tryEvaluate(IntegerPrecision).get.toInt

        def fromInt(x: Int) = Natural(x)

        def negate(x: PositiveReal) = Operation.illegal(s"Cannot negate $x to a positive!")

        def times(x: PositiveReal, y: PositiveReal) = x * y

        def minus(x: PositiveReal, y: PositiveReal) = {
            val z: Real = x - y
            if (z.isStrictlyPositive) z.abs
            else Operation.illegal(s"$x -$y is not positive!")
        }

        def plus(x: PositiveReal, y: PositiveReal) = x + y

        override def parseString(str: String): Option[PositiveReal] = ???

    }

}

class PositiveRealInverse(val re: PositiveReal with Evaluable)
        extends InverseReal(re)
        with PositiveReal

/**
 *
 * @param base
 * @param power
 */
class PrincipalPositiveRealPower(val base: PositiveReal, val power: Real)
        extends PositiveReal
        with Exponentiated
        with CachedEvaluated {

    override def df(x: Variable) = super[PositiveReal].df(x)

    protected[this] def doEvaluate(precision: Precision): BigDecimal = {
        BigDecimalMath.pow(base.evaluate(precision).underlying(), power.evaluate(precision).underlying())
    }

}