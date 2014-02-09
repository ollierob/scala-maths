package net.ollie.maths.functions.angular

import scala.Some

import Angle._
import net.ollie.maths._
import net.ollie.maths.functions.{ExpressionBuilder, UnivariateFunction}
import net.ollie.maths.functions.numeric.{Signum, SquareRoot}
import net.ollie.maths.functions.special.Sinc
import net.ollie.maths.methods.MaclaurinSeries
import net.ollie.maths.numbers._
import org.nevec.rjm.BigDecimalMath

/**
 * Created by Ollie on 02/01/14.
 */
object Sin
        extends UnivariateFunction[Angle, Real]
        with ExpressionBuilder {

    def apply(n: Number): Number = n match {
        case Zero => empty
        case re: Real => Sin(Radians(re))
        case _ => ???
    }

    def apply(re: Real): Real = re match {
        case angle: Angle => apply(angle)
        case _ => apply(re radians)
    }

    def apply(angle: Angle) = if (angle.isEmpty) empty else new RealSin(angle)

    protected[this] def create(expr: Expression): Expression = new Sin(expr)

    protected[angular] def empty = Zero

}

private class Sin(val of: Expression)
        extends OddFunctionBuilder
        with Invertible {

    protected[this] def builder = Sin

    def isEmpty = of.isEmpty

    protected[this] def derivative(at: Expression) = Cos(at)

    def inverse = ArcSin(of)

    override def /(that: Expression) = if (that equals of) Sinc(of) else super./(that)

    override def toString = s"Sin($of)"

}

/**
 * TODO periodicity
 * @param of
 */
private class RealSin(val of: Angle)
        extends Real {

    private lazy val series = MaclaurinSeries(Sin, of.toRadians)

    protected[this] def eval(precision: Precision) = series.evaluate(precision)

    override def variables = super[Real].variables

    override def toConstant = Some(this)

    def isEmpty = of.isEmpty

    override def toString = s"Sin($of)"

}

object Cosec
        extends ExpressionBuilder {

    def apply(n: Number) = Sin(n).inverse

    protected[this] def create(expr: Expression) = 1 / Sin(expr)

    protected[this] def empty = Sin.empty.inverse

}

object ArcSin
        extends ExpressionBuilder
        with UnivariateFunction[Real, Angle] {

    def apply(n: Number) = n match {
        case re: Real => apply(re)
        case _ => ???
    }

    def apply(d: BigDecimal): Angle = apply(Real(d))

    def apply(re: Real): Angle = re match {
        case _ if re.abs < One => new RealArcSin(re) radians
        case _ if re.abs == One => Signum(re) * One radians
        case _ => Operation.undefined
    }

    protected[this] def empty = Zero

    protected[this] def create(expr: Expression) = new ArcSin(expr)

}

class ArcSin(val of: Expression)
        extends OddFunctionBuilder {

    def isEmpty = of.isEmpty

    protected[this] def builder = ArcSin

    protected[this] def derivative(x: Expression) = 1 / SquareRoot(1 - (x ^ 2))

    override def toString = s"ArcSin($of)"

}

class RealArcSin(val x: Real)
        extends Real {

    //private lazy val series = Series(nth _, Zero)

    //private def nth(n: NaturalNumber): RealNumber = BinomialCoefficient(2 * n, n) * (x ^ (2 * n + 1)) / ((4 ^ n) * (2 * n + 1))

    protected[this] def eval(precision: Precision) = BigDecimalMath.asin(x.approximatelyEvaluate(precision).underlying())

    def isEmpty = x.isEmpty

}