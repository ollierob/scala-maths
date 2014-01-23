package net.ollie.maths.functions.special

import net.ollie.maths.{Expression, Number}
import net.ollie.maths.functions.{ExpressionBuilder, Represented, UnivariateFunction}
import net.ollie.maths.functions.angular.Sin
import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers.{One, Precision, RealNumber, Zero}

/**
 * Created by Ollie on 23/01/14.
 */
object Sinc
        extends ExpressionBuilder
        with UnivariateFunction[RealNumber, RealNumber] {

    def apply(n: Number) = n match {
        case Zero => empty
        case re: RealNumber => apply(re)
        case _ => ???
    }

    def apply(f: RealNumber): RealNumber = new RealSinc(f)

    protected[this] def create(expr: Expression) = new Sinc(expr)

    protected[this] def empty = One

}

class Sinc(val of: Expression)
        extends Represented {

    protected[this] def f = Sin(of) / of

    override def toString = s"Sinc($of)"

}

class RealSinc(val x: RealNumber)
        extends RealNumber
        with ApproximatelyEvaluated {

    require(!x.isEmpty)

    private lazy val empty = Sin(x).isEmpty

    def isEmpty = empty

    protected[this] def approx(precision: Precision): BigDecimal = if (isEmpty) 0 else Sin(x).approximatelyEvaluate(precision) / x.approximatelyEvaluate(precision)

}
