package net.ollie.maths.functions.special

import net.ollie.maths.{Expression, Number, Variable}
import net.ollie.maths.functions.{FunctionBuilder, Represented, UnivariateFunction}
import net.ollie.maths.functions.angular.Sin
import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.numbers.constants.{Zero, One, Pi}

/**
 * Created by Ollie on 23/01/14.
 * @see http://mathworld.wolfram.com/SincFunction.html
 */
object Sinc
        extends FunctionBuilder
        with UnivariateFunction[Real, Real] {

    def apply(n: Number) = n match {
        case Zero => empty
        case re: Real => apply(re)
        case _ => ???
    }

    def apply(f: Real): Real = if (f.isEmpty) empty else new RealSinc(f)

    def normalized(expr: Expression) = apply(Pi * expr)

    def normalized(re: Real) = apply(Pi * re)

    protected[this] def create(expr: Expression) = new Sinc(expr)

    protected[special] def empty = One

}

class Sinc(val of: Expression)
        extends Represented {

    def representation = Sin(of) / of

    override def replace(variables: Map[Variable, Expression]) = {
        val d = of.replace(variables)
        if (d.isEmpty) singularityValue else Sin(of).replace(variables) / d
    }

    override def unary_-() = Expression.negate(this)

    override def isEmpty = Sin(of).isEmpty

    override def toString = s"Sinc($of)"

    def singularityValue = Sinc.empty

}

class RealSinc(val x: Real)
        extends Real
        with ApproximatelyEvaluated {

    require(!x.isEmpty)

    private lazy val empty = Sin(x).isEmpty

    def isEmpty = empty

    def doApproximatelyEvaluate(precision: Precision): BigDecimal = {
        if (isEmpty) 0
        else Sin(x).approximatelyEvaluate(precision) / x.approximatelyEvaluate(precision)
    }

    override def toString = s"Sinc($x)"

}
