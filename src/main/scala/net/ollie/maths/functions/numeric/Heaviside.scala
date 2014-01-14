package net.ollie.maths.functions.numeric

import net.ollie.maths._
import net.ollie.maths.functions.{DifferentiableExpressionBuilder, UnivariateFunction}
import net.ollie.maths.numbers.{One, RealNumber, Zero}

/**
 * Created by Ollie on 10/01/14.
 */
object Heaviside
        extends UnivariateFunction[RealNumber, RealNumber]
        with DifferentiableExpressionBuilder {

    def apply(n: Number): Number = n match {
        case re: RealNumber => apply(re)
        case _ => ???
    }

    def apply(f: RealNumber): RealNumber = f match {
        case Zero => empty
        case _ if f.isStrictlyPositive => One
        case _ => Zero
    }

    protected[this] def create(expr: Expression) = new Heaviside(expr)

    protected[this] def create(diff: Differentiable) = new DifferentiableHeaviside(diff)

    override def toString = "Heaviside(?)"

    private final val HALF = One / 2

    protected[this] def empty = HALF

}

class Heaviside(val expr: Expression)
        extends AnyRef
        with Composite {

    def of = expr

    def isEmpty = false

    protected[this] def at(n: Number) = Heaviside(n)

    protected[this] def apply(expr: Expression) = Heaviside(expr)

    override def toString = s"Heaviside($of)"

}

class DifferentiableHeaviside(val diff: Differentiable)
        extends Heaviside(diff)
        with DifferentiableComposite {

    override def of = diff

    protected[this] def df(of: Differentiable) = DiracDelta(of)

}