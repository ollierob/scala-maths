package net.ollie.maths.functions.polynomial


import net.ollie.maths._
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 08/01/14.
 */
trait LegendrePolynomial
        extends AssociatedLegendrePolynomial
        with Polynomial {

    override def m = Zero

    override def df(x: Variable) = l * (x * LegendrePolynomial(l, x) - LegendrePolynomial(l - 1, x)) / ((x ^ 2) + 1)

    override def isEmpty = false

    override def toString = "P(" + l + ")"

}

object LegendrePolynomial {

    def apply(l: NaturalNumber, z: Variable): LegendrePolynomial = l match {
        case Zero => ZeroLegendrePolynomial
        case One => new OneLegendrePolynomial(z)
        case _ => new SomeLegendrePolynomial(l)(z)
    }

    def apply(l: NaturalNumber, number: RealNumber): RealNumber = apply(l, number.asInstanceOf[Expression]).toConstant match {
        case Some(re: RealNumber) => re
        case _ => ???
    }

    def apply(l: NaturalNumber, expression: Expression): Expression = {
        val z = new Variable("$z")
        apply(l, z).replace(z, expression)
    }

    def apply(l: NaturalNumber, expression: Differentiable): Differentiable = apply(l, expression) match {
        case d: Differentiable => d
        case _ => ???
    }

}

object ZeroLegendrePolynomial
        extends LegendrePolynomial
        with DifferentiablePolynomial
        with NaturalNumber {

    def l = Zero

    protected[this] def f = One

    def evaluate = f.evaluate

    override def toConstant = super[NaturalNumber].toConstant

    override def variables = super[NaturalNumber].variables

}

sealed trait VariateLegendrePolynomial
        extends LegendrePolynomial
        with Univariate {

    override def toString = super[LegendrePolynomial].toString + "(" + variable + ")"

    override def variables = super[Univariate].variables

}

class OneLegendrePolynomial(val variable: Variable)
        extends VariateLegendrePolynomial {

    def l = One

    protected[this] def f = variable

}

class SomeLegendrePolynomial(val l: NaturalNumber)(val x: Variable)
        extends VariateLegendrePolynomial {

    require(l > One)

    def variable = x

    protected[this] def f = (((2 * l - 1) * x * LegendrePolynomial(l - 1, x)) - ((l - 1) * LegendrePolynomial(l - 2, x)))

}