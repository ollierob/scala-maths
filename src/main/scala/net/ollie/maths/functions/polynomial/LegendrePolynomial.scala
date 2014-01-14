package net.ollie.maths.functions.polynomial


import net.ollie.maths._
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 08/01/14.
 */
trait LegendrePolynomial
        extends Polynomial {

    def l = degree

    override def df(x: Variable) = l * (x * LegendrePolynomial(l, x) - LegendrePolynomial(l - 1, x)) / ((x ^ 2) + 1)

    override def isEmpty = false

    override def toString = "P(" + l + ")"

}

object LegendrePolynomial {

    def apply(l: NaturalNumber, z: Variable): LegendrePolynomial = l match {
        case Zero => ZeroLegendrePolynomial
        case One => new OneLegendrePolynomial(z)
        case _ => new SomeLegendrePolynomial(l, z)
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
        with NaturalNumber {

    def degree = Zero

    protected[this] def f = One

    def evaluate = f.evaluate

    override def toConstant = super[NaturalNumber].toConstant

    override def variables = super[NaturalNumber].variables

}

sealed trait VariateLegendrePolynomial
        extends LegendrePolynomial {

    def x: Variable

    override def variables = Set(x)

    override def toString = super[LegendrePolynomial].toString + "(" + x + ")"

}

class OneLegendrePolynomial(val x: Variable)
        extends VariateLegendrePolynomial {

    def degree = One

    protected[this] def f = x

}

class SomeLegendrePolynomial(val degree: NaturalNumber, val x: Variable)
        extends VariateLegendrePolynomial {

    require(degree > One)

    protected[this] def f = (((2 * l - 1) * x * LegendrePolynomial(l - 1, x)) - ((l - 1) * LegendrePolynomial(l - 2, x)))

}