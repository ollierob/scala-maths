package net.ollie.maths.functions.polynomial

import net.ollie.maths._
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{Zero, One}

/**
 * Created by Ollie on 08/01/14.
 */
trait LegendrePolynomial
        extends AssociatedLegendrePolynomial
        with Polynomial {

    override def m = Zero

    override def isEmpty = false

    override def toString = "P(" + l + ")"

}

object LegendrePolynomial {

    def apply(l: Natural, z: Expression): LegendrePolynomial = l match {
        case Zero => ZeroLegendrePolynomial
        case One => new OneLegendrePolynomial(z)
        case _ => new SomeLegendrePolynomial(l)(z)
    }

    def apply(l: Natural, re: Real): Real = {
        LegendrePolynomial(l, re.asInstanceOf[Expression]).toConstant match {
            case Some(re: Real) => re
            case _ => ???
        }
    }

}

object ZeroLegendrePolynomial
        extends LegendrePolynomial {

    //could mixin Natural, but this makes negation ugly.

    def l = Zero

    def f = One

    def evaluate = f.evaluate

    override def toString = "LegendreP(0)()"

}

class OneLegendrePolynomial(val of: Expression)
        extends LegendrePolynomial {

    def l = One

    def f = of

    override def toString = s"LegendreP(1)($of)"

}

class SomeLegendrePolynomial(val l: Natural)(val x: Expression)
        extends LegendrePolynomial {

    require(l > One)

    def f = ((((2 * l) - 1) * x * LegendrePolynomial(l - 1, x)) - ((l - 1) * LegendrePolynomial(l - 2, x))) / l

    override def df(x: Variable) = l * (x * LegendrePolynomial(l, x) - LegendrePolynomial(l - 1, x)) / ((x ^ 2) + 1)

    override def toString = s"LegendreP($l)($x)"

}