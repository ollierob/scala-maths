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

    override def toString = s"LegendreP($l)($of)"

}

object LegendrePolynomial {

    def apply(l: Natural, z: Expression): LegendrePolynomial = l match {
        case Zero => new ZeroLegendrePolynomial(z)
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

class ZeroLegendrePolynomial(val of: Expression)
        extends ConstantPolynomial(One)
        with LegendrePolynomial {

    //could mixin Natural, but this makes negation ugly.

    def l = Zero

}

class OneLegendrePolynomial(val of: Expression)
        extends LegendrePolynomial {

    def l = One

    def representation = of

}

class SomeLegendrePolynomial(val l: Natural)(val of: Expression)
        extends LegendrePolynomial {

    require(l > One)

    def representation = ((((2 * l) - 1) * of * LegendrePolynomial(l - 1, of)) - ((l - 1) * LegendrePolynomial(l - 2, of))) / l

    override def df(x: Variable) = l * (x * LegendrePolynomial(l, x) - LegendrePolynomial(l - 1, x)) / ((x ^ 2) + 1)

}