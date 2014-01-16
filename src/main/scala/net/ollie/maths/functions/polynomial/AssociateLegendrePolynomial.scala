package net.ollie.maths.functions.polynomial

import net.ollie.maths._
import net.ollie.maths.functions.Modal
import net.ollie.maths.numbers.{IntegerNumber, NaturalNumber, Zero}

/**
 * Created by Ollie on 08/01/14.
 */
trait AssociatedLegendrePolynomial
        extends Modal
        with DifferentiableRepresented {

    def l: NaturalNumber

    def degree = l

    def m: IntegerNumber

    def order = m

    override def toString = "P(" + l + "," + m + ")"

}

object AssociatedLegendrePolynomial {

    def apply(l: NaturalNumber, m: IntegerNumber, x: Variable): AssociatedLegendrePolynomial = m match {
        case Zero => LegendrePolynomial(l, x)
        case _ if m.abs > l => new EmptyAssociatedLegendrePolynomial(l, m)
        case _ => new RegularAssociatedLegendrePolynomial(l, m)(x)
    }

}

/**
 * When |m| > l the polynomial is empty.
 */
class EmptyAssociatedLegendrePolynomial(val l: NaturalNumber, val m: IntegerNumber)
        extends AssociatedLegendrePolynomial
        with Empty {

    require(order.abs > degree)

    protected[this] def f = Zero

    override def isEmpty = super[Empty].isEmpty

    override def variables = super[Empty].variables

}

class RegularAssociatedLegendrePolynomial(val l: NaturalNumber, val m: IntegerNumber)(val x: Variable)
        extends AssociatedLegendrePolynomial
        with Univariate {

    import net.ollie.maths.functions.polynomial.{AssociatedLegendrePolynomial => Plm}

    require(degree <= order.abs)

    override def variables = super[Univariate].variables

    def variable = x

    protected[this] def f = (((2 * l - 1) * Plm(l - 1, m, x)) - ((l + m) * Plm(l - 2, m, x))) / (l - m + 1)

}