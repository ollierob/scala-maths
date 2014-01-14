package net.ollie.maths.functions.polynomial

import net.ollie.maths.{Differentiable, Variable}
import net.ollie.maths.functions.Modal
import net.ollie.maths.numbers.{IntegerNumber, NaturalNumber, Zero}

/**
 * Created by Ollie on 08/01/14.
 */
trait AssociatedLegendrePolynomial
        extends LegendrePolynomial
        with Modal {

    def m = order

    override def toString = "P(" + l + "," + m + ")"

}

object AssociatedLegendrePolynomial {

    def apply(l: NaturalNumber, m: IntegerNumber, z: Variable): LegendrePolynomial = m match {
        case Zero => LegendrePolynomial(l, z)
        case _ if m.abs > l => new EmptyAssociatedLegendrePolynomial(l, m, z)
        case _ => new RegularAssociatedLegendrePolynomial(l, m, z)
    }

}

/**
 * When |m| > l the polynomial is empty.
 */
class EmptyAssociatedLegendrePolynomial(val degree: NaturalNumber, val order: IntegerNumber, val z: Variable)
        extends AssociatedLegendrePolynomial {

    require(order.abs > degree)

    protected[this] def f = Zero

}

class RegularAssociatedLegendrePolynomial(val degree: NaturalNumber, val order: IntegerNumber, val z: Variable)
        extends AssociatedLegendrePolynomial {

    require(degree <= order.abs)

    protected[this] def f: Differentiable = ???

}