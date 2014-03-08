package net.ollie.maths.functions.polynomial

import net.ollie.maths._
import net.ollie.maths.functions.{Modal, Represented}
import net.ollie.maths.numbers.{Integer, Natural}
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 08/01/14.
 */
trait AssociatedLegendrePolynomial
        extends Modal
        with Polynomial {

    override def toString = s"LegendreP($degree, $order)($of)"

}

object AssociatedLegendrePolynomial {

    def apply(l: Int, m: Int)(x: Expression): AssociatedLegendrePolynomial = apply(Integer(l), Integer(m))(x)

    def apply(l: Integer, m: Integer)(x: Expression): AssociatedLegendrePolynomial = l match {
        case _ if l >= 0 => apply(l.abs, m)(x)
        case _ => apply(l.abs - 1, m)(x)
    }

    def apply(l: Natural, m: Integer)(x: Expression): AssociatedLegendrePolynomial = m match {
        case Zero => LegendrePolynomial(l)(x)
        case _ if m.abs > l => new EmptyAssociatedLegendrePolynomial(l, m)(x)
        case _ => new RegularAssociatedLegendrePolynomial(l, m)(x)
    }

}

/**
 * When |m| > l the polynomial is empty.
 */
private class EmptyAssociatedLegendrePolynomial(val degree: Natural, val order: Integer)(val of: Expression)
        extends AssociatedLegendrePolynomial
        with EmptyPolynomial

private class RegularAssociatedLegendrePolynomial(val degree: Natural, val order: Integer)(val of: Expression)
        extends AssociatedLegendrePolynomial {

    import net.ollie.maths.functions.polynomial.{AssociatedLegendrePolynomial => Plm}

    require(degree <= order.abs)

    def representation = {
        ((((2 * l) - 1) * Plm(degree - 1, order)(of)) - ((degree + order) * Plm(degree - 2, order)(of))) / (degree - order + 1)
    }

}