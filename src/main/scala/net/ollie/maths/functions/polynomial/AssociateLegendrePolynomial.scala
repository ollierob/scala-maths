package net.ollie.maths.functions.polynomial

import net.ollie.maths._
import net.ollie.maths.functions.{Represented, Modal}
import net.ollie.maths.numbers.{Integer, Natural, Zero}

/**
 * Created by Ollie on 08/01/14.
 */
trait AssociatedLegendrePolynomial
        extends Modal
        with Represented {

    override def toString = "P(" + l + "," + m + ")"

}

object AssociatedLegendrePolynomial {

    def apply(l: Natural, m: Integer, x: Expression): AssociatedLegendrePolynomial = m match {
        case Zero => LegendrePolynomial(l, x)
        case _ if m.abs > l => new EmptyAssociatedLegendrePolynomial(l, m)
        case _ => new RegularAssociatedLegendrePolynomial(l, m, x)
    }

}

/**
 * When |m| > l the polynomial is empty.
 */
class EmptyAssociatedLegendrePolynomial(val l: Natural, val m: Integer)
        extends AssociatedLegendrePolynomial
        with Empty {

    require(m.abs > l)

    protected[this] def f = Zero

    override def isEmpty = super[Empty].isEmpty

    override def variables = super[Empty].variables

}

class RegularAssociatedLegendrePolynomial(val l: Natural, val m: Integer, val x: Expression)
        extends AssociatedLegendrePolynomial {

    import net.ollie.maths.functions.polynomial.{AssociatedLegendrePolynomial => Plm}

    require(l <= m.abs)

    protected[this] def f = (((2 * l - 1) * Plm(l - 1, m, x)) - ((l + m) * Plm(l - 2, m, x))) / (l - m + 1)

}