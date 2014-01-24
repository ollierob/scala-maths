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

    override def isEmpty = false

    override def toString = "P(" + l + ")"

}

object LegendrePolynomial {

    def apply(l: NaturalNumber, z: Expression): LegendrePolynomial = l match {
        case Zero => ZeroLegendrePolynomial
        case One => new OneLegendrePolynomial(z)
        case _ => new SomeLegendrePolynomial(l)(z)
    }

    def apply(l: NaturalNumber, re: RealNumber): RealNumber = {
        LegendrePolynomial(l, re.asInstanceOf[Expression]).toConstant match {
            case Some(re: RealNumber) => re
            case _ => ???
        }
    }

}

object ZeroLegendrePolynomial
        extends LegendrePolynomial
        with NaturalNumber {

    def l = Zero

    protected[this] def f = One

    def evaluate = f.evaluate

    override def toConstant = super[NaturalNumber].toConstant

    override def variables = super[NaturalNumber].variables

    override def toString = "LegendreP(0)()"

}

class OneLegendrePolynomial(val of: Expression)
        extends LegendrePolynomial {

    def l = One

    protected[this] def f = of

    override def toString = s"LegendreP(1)($of)"

}

class SomeLegendrePolynomial(val l: NaturalNumber)(val x: Expression)
        extends LegendrePolynomial {

    require(l > One)

    protected[this] def f = ((((2 * l) - 1) * x * LegendrePolynomial(l - 1, x)) - ((l - 1) * LegendrePolynomial(l - 2, x))) / l

    override def df(x: Variable) = l * (x * LegendrePolynomial(l, x) - LegendrePolynomial(l - 1, x)) / ((x ^ 2) + 1)

    override def toString = s"LegendreP($l)($x)"

}