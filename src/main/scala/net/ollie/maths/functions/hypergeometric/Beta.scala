package net.ollie.maths.functions.hypergeometric

import net.ollie.maths.Expression
import net.ollie.maths.functions.{Represented, SymmetricBivariateFunction}
import net.ollie.maths.methods.Product
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.{ComplexInfinity, ComplexNumber}

/**
 * Created by Ollie on 22/01/14.
 * @see http://mathworld.wolfram.com/BetaFunction.html
 */
object Beta
        extends SymmetricBivariateFunction[RealNumber, ComplexNumber] {

    def apply(x: Expression, y: Expression): Expression = new Beta(x, y)

    def apply(x: RealNumber, y: RealNumber): ComplexNumber = (x, y) match {
        case (Zero, Zero) => empty
        case (n1: NaturalNumber, n2: NaturalNumber) => apply(n1, n2)
        case _ => new RealBeta(x, y)
    }

    def apply(n1: NaturalNumber, n2: NaturalNumber) = Gamma(n1) * Gamma(n2) / Gamma(n1 + n2.decr)

    protected[this] def empty = ComplexInfinity

}

class Beta(val x: Expression, val y: Expression)
        extends Represented {

    protected[this] def f = Gamma(x) * Gamma(y) / Gamma(x + y)

    override def hashCode = 17 * x.hashCode + 33 * y.hashCode

    override def equals(expression: Expression) = expression match {
        case b: Beta if x == b.x && y == b.y => true
        case b: Beta if x == b.y && y == b.x => true
        case _ => super.equals(expression)
    }

    override def toString = s"Beta($x, $y)"

}

class RealBeta(override val x: RealNumber, override val y: RealNumber)
        extends Beta(x, y)
        with RealNumber {

    private val series: RealNumber = (x + y) * Product(nth _, One) / (x * y)

    private def nth(n: NaturalNumber): RealNumber = (1 + ((x * y) / (n * (x + y + n)))).inverse

    protected[this] def eval(precision: Precision) = series.evaluate(precision)

    override def toConstant = super[RealNumber].toConstant

    override def variables = super[RealNumber].variables

}