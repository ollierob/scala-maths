package net.ollie.maths.functions.hypergeometric

import net.ollie.maths.Expression
import net.ollie.maths.functions.{Represented, SymmetricBivariateFunction}
import net.ollie.maths.methods.Product
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.{ComplexInfinity, Complex}
import net.ollie.maths.numbers.constants.{Zero, One}
import java.util.Objects

/**
 * Created by Ollie on 22/01/14.
 * @see http://mathworld.wolfram.com/BetaFunction.html
 */
object Beta
        extends SymmetricBivariateFunction[Real, Complex] {

    def apply(x: Expression, y: Expression): Expression = new Beta(x, y)

    def apply(x: Real, y: Real): Complex = (x, y) match {
        case (Zero, Zero) => empty
        case (n1: Natural, n2: Natural) => apply(n1, n2)
        case _ => new RealBeta(x, y)
    }

    def apply(n1: Natural, n2: Natural) = Gamma(n1) * Gamma(n2) / Gamma(n1 + n2.decr)

    protected[this] def empty = ComplexInfinity

}

class Beta(val x: Expression, val y: Expression)
        extends Represented {

    def f = Gamma(x) * Gamma(y) / Gamma(x + y)

    override def equals(expression: Expression) = expression match {
        case b: Beta if x == b.x && y == b.y => true
        case b: Beta if x == b.y && y == b.x => true
        case _ => super.equals(expression)
    }

    override def hashCode = Objects.hash(x, y)

    override def toString = s"Beta($x, $y)"

}

class RealBeta(override val x: Real, override val y: Real)
        extends Beta(x, y)
        with Real {

    private val series: Real = (x + y) * Product(nth _, One) / (x * y)

    private def nth(n: Natural): Real = (1 + ((x * y) / (n * (x + y + n)))).inverse

    protected[this] def doEvaluate(precision: Precision) = series.evaluate(precision)

    override def toConstant = super[Real].toConstant

    override def variables = super[Real].variables

}