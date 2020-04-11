package net.ollie.maths.methods

import net.ollie.maths._
import net.ollie.maths.expressions.{Expression, Univariate}
import net.ollie.maths.numbers.{Natural, Real}
import net.ollie.maths.numbers.constants.{One, Zero}
import net.ollie.maths.functions.Represented
import net.ollie.maths.functions.hypergeometric.Gamma
import net.ollie.maths.functions.numeric.Modulo

/**
 * Created by Ollie on 19/02/14.
 * @see http://mathworld.wolfram.com/FractionalCalculus.html
 */
object Differentiate {

    def apply(expr: Expression, x: Variable): Expression = expr.df(x)

    def apply(expr: Expression, x: Variable, n: Natural): Expression = expr match {
        case m: MultiDifferentiable => m.df(x, n)
        case _ => new MultiplyDifferentiated(expr, x, n)
    }

    def apply(f: Univariate, alpha: Real): Expression = alpha match {
        case Zero => f
        case One => f.dx
        case _ => new FractionalDerivative(f, alpha)
    }

}

trait MultiDifferentiable
        extends Differentiable
        with Expression {

    def df(x: Variable): Expression = df(x, 1)

    def df(x: Variable, n: Natural): Expression

}

class MultiplyDifferentiated(val of: Expression, val x: Variable, val n: Natural)
        extends Represented {

    private lazy val deriv: Expression = {
        var i: Natural = Zero
        var dn = of
        while (i < n) {
            dn = dn.df(x)
            i = i.succ
        }
        dn
    }

    def representation = deriv

    override def toString = s"Derivative($n, $x)($of)"

}

class FractionalDerivative(val f: Univariate, val alpha: Real)
        extends Represented {

    require(!alpha.isEmpty)

    private val modAlpha = Modulo(alpha, One)

    private val x = f.variable

    private lazy val repr = Differentiate(Integrate(integrand _, 0, x), x) / Gamma(1 - modAlpha.remainder)

    private def integrand(t: Variable): Expression = {
        Differentiate(f(t), modAlpha.quotient) / ((x - t) ^ modAlpha.remainder)
    }

    def representation = repr

    override def toString = s"FractionalDerivative($alpha, $x)($f)"

}