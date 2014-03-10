package net.ollie.maths.numbers.constants

import net.ollie.maths.functions.numeric.Ln
import net.ollie.maths.methods.{SimpsonsIntegrationMethod, Integrate}
import net.ollie.maths.numbers.{Infinity, Precision}
import net.ollie.maths.{Univariate, Variable}

/**
 * Created by Ollie on 04/01/14.
 * @see http://mathworld.wolfram.com/Euler-MascheroniConstant.html
 */
object EulerMascheroniConstant
        extends PositiveNamedReal {

    private final val E50 = BigDecimal("0.57721566490153286060651209008240243104215933593992")
    private lazy val INTEGRAL = -Integrate(fn _, Zero, Infinity)(SimpsonsIntegrationMethod)

    private def fn(x: Variable): Univariate = Ln(Ln(1 / x))

    def evaluate(precision: Precision) = {
        if (precision.digits < 50) precision(E50)
        else INTEGRAL.evaluate(precision)
    }

    override def toString = "γ"

}
