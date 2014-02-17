package net.ollie.maths.numbers.constants

import net.ollie.maths.functions.numeric.Ln
import net.ollie.maths.methods.{SimpsonsIntegrationMethod, Integral}
import net.ollie.maths.numbers.{Infinity, PositiveReal, Precision}

/**
 * Created by Ollie on 04/01/14.
 * @see http://mathworld.wolfram.com/Euler-MascheroniConstant.html
 */
object EulerMascheroniConstant extends PositiveReal {

    private final val E50 = BigDecimal("0.57721566490153286060651209008240243104215933593992")
    private lazy val INTEGRAL = -Integral(x => Ln(Ln(1 / x)), Zero, Infinity)(SimpsonsIntegrationMethod)

    protected[this] def doEvaluate(precision: Precision) = {
        if (precision.digits < 50) precision(E50)
        else INTEGRAL.evaluate(precision)
    }

    def isEmpty = false

    override def toString = "γ"

}
