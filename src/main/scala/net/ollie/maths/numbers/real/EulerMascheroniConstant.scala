package net.ollie.maths.numbers.real


import net.ollie.maths.numbers.{PositiveRealNumber, Precision}

/**
 * Created by Ollie on 04/01/14.
 */
object EulerMascheroniConstant extends PositiveRealNumber {

    private final val E50 = BigDecimal("0.57721566490153286060651209008240243104215933593992")

    protected[this] def eval(precision: Precision) = if (precision.value < 50) precision(E50) else ???

    def isEmpty = false

    override def toString = "γ"

}
