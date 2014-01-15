package net.ollie.maths.numbers.real

import scala.math.BigDecimal.RoundingMode

import net.ollie.maths.numbers._

/**
 * Created by Ollie on 18/12/13.
 */
object EulersNumber extends PositiveRealNumber {

    private final val E50 = BigDecimal("2.71828182845904523536028747135266249775724709369995")

    def nthTerm(n: NaturalNumber): PositiveRealNumber = 1 / (n !)

    def isEmpty = false

    protected[this] def eval(precision: Precision)(implicit mode: RoundingMode.RoundingMode) = if (precision.value < 50) precision(E50) else ???

    override def toString = "e"

}