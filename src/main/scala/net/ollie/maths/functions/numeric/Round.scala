package net.ollie.maths.functions.numeric

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers.{IntegerNumber, IntegerPrecision, RealNumber}

/**
 * Created by Ollie on 04/01/14.
 */
object Ceiling extends Round {

    def mode: RoundingMode = RoundingMode.CEILING

}

object Floor extends Round {

    def mode = RoundingMode.FLOOR

}

trait Round
        extends UnivariateFunction[RealNumber, IntegerNumber] {

    protected[this] def mode: RoundingMode

    def apply(d: BigDecimal): IntegerNumber = IntegerPrecision(d)(mode).toBigInt

    def apply(re: RealNumber): IntegerNumber = IntegerNumber(re.evaluate(IntegerPrecision)(mode).toBigInt())

}