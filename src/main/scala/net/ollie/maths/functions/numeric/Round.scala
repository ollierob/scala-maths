package net.ollie.maths.functions.numeric

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers.{Integer, IntegerPrecision, Real}

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
        extends UnivariateFunction[Real, Integer] {

    protected[this] def mode: RoundingMode

    def apply(d: BigDecimal): Integer = IntegerPrecision(d)(mode).toBigInt

    def apply(re: Real): Integer = Integer(re.evaluate(IntegerPrecision)(mode).toBigInt())

}