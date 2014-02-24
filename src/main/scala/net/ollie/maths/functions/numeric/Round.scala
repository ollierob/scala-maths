package net.ollie.maths.functions.numeric

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers.{Integer, IntegerPrecision, Real}
import net.ollie.maths.numbers.Precision._

/**
 * Created by Ollie on 04/01/14.
 */
object Ceiling extends Round {

    def roundingMode: RoundingMode = RoundingMode.CEILING

}

object Floor extends Round {

    def roundingMode = RoundingMode.FLOOR

}

object Nearest extends Round {

    def roundingMode = RoundingMode.HALF_EVEN;

}

sealed trait Round
        extends UnivariateFunction[Real, Integer] {

    protected[this] def roundingMode: RoundingMode

    def apply(d: BigDecimal): Integer = IntegerPrecision(d)(roundingMode).toBigInt

    def apply(re: Real): Integer = {
        val bd = re.evaluate(Round.ONE_DP)
        Integer(bd.setScale(0, roundingMode).toBigInt())
    }

}

object Round {

    protected val ONE_DP = 1 dp

}