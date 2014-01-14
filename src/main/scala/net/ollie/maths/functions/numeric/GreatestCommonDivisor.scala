package net.ollie.maths.functions.numeric

import net.ollie.maths.functions.BivariateFunction
import net.ollie.maths.numbers.{IntegerNumber, Zero}

/**
 * Created by Ollie on 04/01/14.
 */
object GreatestCommonDivisor extends BivariateFunction[IntegerNumber, IntegerNumber, IntegerNumber] {

    def of(a: IntegerNumber, b: IntegerNumber)(implicit algorithm: GreatestCommonDivisorAlgorithm): IntegerNumber = (a, b) match {
        case (Zero, _) => Zero
        case (_, Zero) => a
        case _ if a == b => a
        case _ => algorithm(a, b)
    }

    def apply(f1: IntegerNumber, f2: IntegerNumber) = of(f1, f2)(EuclidsAlgorithm)

}

trait GreatestCommonDivisorAlgorithm {

    def apply(a: IntegerNumber, b: IntegerNumber): IntegerNumber

}

object EuclidsAlgorithm extends GreatestCommonDivisorAlgorithm {

    import net.ollie.maths.numbers.Precision
    import Precision._

    private val ONE_DP = 1 dp

    def apply(a: IntegerNumber, b: IntegerNumber) = GreatestCommonDivisor.of(b, a mod b)(this)

    private implicit class Mod(val a: IntegerNumber) extends AnyRef {

        def mod(b: IntegerNumber): IntegerNumber = a - (b * Floor(a.evaluate(ONE_DP) / b.evaluate(ONE_DP)))

    }

}
