package net.ollie.maths.functions.numeric

import net.ollie.maths.functions.BivariateFunction
import net.ollie.maths.numbers.{IntegerNumber, NaturalNumber, Zero}

/**
 * Created by Ollie on 04/01/14.
 * @see http://mathworld.wolfram.com/GreatestCommonDivisor.html
 */
object GreatestCommonDivisor extends BivariateFunction[IntegerNumber, IntegerNumber, NaturalNumber] {

    def apply(i1: Int, i2: Int): NaturalNumber = apply(IntegerNumber(i1), IntegerNumber(i2))

    def apply(a: NaturalNumber, b: NaturalNumber)(implicit algorithm: GreatestCommonDivisorAlgorithm = EuclidsAlgorithm): NaturalNumber = (a, b) match {
        case (Zero, _) => Zero
        case (_, Zero) => a
        case _ if a == b => a
        case _ => algorithm(a, b)
    }

    def apply(i1: IntegerNumber, i2: IntegerNumber): NaturalNumber = apply(i1.abs, i2.abs)

    implicit object EuclidsAlgorithm
            extends GreatestCommonDivisorAlgorithm {

        import net.ollie.maths.numbers.Precision._

        private val ONE_DP = 1 dp

        def apply(a: NaturalNumber, b: NaturalNumber) = GreatestCommonDivisor(b, a mod b)(this)

        private implicit class Mod(val a: IntegerNumber) extends AnyRef {

            def mod(b: NaturalNumber): NaturalNumber = a - (b * Floor(a.evaluate(ONE_DP) / b.evaluate(ONE_DP)))

        }

    }

}

trait GreatestCommonDivisorAlgorithm {

    def apply(a: NaturalNumber, b: NaturalNumber): NaturalNumber

}
