package net.ollie.maths.functions.numeric

import net.ollie.maths.functions.BivariateFunction
import net.ollie.maths.numbers.{One, Integer, Natural, Zero}

/**
 * Created by Ollie on 04/01/14.
 * @see http://mathworld.wolfram.com/GreatestCommonDivisor.html
 */
object GreatestCommonDivisor extends BivariateFunction[Integer, Integer, Natural] {

    def apply(i1: Int, i2: Int): Natural = apply(Integer(i1), Integer(i2))

    def apply(a: Natural, b: Natural)(implicit algorithm: GreatestCommonDivisorAlgorithm = EuclidsAlgorithm): Natural = (a, b) match {
        case (Zero, _) => Zero
        case (_, Zero) => a
        case _ if a == b => a
        case _ => algorithm(a, b)
    }

    def apply(i1: Integer, i2: Integer): Natural = apply(i1.abs, i2.abs)

    implicit object EuclidsAlgorithm
            extends GreatestCommonDivisorAlgorithm {

        import net.ollie.maths.numbers.Precision._

        private val ONE_DP = 1 dp

        def apply(a: Natural, b: Natural) = GreatestCommonDivisor(b, a mod b)(this)

        private implicit class Mod(val a: Integer) extends AnyRef {

            def mod(b: Natural): Natural = {
                if (One == b) Zero
                else Natural(a.evaluate % b.evaluate)
            }

        }

    }

}

trait GreatestCommonDivisorAlgorithm {

    def apply(a: Natural, b: Natural): Natural

}
