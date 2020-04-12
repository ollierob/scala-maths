package net.ollie.maths.numbers.constants

import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.methods.{EvaluationIterator, IterativelyEvaluated}
import net.ollie.maths.numbers.{Irrational, Natural, Precision, Real}

/**
 * Created by Ollie on 24/02/14.
 *
 * @see http://mathworld.wolfram.com/OmegaConstant.html
 * @see http://oeis.org/A030178
 */
object Omega
    extends PositiveNamedReal with Irrational {

    private val OMEGA_100 = BigDecimal("0.5671432904097838729999686622103555497538157871865125081351310792230457930866845666932194469617522945")
    private lazy val ITERATION = new IterativelyEvaluated {

        def evaluationIterator(startPrecision: Precision) = new EvaluationIterator {

            private var current = atMaxPrecision match {
                case Some(bd) => bd
                case _ => OMEGA_100
            }

            def next(nth: Natural, precision: Precision) = {
                current = (1 + current) / (1 + Exp(Real(current)).evaluate(precision))
                current
            }

        }

    }

    def evaluate(precision: Precision) = {
        if (precision.digits < 100) precision(OMEGA_100)
        else ITERATION.evaluate(precision)
    }

    override def toString = "Ω"

}
