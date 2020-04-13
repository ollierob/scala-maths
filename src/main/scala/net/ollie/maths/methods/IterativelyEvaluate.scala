package net.ollie.maths.methods

import net.ollie.maths.CachedEvaluated
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.numbers.{Natural, Precision}

/**
 * Created by Ollie on 12/01/14.
 */
object IterativelyEvaluate {

    def apply(precision: Precision, f: IterativelyEvaluated): BigDecimal = {

        val maxPrecision = precision.doubled

        var current: BigDecimal = null
        var currentPrecision = precision
        var n: Natural = Zero
        var continue: Boolean = true
        val iterator = f.evaluationIterator(precision)
        var increasePrecision = false

        do {

            val prev = current
            current = iterator.next(n, currentPrecision)
            if (prev != null) {
                val isPrecise = precision.within(current, prev) || current == prev
                if (isPrecise && !increasePrecision) increasePrecision = true
                else if (isPrecise) continue = false
            }

            if (continue) n = n.succ
            if (continue && increasePrecision) {
                //FIXME need to restart with increased precision
                currentPrecision = currentPrecision.increase
                if ((currentPrecision > maxPrecision).contains(true)) continue = false //Failed to converge
            }

        } while (continue)

        current to precision

    }

}

/**
 * Keeps iterating over an evaluation at increasing precision until it converges.
 * Highest precision result is cached.
 */
trait IterativelyEvaluated
    extends CachedEvaluated {

    final def doEvaluate(precision: Precision): BigDecimal = IterativelyEvaluate(precision, this)

    def evaluationIterator(startPrecision: Precision): EvaluationIterator

}

/**
 * Keeps iterating over an approximate evaluation at increasing precision until it converges.
 * Highest precision result is cached.
 */
trait ApproximatelyEvaluated
    extends IterativelyEvaluated {

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator {

        def next(nth: Natural, precision: Precision) = approximatelyEvaluate(precision)

    }

    override final def approximatelyEvaluate(precision: Precision): BigDecimal = {
        doApproximatelyEvaluate(precision) to precision
    }

    protected[this] def doApproximatelyEvaluate(precision: Precision): BigDecimal

}

trait EvaluationIterator {

    def next(nth: Natural, precision: Precision): BigDecimal

}
