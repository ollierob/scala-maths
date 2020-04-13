package net.ollie.maths.methods

import net.ollie.maths.CachedEvaluated
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.numbers.{Natural, Precision}

/**
 * Created by Ollie on 12/01/14.
 */
object IterativelyEvaluate {

    def apply(precision: Precision, f: IterativelyEvaluated): BigDecimal = {
        val max = precision.doubled
        var current: BigDecimal = 0
        var currentPrecision = precision
        var n: Natural = Zero
        var continue: Boolean = true
        val iterator = f.evaluationIterator(precision)
        do {
            val prev = current
            current = iterator.next(n, currentPrecision)
            if (precision.within(current, prev)) continue = false
            else currentPrecision = currentPrecision.increase
            if (continue && (currentPrecision > max).contains(true)) continue = false //FIXME check convergence better
            if (continue) n = n.succ
        } while (continue)
        current to precision
    }

    private def toBigInt(decimal: BigDecimal, precision: Precision): BigInt = {
        return precision(decimal.underlying.movePointRight(precision.digits.toInt.get)) toBigInt
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
        precision(doApproximatelyEvaluate(precision))
    }

    protected[this] def doApproximatelyEvaluate(precision: Precision): BigDecimal

}

trait EvaluationIterator {

    def next(nth: Natural, precision: Precision): BigDecimal

}
