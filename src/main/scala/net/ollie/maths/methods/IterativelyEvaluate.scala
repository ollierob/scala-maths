package net.ollie.maths.methods

import net.ollie.maths.CachedEvaluated
import net.ollie.maths.numbers.{Natural, Precision}
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 12/01/14.
 */
object IterativelyEvaluate {

    //FIXME this doesn't converge properly
    def apply(precision: Precision, f: IterativelyEvaluated): BigDecimal = {
        var previous: BigInt = 0
        var current: BigDecimal = 0
        var continue = true
        var currentPrecision = precision
        var n: Natural = Zero
        val iterator = f.evaluationIterator(precision)
        var life = true
        while (continue) {
            current = iterator.next(n, currentPrecision)
            val next = toBigInt(current, precision)
            continue = next.compare(previous) != 0
            if (continue)
                life = true
            else {
                continue = life;
                life = false
            }
            if (continue) {
                previous = next
                currentPrecision = currentPrecision.increase
                n = n.succ
            }
        }
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
