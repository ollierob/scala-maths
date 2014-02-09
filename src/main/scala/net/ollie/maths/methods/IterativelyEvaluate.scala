package net.ollie.maths.methods

import net.ollie.maths.Evaluable
import net.ollie.maths.numbers.{Natural, Precision, Zero}

/**
 * Created by Ollie on 12/01/14.
 */
object IterativelyEvaluate {

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
            if (continue) life = true
            else {
                continue = life;
                life = false
            }
            if (continue) {
                previous = next
                currentPrecision = precision.increase
                n = n.succ
            }
        }
        current to precision
    }

    private def toBigInt(decimal: BigDecimal, precision: Precision): BigInt = {
        return precision(decimal.underlying.movePointRight(precision.value)) toBigInt
    }

}

trait IterativelyEvaluated
        extends Evaluable {

    protected[this] def doEvaluate(precision: Precision) = IterativelyEvaluate(precision, this)

    def evaluationIterator(startPrecision: Precision): EvaluationIterator

}

trait EvaluationIterator {

    def next(nth: Natural, precision: Precision): BigDecimal

}

/**
 * Evaluated at an ever increasing precision.
 */
trait ApproximatelyEvaluated
        extends IterativelyEvaluated {

    private lazy val it = new EvaluationIterator {

        def next(nth: Natural, precision: Precision) = approximatelyEvaluate(precision)

    }

    final def evaluationIterator(startPrecision: Precision) = it

    override def approximatelyEvaluate(precision: Precision): BigDecimal = approx(precision)

    protected[this] def approx(precision: Precision): BigDecimal

}