package net.ollie.maths.methods


import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

import net.ollie.maths.Evaluable
import net.ollie.maths.numbers.{NaturalNumber, Precision, Zero}

/**
 * Created by Ollie on 12/01/14.
 */
object IterativelyEvaluate {

    def apply(precision: Precision, f: IterativelyEvaluated)(implicit mode: RoundingMode): BigDecimal = {
        var previous: BigInt = 0
        var current: BigDecimal = 0
        var continue = true
        var currentPrecision = precision
        var n: NaturalNumber = Zero
        val iterator = f.evaluationIterator
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

    protected[this] def eval(precision: Precision) = IterativelyEvaluate(precision, this)(Precision.DEFAULT_ROUNDING)

    def evaluationIterator: EvaluationIterator

}

trait EvaluationIterator {

    def next(nth: NaturalNumber, precision: Precision)(implicit mode: RoundingMode): BigDecimal

}

/**
 * Evaluated at an ever increasing precision.
 */
trait ApproximatelyEvaluated
        extends IterativelyEvaluated {

    private lazy val it = new EvaluationIterator {

        def next(nth: NaturalNumber, precision: Precision)(implicit mode: RoundingMode.RoundingMode) = approximatelyEvaluate(precision)

    }

    final def evaluationIterator = it

    override def approximatelyEvaluate(precision: Precision)(implicit mode: RoundingMode): BigDecimal = approx(precision)(mode)

    protected[this] def approx(precision: Precision)(implicit mode: RoundingMode): BigDecimal

}