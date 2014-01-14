package net.ollie.maths

import scala.collection.mutable
import scala.math.BigDecimal.RoundingMode._

import net.ollie.maths.numbers.{DoublePrecision, IntegerPrecision, Precision, SinglePrecision}

/**
 * Created by Ollie on 12/01/14.
 */
trait Evaluable {

    private final def cached: mutable.Map[(Precision, RoundingMode), BigDecimal] = new mutable.HashMap()

    final def evaluate(precision: Precision)(implicit mode: RoundingMode = Precision.DEFAULT_ROUNDING): BigDecimal = {

        if (!cache(precision)) return eval(precision)

        val key = (precision, mode)
        cached.get(key) match {
            case Some(d) => d
            case otherwise => {
                val d = eval(precision)
                cached.put(key, d)
                d
            }
        }

    }

    protected[this] def eval(precision: Precision)(implicit mode: RoundingMode): BigDecimal

    def approximatelyEvaluate(precision: Precision)(implicit mode: RoundingMode): BigDecimal = evaluate(precision)

    protected[this] def cache(precision: Precision): Boolean = Evaluable.DO_CACHE.contains(precision)

}

object Evaluable {

    final val DO_CACHE = Set(IntegerPrecision, SinglePrecision, DoublePrecision)

}