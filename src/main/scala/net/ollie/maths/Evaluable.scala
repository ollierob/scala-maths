package net.ollie.maths

import scala.math.BigDecimal.RoundingMode._

import net.ollie.maths.numbers.{DoublePrecision, IntegerPrecision, Precision, SinglePrecision}

/**
 * Something that can be represented as a BigDecimal.
 *
 * This is more-or-less limited to the (finite) real numbers.
 *
 * Created by Ollie on 12/01/14.
 */
trait Evaluable {

    private var max: Option[(Precision, BigDecimal)] = None

    def evaluate(precision: Precision)(implicit mode: RoundingMode = Precision.DEFAULT_ROUNDING): BigDecimal = {

        max match {
            case Some((prec, value)) => prec > precision match {
                case Some(true) => return precision(value)(mode)
                case _ =>
            }
            case _ =>
        }

        val evaluated = this.eval(precision)

        if (max.isDefined) {
            precision > max.get._1 match {
                case Some(true) => max = Some(precision, evaluated)
                case _ =>
            }
        } else {
            max = Some(precision, evaluated)
        }

        return precision(evaluated)(mode)

    }

    protected[this] def eval(precision: Precision): BigDecimal

    def approximatelyEvaluate(precision: Precision): BigDecimal = evaluate(precision)

    protected[this] def cache(precision: Precision): Boolean = true //Evaluable.DO_CACHE.contains(precision)

}

object Evaluable {

    final val DO_CACHE = Set(IntegerPrecision, SinglePrecision, DoublePrecision)

}