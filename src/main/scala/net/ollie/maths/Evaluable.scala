package net.ollie.maths

import net.ollie.maths.numbers.Precision
import net.ollie.utils.OptionalBigDecimal

import scala.collection.mutable
import scala.math.BigDecimal.RoundingMode._

/**
 * Something that can be represented as a BigDecimal.
 *
 * This is more-or-less limited to the (finite) real numbers.
 *
 * Created by Ollie on 12/01/14.
 */
trait Evaluable
    extends MaybeEvaluable {

    protected implicit def rounding: RoundingMode = Precision.DEFAULT_ROUNDING

    /**
     * Evaluate to the given precision. Will throw an exception if not evaluable.
     * Use [[tryEvaluate( )]] for safety.
     */
    def evaluate(precision: Precision): BigDecimal

    def approximatelyEvaluate(precision: Precision): BigDecimal = evaluate(precision)

    def tryEvaluate(precision: Precision) = OptionalBigDecimal.some(evaluate(precision))

}

trait MaybeEvaluable {

    def tryEvaluate(precision: Precision): OptionalBigDecimal

}

trait NotEvaluable
    extends MaybeEvaluable {

    def evaluate(precision: Precision): BigDecimal = Operation.illegal(s"Cannot evaluate $this")

    override def tryEvaluate(precision: Precision): OptionalBigDecimal = OptionalBigDecimal.none

}

trait CachedEvaluated
    extends Evaluable {

    private val max: mutable.Map[String, (Precision, BigDecimal)] = mutable.Map()

    def evaluate(precision: Precision): BigDecimal = {

        val maxForType: Option[(Precision, BigDecimal)] = max.get(precision.precisionType)

        maxForType match {
            case Some((maxPrecision, value)) => (maxPrecision >= precision) match {
                case Some(true) => return precision(value)
                case _ =>
            }
            case _ =>
        }

        val evaluated = this.doEvaluate(precision) to precision //Ensure evaluated at the correct precision

        if (maxForType.isDefined) {
            precision > maxForType.get._1 match {
                case Some(true) => max.put(precision.precisionType, (precision, evaluated))
                case _ =>
            }
        } else {
            max.put(precision.precisionType, (precision, evaluated))
        }

        precision(evaluated)(rounding)

    }

    protected[this] def doEvaluate(precision: Precision): BigDecimal

    protected[this] def cache(precision: Precision): Boolean = true

    protected[this] def atMaxPrecision(precision: Precision): Option[BigDecimal] = max.get(precision.precisionType) match {
        case Some((p, bd)) => Some(bd)
        case _ => None
    }

}