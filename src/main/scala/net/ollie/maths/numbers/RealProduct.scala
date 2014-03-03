package net.ollie.maths.numbers

import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.{NotEvaluable, ConstantProduct}
import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.utils.OptionalBigDecimal

/**
 * Created by Ollie on 23/02/14.
 */
object RealProduct {

    def apply(left: Real, right: Real): Real = {
        if (left.isEmpty || right.isEmpty) Zero
        else new RealProduct(Seq(left, right))
    }

    def apply(terms: Seq[Real]): Real = terms match {
        case Nil => Zero
        case item :: Nil => item
        case _ if terms.contains(Zero) => Zero
        case _ => new RealProduct(terms)
    }

}

class RealProduct(override val terms: Seq[Real])
        extends ConstantProduct(terms)
        with Real
        with ApproximatelyEvaluated {

    override def ?*(that: Real) = Some(RealProduct(simplify(terms :+ that)))

    protected[this] def tryMultiply(left: Real, right: Real) = left ?*? right

    def doApproximatelyEvaluate(precision: Precision) = {
        val evaluated = terms.map(_.approximatelyEvaluate(precision))
        val totalPrecision = evaluated.foldLeft(precision.digits.asInstanceOf[Integer])((current, term) => current + intLength(term))
        if (totalPrecision <= precision.digits) evaluated.product
        else {
            val newPrecision = precision.increaseBy(totalPrecision - precision.digits)
            terms.map(_.approximatelyEvaluate(newPrecision)).product
        }
    }

    override def tryEvaluate(precision: Precision): OptionalBigDecimal = {
        if (terms.find(_.isInstanceOf[NotEvaluable]).isDefined) None
        else super.tryEvaluate(precision)
    }

    /**
     * Count the number of zero digits in the given decimal.
     * @param bd
     * @return
     */
    private def intLength(bd: BigDecimal): Int = {
        val abs = bd.abs
        if (abs >= 1) {
            return abs.toBigInt.toString.length
        } else {
            return -1; //TODO negative value
        }
    }

    override def equals(that: Real) = that match {
        case product: RealProduct => this.terms == product.terms || super.equals(product)
        case _ => super.equals(that)
    }

}