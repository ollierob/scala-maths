package net.ollie.maths.numbers

import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers.constants.{One, Zero}
import net.ollie.maths.{ConstantProduct, NotEvaluable}
import net.ollie.utils.OptionalBigDecimal

/**
 * Created by Ollie on 23/02/14.
 */
object RealProduct {

    def apply(left: Real, right: Real): Real = {
        if (left.isZero || right.isZero) Zero
        else if (One.isExactly(left)) right
        else if (One.isExactly(right)) left
        else new RealProduct(Seq(left, right))
    }

    def apply(terms: Seq[Real]): Real = terms match {
        case Nil => Zero
        case item :: Nil => item
        case One :: c :: Nil => c
        case c :: One :: Nil => c
        case _ if terms.contains(Zero) => Zero
        case _ => new RealProduct(terms)
    }

}

case class RealProduct protected(override val terms: Seq[Real])
    extends ConstantProduct(terms)(Real)
        with Real
        with ApproximatelyEvaluated {

    override protected[this] def apply(terms: Seq[Real]) = RealProduct(terms)

    override def ?*(that: Real) = that match {
        case RealProduct(seq) => Some(RealProduct(simplify(terms ++ seq)))
        case _ => Some(RealProduct(simplify(terms :+ that)))
    }

    protected[this] def tryMultiply(left: Real, right: Real) = left ?*? right

    def doApproximatelyEvaluate(precision: Precision) = {
        val evaluated = terms.map(_.approximatelyEvaluate(precision))
        val totalPrecision: Integer = evaluated.foldLeft(precision.digits.asInstanceOf[Integer])((current, term) => current + intLength(term))
        if (totalPrecision <= precision.digits) {
            evaluated.product;
        } else {
            val diff: Natural = Natural.convert(totalPrecision - precision.digits);
            val newPrecision = precision.increaseBy(diff)
            terms.map(_.approximatelyEvaluate(newPrecision)).product
        }
    }

    override def tryEvaluate(precision: Precision): OptionalBigDecimal = {
        if (terms.find(_.isInstanceOf[NotEvaluable]).isDefined) None
        else super.tryEvaluate(precision)
    }

    /**
     * Count the number of zero digits in the given decimal.
     *
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