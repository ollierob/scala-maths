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

class RealProduct protected(override val terms: Seq[Real])
    extends ConstantProduct(terms)(Real)
        with Real
        with ApproximatelyEvaluated {

    private lazy val empty = isEmpty

    override protected[this] def apply(terms: Seq[Real]) = RealProduct(terms)

    override def ?*(that: Real): Option[Real] = that match {
        case p: RealProduct => Some(RealProduct(simplify(terms ++ p.terms)))
        case _ => Some(RealProduct(simplify(terms :+ that)))
    }

    protected[this] def tryMultiply(left: Real, right: Real) = left ?*? right

    def doApproximatelyEvaluate(precision: Precision) = {
        val product = terms.map(_.approximatelyEvaluate(precision)).product
        if (!empty && product == 0) doApproximatelyEvaluate(precision.increase) //FIXME only increase a certain number of times
        else product
    }

    override def tryEvaluate(precision: Precision): OptionalBigDecimal = {
        if (terms.exists(_.isInstanceOf[NotEvaluable])) None
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

    override def toString: String = super.toString

}