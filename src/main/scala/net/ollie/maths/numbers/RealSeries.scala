package net.ollie.maths.numbers

import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.{NotEvaluable, ConstantSeries}
import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.utils.OptionalBigDecimal

/**
 * Created by Ollie on 14/01/14.
 */
object RealSeries {

    def apply(left: Real, right: Real): Real = (left, right) match {
        case (Zero, _) => right
        case (_, Zero) => left
        case _ => new RealSeries(Seq(left, right))
    }

    def apply(terms: Seq[Real]): Real = terms.filterNot(_.isEmpty) match {
        case Nil => Zero
        case term :: Nil => term
        case _ => new RealSeries(terms)
    }

}

class RealSeries private(override val terms: Seq[Real])
    extends ConstantSeries(terms)
        with Real
        with ApproximatelyEvaluated {

    override protected[this] def doApproximatelyEvaluate(precision: Precision) = {
        terms.map(_.approximatelyEvaluate(precision)).sum
    }

    override def tryEvaluate(precision: Precision): OptionalBigDecimal = {
        if (terms.exists(_.isInstanceOf[NotEvaluable])) None
        else super.tryEvaluate(precision)
    }

    override def ?+(that: Real) = {
        if (that.isEmpty) this
        Some(RealSeries(that match {
            case series: RealSeries => simplify(this.terms ++ series.terms)
            case _ => simplify(terms :+ that)
        }))
    }

    override def tryAdd(left: Real, right: Real) = left ?+? right

    override def equals(that: Real) = that match {
        case series: RealSeries => this.terms == series.terms || super.equals(series)
        case _ => super.equals(that)
    }

}