package net.ollie.maths.numbers

import net.ollie.maths.numbers.constants.{One, Zero}

trait Irrational
    extends Real {

    override def isEmpty = false

    override def ?*(r: Real): Option[Real] = r match {
        case r: Rational => Some(this * r)
        case _ => super.?*(r)
    }

    def *(i: Rational): Real = IrrationalProduct(this, i)

}

object IrrationalProduct {

    def apply(i: Irrational, r: Rational) = r match {
        case Zero => Zero
        case One => i
        case _ => new IrrationalProduct(Seq(i, r))
    }

}

case class IrrationalProduct private(override val terms: Seq[Real])
    extends RealProduct(terms) with Irrational {

    override def *(r: Rational) = r match {
        case Zero => Zero
        case One => this
        case IrrationalProduct(seq) => new IrrationalProduct(simplify(terms :+ seq))
        case _ => new IrrationalProduct(simplify(terms ++ r))
    }

}