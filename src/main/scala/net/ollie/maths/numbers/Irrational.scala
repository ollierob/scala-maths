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

object Irrational {

    def is(r: Real): Boolean = r.isInstanceOf[Irrational]

}

object IrrationalProduct {

    def apply(i: Irrational, r: Rational) = (i, r) match {
        case (_, Zero) => Zero
        case (_, One) => i
        case (p: IrrationalProduct, _) => p * r
        case _ => new IrrationalProduct(Seq(i, r))
    }

}

class IrrationalProduct private(override val terms: Seq[Real])
    extends RealProduct(terms) with Irrational {

    override def *(r: Rational) = r match {
        case Zero => Zero
        case One => this
        case p: IrrationalProduct => new IrrationalProduct(simplify(terms ++ p.terms))
        case _ => new IrrationalProduct(simplify(terms ++ r))
    }

}