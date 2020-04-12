package net.ollie.maths.numbers

import net.ollie.maths.Constant

/**
 * Takes multiple values - perhaps an infinite number.
 * Created by Ollie on 09/02/14.
 */
trait Multivalued
    extends Constant {

    type System = Multivalued

    type Contents <: Constant

    def principal: Contents

    override def unary_-(): Multivalued = Multivalued.negate(this)

    override def ?^(that: Constant) = None

    override def ?*(that: Constant)(leftToRight: Boolean) = None

    override def ?+(that: Constant) = None

}

object Multivalued {

    def apply(n: Constant): Multivalued = n match {
        case m: Multivalued => m
        case _ => new MultivaluedSingleton(n)
    }

    def apply[N <: Constant](principal: N, values: Set[N]): Multivalued = {
        new MultivaluedSet(principal, values)
    }

    def negate[M <: Multivalued](m: M): NegatedMultivalued[M] = {
        new NegatedMultivalued(m)
    }

}

class MultivaluedSingleton[N <: Constant](val principal: N)
    extends Multivalued {

    type Contents = N

    def values = Set(principal)

    def isEmpty = principal.isEmpty

    def inverse = Multivalued(principal.inverse)

}

class MultivaluedSet[N <: Constant](val principal: N, val values: Set[N])
    extends Multivalued {

    type Contents = N

    require(values.contains(principal))

    def isEmpty = values.forall(_.isEmpty)

    def inverse = {
        val p: N = principal
        Multivalued(p.inverse, values.map(_.inverse))
    }

}

class NegatedMultivalued[M <: Multivalued](val of: M)
    extends Multivalued {

    type Contents = M#Contents#System

    def inverse = (-of).inverse

    def isEmpty = of.isEmpty

    override def unary_-() = ??? //of

    def principal = -(of.principal)

    override def toString = s"-$of"

}
