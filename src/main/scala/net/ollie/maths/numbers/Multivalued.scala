package net.ollie.maths.numbers

import net.ollie.maths.Number

/**
 * TODO should have a  (Very difficult!)
 * Created by Ollie on 09/02/14.
 */
trait Multivalued
        extends Number {

    type System = Multivalued

    type Contents <: Number

    def principal: Contents

    def unary_-(): Multivalued = Multivalued.negate(this)

    def ?^(that: Number) = None

    def ?*(that: Number)(leftToRight: Boolean) = None

    def ?+(that: Number) = None

}

object Multivalued {

    def apply(n: Number): Multivalued = n match {
        case m: Multivalued => m
        case _ => new MultivaluedSingleton(n)
    }

    def apply[N <: Number](principal: N, values: Set[N]): Multivalued = {
        new MultivaluedSet(principal, values)
    }

    def negate[M <: Multivalued](m: M): Multivalued = {
        new NegatedMultivalued(m)
    }

}

class MultivaluedSingleton[N <: Number](val principal: N)
        extends Multivalued {

    type Contents = N

    def values = Set(principal)

    def isEmpty = principal.isEmpty

    def inverse = Multivalued(principal.inverse)

}

class MultivaluedSet[N <: Number](val principal: N, val values: Set[N])
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