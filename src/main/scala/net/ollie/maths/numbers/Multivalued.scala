package net.ollie.maths.numbers

import net.ollie.maths.Number

/**
 * Created by Ollie on 09/02/14.
 */
trait Multivalued
        extends Number {

    type System = Multivalued

    type Contents <: Number

    type Inverse = Multivalued //{type Inverse = Contents#Inverse}

    def values: Set[Contents]

    def isEmpty = values.forall(_.isEmpty)

    def unary_-(): Multivalued

    def ?^(that: Number): Option[Number] = ???

    def ?*(that: Number)(leftToRight: Boolean): Option[Number] = ???

    def ?+(that: Number): Option[Number] = ???

}

object Multivalued {

    def apply[N <: Number](n: N): Multivalued = n match {
        case m: Multivalued => m
        case _ => new MultivaluedSingleton(n)
    }

    def apply[N <: Number](values: Set[N]): Multivalued = new MultivaluedSet(values)

    trait Builder[N] {

        def apply(set: Set[N]): N with Multivalued

    }

}

class MultivaluedSingleton[N <: Number](val value: N)
        extends Multivalued {

    type Contents = N

    def values = Set(value)

    def unary_-(): Multivalued = Multivalued(-value)

    def inverse = Multivalued(value.inverse)

}

class MultivaluedSet[N <: Number](val values: Set[N])
        extends Multivalued {

    type Contents = N

    def unary_-() = Multivalued(values.map(-_))

    def inverse = Multivalued(values.map(_.inverse))

}