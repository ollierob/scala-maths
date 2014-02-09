package net.ollie.maths.numbers

import net.ollie.maths.Number

/**
 * Created by Ollie on 09/02/14.
 */
trait Multivalued[N <: Number]
        extends Number {

    type System = Multivalued[_]

    def values: Set[N]

    def isEmpty = values.forall(_.isEmpty)

    def inverse: Multivalued[_]

    def unary_-(): Multivalued[_]

    def ?^(that: Number): Option[Number] = ???

    def ?*(that: Number)(leftToRight: Boolean): Option[Number] = ???

    def ?+(that: Number): Option[Number] = ???

}

object Multivalued {

    def apply[N <: Number](n: N): Multivalued[N] = n match {
        case m: Multivalued[N] => m
        case _ => new MultivaluedSingleton(n)
    }

    def apply[N <: Number](values: Set[N]): Multivalued[N] = new MultivaluedSet(values)

}

class MultivaluedSingleton[N <: Number](val value: N)
        extends Multivalued[N] {

    def values = Set(value)

    def inverse = Multivalued(value.inverse)

    def unary_-() = Multivalued(-value)

}

class MultivaluedSet[N <: Number](val values: Set[N])
        extends Multivalued[N] {

    def unary_-() = Multivalued(values.map(-_))

    def inverse = Multivalued(values.map(_.inverse))

}