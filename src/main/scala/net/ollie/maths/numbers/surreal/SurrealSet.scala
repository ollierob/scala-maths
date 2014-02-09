package net.ollie.maths.numbers.surreal

import net.ollie.maths.Operation
import net.ollie.maths.numbers.Real

/**
 * Created by Ollie on 28/01/14.
 */
trait SurrealSet {

    def isEmpty: Boolean

    def unary_-(): SurrealSet

    def +(that: Real): SurrealSet

    /**
     * Construct a surreal number from both sets.
     * @param that
     * @return
     */
    def |(that: SurrealSet): Surreal = Surreal(this, that)

    /**
     * Intersect both sets.
     * @param that
     * @return
     */
    def ::(that: SurrealSet): SurrealSet = SurrealSet.compose(this, that)

    def contains(s: Real): Boolean

    def intersects(that: SurrealSet): Boolean

    def max: Real

    def min: Real

}

object SurrealSet {

    def apply(): SurrealSet = EmptySurrealSet

    implicit def apply(re: Real): SurrealSet = new SingletonSurrealSet(re)

    def apply(set: Set[Real]): SurrealSet = if (set.isEmpty) EmptySurrealSet else new RegularSurrealSet(set)

    protected[surreal] def compose(a: SurrealSet, b: SurrealSet): SurrealSet = new CompositeSurrealSet(Set(a, b))

}

/**
 * Zero is not the same as the empty set.
 */
object EmptySurrealSet
        extends SurrealSet {

    def isEmpty = true

    def unary_-() = this

    def +(that: Real) = that

    override def ::(that: SurrealSet) = that

    def intersects(that: SurrealSet) = false

    def contains(s: Real) = false

    def min = Operation.undefined

    def max = Operation.undefined

    override def toString = "-"

}

class SingletonSurrealSet(val s: Real)
        extends SurrealSet {

    def isEmpty = false

    def unary_-() = -s

    def +(that: Real) = s + that

    def contains(s: Real) = this.s == s

    def intersects(that: SurrealSet) = that.contains(s)

    override def toString = s.toString

    def min = s

    def max = s

}

class RegularSurrealSet(val set: Set[Real])
        extends SurrealSet {

    require(!set.isEmpty)

    def isEmpty = false

    def unary_-() = SurrealSet(set.map(-_))

    def +(that: Real) = SurrealSet(set.map(_ + that))

    def contains(s: Real) = set.contains(s)

    def intersects(that: SurrealSet) = set.find(that.contains(_) == true).isDefined

    def min = set.min

    def max = set.max

    override def toString = set.mkString("[", ",", "]")

}

class CompositeSurrealSet(val sets: Set[SurrealSet])
        extends SurrealSet {

    require(!sets.isEmpty)

    def isEmpty = sets.forall(_.isEmpty)

    def unary_-(): SurrealSet = new CompositeSurrealSet(sets.map(-_))

    def +(that: Real): SurrealSet = ???

    def contains(s: Real) = sets.find(_.contains(s)).isDefined

    def intersects(that: SurrealSet) = sets.find(_.intersects(that)).isDefined

    def min = sets.map(_.min).min

    def max = sets.map(_.max).max

}