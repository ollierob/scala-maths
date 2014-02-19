package net.ollie.maths.sequences

import net.ollie.maths.Expression
import net.ollie.maths.numbers.Natural
import scala.collection.mutable

/**
 * Created by Ollie on 19/02/14.
 */
trait Sequence {

    def apply(n: Natural): Expression

}

trait CachingSequence
        extends Sequence {

    type Element <: Expression

    private val cache = new mutable.HashMap[Natural, Element]() ++ initial

    def apply(n: Natural): Element = cache.get(n) match {
        case Some(m) => m
        case _ => {
            val m = create(n)
            cache.put(n, m)
            return m
        }
    }

    protected[this] def create(n: Natural): Element

    protected[this] def initial: Map[Natural, Element]

}
