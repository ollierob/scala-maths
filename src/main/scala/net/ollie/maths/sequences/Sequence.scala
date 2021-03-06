package net.ollie.maths.sequences

import net.ollie.maths.expressions.Expression
import net.ollie.maths.numbers.Natural

import scala.collection.mutable

/**
 * Created by Ollie on 19/02/14.
 */
trait Sequence {

    type Element <: Expression

    def apply(n: Natural): Element

}

trait CachingSequence
    extends Sequence {

    private val cache = new mutable.HashMap[Natural, Element]() ++ initial

    def apply(n: Natural): Element = {
        if (!shouldCache(n)) return createNoCache(n)
        cache.get(n) match {
            case Some(m) => m
            case _ =>
                val m = create(n)
                cache.put(n, m)
                m
        }
    }

    protected[this] def shouldCache(n: Natural): Boolean = true

    protected[this] def create(n: Natural): Element

    protected[this] def createNoCache(n: Natural): Element = create(n)

    protected[this] def initial: Map[Natural, Element]

}
