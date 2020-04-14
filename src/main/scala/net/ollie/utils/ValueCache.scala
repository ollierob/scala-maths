package net.ollie.utils

import java.util.concurrent.ConcurrentHashMap

import scala.collection.mutable
import scala.jdk.CollectionConverters._;

/**
 * Created by Ollie on 05/08/2015.
 */
abstract class ValueCache[K, V]
    extends (K => V) {

    private val cache: mutable.Map[K, V] = new ConcurrentHashMap[K, V].asScala ++ initial

    def apply(key: K) = {
        if (shouldCache(key)) cache.getOrElseUpdate(key, compute(key))
        else compute(key)
    }

    protected[this] def compute(key: K): V

    protected[this] def initial: Map[K, V] = Map()

    protected[this] def shouldCache(k: K): Boolean = true

    def reset() = cache.clear()

}

abstract class BiValueCache[K1, K2, V]
    extends ValueCache[(K1, K2), V]
        with ((K1, K2) => V) {

    def apply(k1: K1, k2: K2): V = apply((k1, k2))

    protected def compute(k1: K1, k2: K2): V

    protected def compute(e: (K1, K2)): V = compute(e._1, e._2)

}
