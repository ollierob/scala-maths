package net.ollie.utils

import java.util.concurrent.ConcurrentHashMap

import scala.collection.JavaConversions._

/**
 * Created by Ollie on 05/08/2015.
 */
abstract class ValueCache[K, V]
        extends (K => V) {

    private val cache: scala.collection.concurrent.Map[K, V] = new ConcurrentHashMap[K, V]

    def apply(key: K) = cache.getOrElseUpdate(key, compute(key))

    protected def compute(key: K): V

    def reset() = cache.clear()

}

abstract class BiValueCache[K1, K2, V]
        extends ValueCache[(K1, K2), V]
        with ((K1, K2) => V) {

    def apply(k1: K1, k2: K2): V = apply((k1, k2))

    protected def compute(k1: K1, k2: K2): V

    protected def compute(e: (K1, K2)): V = compute(e._1, e._2)

}
