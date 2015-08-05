package net.ollie.utils

import java.util.concurrent.ConcurrentHashMap
import java.util.function

/**
 * Created by Ollie on 05/08/2015.
 */
abstract class ValueCache[K, V]
        extends (K => V) {

    private val cache = new ConcurrentHashMap[K, V]

    private val func = new function.Function[K, V] {
        override def apply(k: K) = compute(k)
    }

    def apply(key: K) = cache.computeIfAbsent(key, func)

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
