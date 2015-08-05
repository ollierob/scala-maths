package net.ollie.utils

import java.util.concurrent.ConcurrentHashMap
import java.util.function

/**
 * Created by Ollie on 05/08/2015.
 */
class ValueCache[K, V](create: K => V) {

    private val cache = new ConcurrentHashMap[K, V]

    private val func = new function.Function[K, V] {
        override def apply(t: K): V = create(t)
    }

    def get(key: K): V = cache.computeIfAbsent(key, func);

}

class BiValueCache[K1, K2, V](create: (K1, K2) => V) extends ValueCache[Tuple2[K1, K2], V](e => create(e._1, e._2)) {

    def get(k1: K1, k2: K2): V = get((k1, k2))

}
