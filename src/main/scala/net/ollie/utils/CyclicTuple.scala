package net.ollie.utils

/**
 * Created by Ollie on 26/01/14.
 */
trait CyclicTuple
        extends Product {

    def cycleLeft: CyclicTuple

    def cycleRight: CyclicTuple

    def cycles: Set[_ <: CyclicTuple]

}

object CyclicTuple {

    implicit def apply[A, B, C](t: Tuple3[A, B, C]): CyclicTuple3[A, B, C] = new CyclicTuple3(t._1, t._2, t._3)

}

class CyclicTuple3[+T1, +T2, +T3](override val _1: T1, override val _2: T2, override val _3: T3)
        extends Tuple3(_1, _2, _3)
        with CyclicTuple {

    def cycleLeft: CyclicTuple3[T2, T3, T1] = new CyclicTuple3(_2, _3, _1)

    def cycleRight: CyclicTuple3[T3, T1, T2] = new CyclicTuple3(_3, _1, _2)

    def cycles: Set[CyclicTuple3[_, _, _]] = Set(this, cycleLeft, cycleLeft.cycleLeft)

    override def equals(that: Any) = that match {
        case t: Tuple3[_, _, _] => _1 == t._1 && _2 == t._2 && _3 == t._3
        case _ => false
    }

}