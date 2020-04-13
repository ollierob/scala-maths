package net.ollie.maths.functions

import net.ollie.maths.numbers.{Integer, Natural}

/**
 * Created by Ollie on 08/01/14.
 */
trait Modal {

    def degree: Natural

    def l = degree

    def order: Integer

    def m = order

    def isZero = l == 0 && m == 0

    override def toString = s"($l, $m)"

}

object Modal {

    implicit def apply(t: Tuple2[Natural, Integer]): ModalPair = new ModalPair(t._1, t._2)

    def unapply(m: Modal): Option[(Natural, Integer)] = Some((m.l, m.m))

}

class ModalPair(val degree: Natural, val order: Integer)
    extends Modal {

    require(order <= degree)

}

object ZeroModal
    extends ModalPair(0, 0)