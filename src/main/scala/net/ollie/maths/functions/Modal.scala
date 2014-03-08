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

}

object ModalPair {

    implicit def apply(t: Tuple2[Natural, Integer]): ModalPair = new ModalPair(t._1, t._2)

}

class ModalPair(val degree: Natural, val order: Integer)
        extends Modal {

    override def equals(that: Any) = that match {
        case pair: ModalPair => this.degree == pair.degree && this.order == pair.order
        case _ => super.equals(that)
    }

    override def toString = s"($degree, $order)"

}