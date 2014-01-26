package net.ollie.maths.functions

import net.ollie.maths.numbers.{Integer, Natural}

/**
 * Created by Ollie on 08/01/14.
 */
trait Modal {

    def l: Natural

    def m: Integer

}

object ModalPair {

    implicit def apply(t: Tuple2[Natural, Integer]): ModalPair = new ModalPair(t._1, t._2)

}

class ModalPair(val l: Natural, val m: Integer)
        extends Modal {

    override def equals(that: Any) = that match {
        case pair: ModalPair => this.l == pair.l && this.m == pair.m
        case _ => super.equals(that)
    }

    override def toString = s"($l, $m)"

}