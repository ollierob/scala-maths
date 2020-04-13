package net.ollie.maths.functions.angular

import net.ollie.maths.CachedEvaluated
import net.ollie.maths.functions.{Modal, ModalPair}
import net.ollie.maths.functions.numeric.{Floor, Min, PositiveSquareRoot, TriangleCoefficient}
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers._
import net.ollie.utils.CyclicTuple3
import net.ollie.maths.numbers.constants.{MinusOne, One, Zero}

/**
 * Created by Ollie on 26/01/14.
 *
 * @see http://mathworld.wolfram.com/Wigner3j-Symbol.html
 */
object Wigner3j {

    def apply(l: (Natural, Natural, Natural), m: (Integer, Integer, Integer)): Wigner3j = m match {
        case (Zero, Zero, Zero) => apply(l)
        case _ => new FullWigner3j(l, m)
    }

    def apply(l: (Natural, Natural, Natural)): Wigner3j = {
        if ((l._1 + l._2 + l._3).isEven) new EvenWigner3J(l)
        else new ZeroWigner3J((l, (Zero, Zero, Zero)))
    }

    def apply(g1: (Natural, Integer), g2: (Natural, Integer), g3: (Natural, Integer)): Wigner3j = {
        apply((g1._1, g2._1, g3._1), (g1._2, g2._2, g3._2))
    }

    def apply(y1: Modal, y2: Modal, y3: Modal): Wigner3j = {
        apply((y1.l, y2.l, y3.l), (y1.m, y2.m, y3.m))
    }

    implicit def convert(t: ((Natural, Natural, Natural), (Integer, Integer, Integer))): CyclicTuple3[ModalPair, ModalPair, ModalPair] = {
        new CyclicTuple3((t._1._1, t._2._1), (t._1._2, t._2._2), (t._1._3, t._2._3))
    }

}

trait Wigner3j
    extends Real {

    require(l1 >= m1.abs)
    require(l2 >= m2.abs)
    require(l3 >= m3.abs)

    def lAndM: CyclicTuple3[ModalPair, ModalPair, ModalPair]

    def l1: Natural = lAndM._1.degree

    def l2: Natural = lAndM._2.degree

    def l3: Natural = lAndM._3.degree

    def m1: Integer = lAndM._1.order

    def m2: Integer = lAndM._2.order

    def m3: Integer = lAndM._3.order

    def isEmpty = !evenSum || !zeroSum || !triangleInequality

    final def evenSum: Boolean = (l1 + l2 + l3).isEven

    final def zeroSum: Boolean = m1 + m2 + m3 == Zero

    final def triangleInequality: Boolean = {
        l3 <= l1 + l2 && l3 >= (l1 - l2).abs
    }

    override def toString = s"Wigner3j($lAndM)"

    override def equals(that: Real) = that match {
        case w: Wigner3j => this equals w
        case _ => super.equals(that)
    }

    def equals(that: Wigner3j): Boolean = (lAndM.cycles contains that.lAndM) || super.equals(that)

}

private class ZeroWigner3J(val lAndM: CyclicTuple3[ModalPair, ModalPair, ModalPair])
    extends Wigner3j
        with EmptyConstant {

    override def isEmpty = true

    override def abs = Zero

    override def toString = super[EmptyConstant].toString

    override def evaluate(precision: Precision) = super[EmptyConstant].evaluate(precision)

}

private class EvenWigner3J(val l: CyclicTuple3[Natural, Natural, Natural])
    extends Wigner3j
        with CachedEvaluated {

    require((l1 + l2 + l3).isEven)

    private lazy val pairs: CyclicTuple3[ModalPair, ModalPair, ModalPair] = new CyclicTuple3((l._1, Zero), (l._2, Zero), (l._3, Zero))

    def lAndM = pairs

    final val j: Natural = l1 + l2 + l3

    final val g: Natural = Floor(j / 2)

    final val f = ((MinusOne ^ g)
        * PositiveSquareRoot(gm2l(l1) * gm2l(l2) * gm2l(l3) / (((2 * g) + 1).abs !))
        * (g !)
        / (gml(l1) * gml(l2) * gml(l3)))

    private def gm2l(l: Natural) = (2 * (g - l)).abs.!

    private def gml(l: Natural) = (g - l).abs.!

    protected[this] def doEvaluate(precision: Precision) = f.evaluate(precision)

}

private class FullWigner3j(val lAndM: CyclicTuple3[ModalPair, ModalPair, ModalPair])
    extends Wigner3j
        with CachedEvaluated {

    private lazy val f = ((MinusOne ^ (l1 - l2 - m3))
        * PositiveSquareRoot(TriangleCoefficient(l1, l2, l3))
        * PositiveSquareRoot(((l1 + m1).abs.!) * ((l1 - m1).abs.!) * ((l2 + m2).abs.!) * ((l2 - m2).abs.!) * ((l3 + m3).abs.!) * ((l3 - m3).abs.!))
        * Series(series _, validT))

    private def series(t: Natural): Real = (MinusOne ^ t) / ((t !)
        * (l3 - l2 + t + m1).abs.!
        * (l3 - l1 + t - m2).abs.!
        * (l1 + l2 - l3 - t).abs.!
        * (l1 - t - m1).abs.!
        * (l2 - t + m2).abs.!)

    private lazy val numT: Natural = Min(l1 + m1, l1 - m1, l2 + m2, l2 - m2, l3 + m3, l3 - m3, l1 + l2 - l3, l2 + l3 - l1, l1 + l3 - l2) + One

    /**
     * Find all values of T simply by iterating numT times whilst checking for valid values.
     */
    private lazy val validT: Seq[Natural] = new Iterable[Natural]() {

        val t1 = m1 + l3 - l2
        val t2 = l3 - l1 - m2
        val t3 = l1 + l2 - l3
        val t4 = l1 - m1
        val t5 = l2 + m2

        def iterator = new Iterator[Natural] {

            var j: Natural = Zero
            var t: Natural = Zero

            def hasNext = j < numT

            def next() = {
                j = j.succ
                var found = false
                while (!found) {
                    if (t + t1 < 0 || t + t2 < 0 || t3 - t < 0 || t4 - t < 0 || t5 - t < 0) t = t.succ
                    else found = true
                }
                val tt = t
                t = t.succ
                tt
            }

        }

    }.toSeq

    protected[this] def doEvaluate(precision: Precision) = f.evaluate(precision)

}