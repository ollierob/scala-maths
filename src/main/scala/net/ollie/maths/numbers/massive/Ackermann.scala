package net.ollie.maths.numbers.massive

import java.util.Objects

import net.ollie.maths.Constant
import net.ollie.maths.numbers.constants.{One, Zero}
import net.ollie.maths.numbers.{Natural, NaturalInfinity}
import net.ollie.utils.BiValueCache

/**
 * Created by Ollie on 04/08/2015.
 */
object Ackermann {

    def apply(m: Natural, n: Natural)(implicit cache: BiValueCache[Natural, Natural, Ackermann]): Ackermann = cache.get(m, n)

    def calculate(m: Natural, n: Natural): Ackermann = m match {
        case Zero => new ZeroMAckermann(n)
        case _ => n match {
            case Zero => new ZeroNAckermann(m)
            case _ => new LargeAckermann(m, n)
        }
    }

    implicit def convert(a: Ackermann): Natural = a closestReal

    implicit object AckermannCache extends BiValueCache[Natural, Natural, Ackermann](calculate)

}

trait Ackermann extends Massive {

    def m: Natural

    def n: Natural

    override def closestReal: Natural = NaturalInfinity

    override def equals(that: Constant) = that match {
        case a: Ackermann => this equals a
        case _ => super.equals(that)
    }

    def equals(that: Ackermann): Boolean = n.equals(that.n) && m.equals(that.m)

    override def toString = s"A($m,$n)"

}

private class ZeroMAckermann(val n: Natural) extends Ackermann {

    override def m = Zero

    override def closestReal: Natural = n + 1

}

private class ZeroNAckermann(val m: Natural) extends Ackermann {

    override def n = Zero

    private lazy val closest = Ackermann(m - 1, 1)

    override def closestReal = closest

    override def equals(that: Ackermann) = (m.equals(that.m + 1) && One.equals(that.n)) || super.equals(that)

}

private class LargeAckermann(val m: Natural, val n: Natural) extends Ackermann {

    private lazy val closest = Ackermann(m - 1, Ackermann(m, n - 1))

    override def closestReal = closest

    override def hashCode() = Objects.hash(n, m)

}