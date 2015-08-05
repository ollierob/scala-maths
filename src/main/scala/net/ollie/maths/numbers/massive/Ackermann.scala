package net.ollie.maths.numbers.massive

import java.util.Objects

import net.ollie.maths.Constant
import net.ollie.maths.numbers.constants.{One, Zero}
import net.ollie.maths.numbers.massive.Ackermann.AckermannCache
import net.ollie.maths.numbers.{Natural, NaturalInfinity}
import net.ollie.utils.BiValueCache

/**
 * Created by Ollie on 04/08/2015.
 */
object Ackermann {

    def apply(m: Natural, n: Natural)(implicit cache: AckermannCache): Ackermann = cache(m, n)

    def calculate(m: Natural, n: Natural, cache: AckermannCache): Ackermann = m match {
        case Zero => new ExplicitAckermann(m, n, n + 1)
        case One => new ExplicitAckermann(m, n, n + 2)
        case Natural(2) => new ExplicitAckermann(m, n, (2 * n) + 3)
        case Natural(3) => new ExplicitAckermann(m, n, (2 ^ (n + 3)) - 3)
        case _ => n match {
            case Zero => new ZeroNAckermann(m)(cache)
            case _ => new LargeAckermann(m, n)(cache)
        }
    }

    implicit def convert(a: Ackermann): Natural = a closestReal

    trait AckermannCache extends ((Natural, Natural) => Ackermann)

    implicit object AckermannCache
            extends BiValueCache[Natural, Natural, Ackermann]
            with AckermannCache {

        override protected def compute(m: Natural, n: Natural): Ackermann = calculate(m, n, AckermannCache.this)

    }

}

trait Ackermann extends Massive {

    def m: Natural

    def n: Natural

    override def closestReal: Natural = NaturalInfinity

    override def equals(that: Constant) = that match {
        case a: Ackermann => this equals a
        case _ => super.equals(that)
    }

    def equals(that: Ackermann): Boolean = (n.equals(that.n) && m.equals(that.m)) || closestReal.equals(that.closestReal)

    override def toString = s"A($m,$n)"

}

private class ExplicitAckermann(val m: Natural, val n: Natural, override val closestReal: Natural) extends Ackermann

private class ZeroNAckermann(val m: Natural)(implicit val cache: AckermannCache) extends Ackermann {

    override def n = Zero

    private lazy val closest = Ackermann(m - 1, 1)(cache)

    override def closestReal = {
        System.out.println(s"A($m,$n) = A(" + (m - 1) + ",1)")
        closest
    }

    override def equals(that: Ackermann) = (m.equals(that.m + 1) && One.equals(that.n)) || super.equals(that)

}

private class LargeAckermann(val m: Natural, val n: Natural)(implicit val cache: AckermannCache) extends Ackermann {

    private lazy val closest = {
        System.out.println(s"A($m,$n) = A(" + (m - 1) + s",A($m," + (n - 1) + ")")
        Ackermann(m - 1, Ackermann(m, n - 1)(cache))(cache)
    }

    override def closestReal = closest

    override def hashCode() = Objects.hash(n, m)

}