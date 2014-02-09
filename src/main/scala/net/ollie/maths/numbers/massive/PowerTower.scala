package net.ollie.maths.numbers.massive

import net.ollie.maths.numbers.{Zero, One, Real}

/**
 * Created by Ollie on 12/01/14.
 * @see http://mathworld.wolfram.com/PowerTower.html
 */
object PowerTower {

    def apply(base: Real, tower: Real): Massive = new PowerTower(base, tower)

}

class PowerTower(val base: Real, val tower: Real)
        extends Massive {

    def isEmpty = base.isEmpty

    def tryReduce = base match {
        case Zero => Some(Zero)
        case One => Some(One)
        case _ => None //TODO
    }

    override def toString = s"$base ↑↑ $tower"

}