package net.ollie.maths.numbers.massive

import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.{One, Zero}

/**
 * Created by Ollie on 12/01/14.
 * @see http://mathworld.wolfram.com/PowerTower.html
 */
object PowerTower {

    def apply(base: Real, tower: Real): Massive = new PowerTower(base, tower)

}

class PowerTower(val base: Real, val tower: Real)
        extends Massive {

    override def isEmpty = base.isEmpty || tower.isEmpty

    def toReal = base match {
        case Zero => Some(Zero)
        case One => Some(One)
        case _ => None //TODO
    }

    override def toString = s"$base ↑↑ $tower"

}