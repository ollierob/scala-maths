package net.ollie.maths.numbers

/**
 * Created by Ollie on 12/01/14.
 */
object PowerTower {

    def apply(base: Real, tower: Real): Massive = new PowerTower(base, tower)

}

class PowerTower(val base: Real, val tower: Real)
        extends Massive {

    def isEmpty = false

    def tryReduce = None

    override def toString = s"$base ↑↑ $tower"

}