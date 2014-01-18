package net.ollie.maths.numbers.real

import net.ollie.maths.numbers.RealNumber

/**
 * Created by Ollie on 12/01/14.
 */
object PowerTower {

    def apply(base: RealNumber, tower: RealNumber): MassiveNumber = new PowerTower(base, tower)

}

class PowerTower(val base: RealNumber, val tower: RealNumber)
        extends MassiveNumber {

    def isEmpty = false

    def tryReduce = None

    override def toString = s"$base ↑↑ $tower"

}