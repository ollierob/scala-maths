package net.ollie.maths.numbers.real

import net.ollie.maths.Expression
import net.ollie.maths.numbers.RealNumber

/**
 * Created by Ollie on 12/01/14.
 */
class PowerTower(base: RealNumber, tower: RealNumber)
        extends MassiveNumber {

    def isEmpty = false

    def tryReduce = None

    def inverse: Expression = ???

}

object PowerTower {

}