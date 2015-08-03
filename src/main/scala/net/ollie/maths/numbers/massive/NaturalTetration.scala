package net.ollie.maths.numbers.massive

import net.ollie.maths.TetrationArithmetic
import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.constants.{One, Zero}

/**
 * Tetration.
 * Created by Ollie on 12/01/14.
 * @see http://mathworld.wolfram.com/Tetration.html
 */
object NaturalTetration {

    def apply(base: Natural, tower: Natural): NaturalTetration = new NaturalTetration(base, tower)

}

class NaturalTetration(val a: Natural, val n: Natural)
        extends Massive {

    override def isEmpty = a.isEmpty || n.isEmpty

    override def closestReal: Natural = a match {
        case Zero => Zero
        case One => One
        case _ => n match {
            case Zero | One => One
            case _ => a ^ NaturalTetration(a, n - 1).closestReal
        }
    }

    override def toString = s"$a ↑↑ $n"

    implicit object NaturalTetrationArithmetic
            extends TetrationArithmetic[Natural, Natural, Massive] {

        override def tetrate(base: Natural, tower: Natural) = NaturalTetration(base, tower)

    }

}