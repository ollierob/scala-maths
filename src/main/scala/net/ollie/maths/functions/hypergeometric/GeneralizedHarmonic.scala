package net.ollie.maths.functions.hypergeometric

import net.ollie.maths.Expression
import net.ollie.maths.numbers.{Real, Natural}

/**
 * Created by Ollie on 17/02/14.
 */
trait GeneralizedHarmonic
        extends Expression {

    def degree: Natural

    def power: Real

    override def toString = s"Harmonic($degree, $power)"

}
