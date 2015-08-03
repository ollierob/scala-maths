package net.ollie.maths.tensors

import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.constants.Zero

/**
 * Purely contravariant tensors. Also referred to as n-vectors.
 * Created by Ollie on 09/03/14.
 * @see [[CovariantTensor]]
 */
trait ContravariantTensor[R <: Natural]
        extends Tensor[Zero.type, R]
