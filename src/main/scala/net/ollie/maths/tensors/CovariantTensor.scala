package net.ollie.maths.tensors

import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.constants.Zero

/**
 * Purely covariant tensors. Also known as m-forms.
 * Created by Ollie on 09/03/14.
 * @see [[ContravariantTensor]]
 */
trait CovariantTensor[R <: Natural]
        extends Tensor[R, Zero.type]