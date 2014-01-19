package net.ollie.maths


/**
 * Something that can be differentiated with respect to a particular variable.
 * Created by Ollie on 03/01/14.
 */
trait Differentiable {

    def df(x: Variable): Differentiable

}
