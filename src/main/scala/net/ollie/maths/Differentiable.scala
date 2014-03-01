package net.ollie.maths

/**
 * Something that can be differentiated with respect to a particular variable.
 * Created by Ollie on 03/01/14.
 */
trait Differentiable {

    /**
     * Differentiate with respect to the variable x.
     */
    def df(x: Variable): Differentiable

}

trait Integrable {

    /**
     * Integrate with respect to the variable x.
     */
    def integrate(x: Variable): Expression = integral(x) + new ConstantOfIntegration

    protected[this] def integral(x: Variable): Expression

}

private class ConstantOfIntegration
        extends Nonvariate {

    def isEmpty = false

    def toConstant = None

    def unary_- = this

    override def equals(e: Expression) = this eq e

    override def toString = "IntegrationConstant"

}