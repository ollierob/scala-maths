package net.ollie.maths

/**
 * Created by Ollie on 01/03/14.
 */
trait Integrable {

    /**
     * Integrate with respect to the variable x.
     */
    def integrate(x: Variable): Expression = integral(x) + new IntegrationConstant

    protected[this] def integral(x: Variable): Expression

}

private class IntegrationConstant
        extends Nonvariate {

    def isEmpty = false

    def toConstant = None

    def unary_- = this

    override def equals(e: Expression) = this eq e

    override def toString = "IntegrationConstant"

}