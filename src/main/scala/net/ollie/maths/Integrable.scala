package net.ollie.maths

import java.util.concurrent.atomic.AtomicLong

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

private object IntegrationConstant {

    val counter = new AtomicLong(0L);

}

class IntegrationConstant
        extends Nonvariate {

    private val id = IntegrationConstant.counter.getAndIncrement()

    def isEmpty = false

    def toConstant = None

    def unary_- = this

    override def equals(e: Expression) = this eq e

    override def toString = s"IntegrationConstant:$id"

}