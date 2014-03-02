package net.ollie.maths

import java.util.concurrent.atomic.AtomicLong
import net.ollie.maths.numbers.Multivalued
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 01/03/14.
 */
trait Integrable {

    /**
     * Integrate with respect to the variable x. Adds an integration constant.
     */
    def integrate(x: Variable): Expression = {
        integral(x) + new IntegrationConstant
    }

    protected[this] def integral(x: Variable): Expression

}

private object IntegrationConstant {

    val counter = new AtomicLong(0L);

}

class IntegrationConstant
        extends Multivalued {

    type Contents = Constant

    def isEmpty = false

    def principal = Zero

    def inverse = this

    override def unary_-() = this

    private val id = IntegrationConstant.counter.getAndIncrement()

    override def toString = s"IntegrationConstant:$id"

}