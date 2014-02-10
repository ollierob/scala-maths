package net.ollie.maths

/**
 * Created by Ollie on 04/01/14.
 */
trait Invertible
        extends Expression {

    type Inverse <: Expression

    def inverse: Inverse

}
