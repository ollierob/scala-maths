package net.ollie.maths.functions

/**
 * Created by Ollie on 05/01/14.
 */
trait BivariateFunction[-F1, -F2, +T] {

    def apply(f1: F1, f2: F2): T

}

trait SymmetricBivariateFunction[-F, +T]
        extends BivariateFunction[F, F, T]

trait HomogeneousBivariateFunction[F]
        extends SymmetricBivariateFunction[F, F]
