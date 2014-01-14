package net.ollie.maths.functions


/**
 * Created by Ollie on 05/01/14.
 */
trait UnivariateFunction[-F, +T] {

    def apply(f: F): T

}
