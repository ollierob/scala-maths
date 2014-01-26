package net.ollie.maths.functions

/**
 * Created by Ollie on 26/01/14.
 */
trait TrivariateFunction[-F1, -F2, -F3, T] {

    def apply(f1: F1, f2: F2, f3: F3): T

}
