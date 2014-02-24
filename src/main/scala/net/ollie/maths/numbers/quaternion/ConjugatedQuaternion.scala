package net.ollie.maths.numbers.quaternion


/**
 * Created by Ollie on 11/01/14.
 */
class ConjugatedQuaternion(val of: Quaternion)
        extends AnyRef
        with Quaternion {

    override def isEmpty = of.isEmpty

    def re = of.re

    def i = -(of.i)

    def j = -(of.j)

    def k = -(of.k)

    override def conjugate = of

    override def toString = s"($of)*"

}
