package net.ollie.maths.numbers.quaternion


/**
 * Created by Ollie on 11/01/14.
 */
class ConjugatedQuaternion(q: Quaternion)
        extends AnyRef
        with Quaternion {

    def re = q.re

    def i = -(q.i)

    def j = -(q.j)

    def k = -(q.k)

    override def conjugate = q

    override def toString = s"($q)*"

}
