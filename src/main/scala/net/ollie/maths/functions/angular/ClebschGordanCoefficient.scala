package net.ollie.maths.functions.angular

import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.functions.{Modal, ZeroModal}
import net.ollie.maths.numbers.constants.MinusOne
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.tensors.KroneckerDelta

trait ClebschGordanCoefficient
    extends Real {

    def y1: Modal

    def y2: Modal

    def y3: Modal

    override def toString = s"<$y1,$y2|$y3>"

}

object ClebschGordanCoefficient {

    def apply(y1: Modal, y2: Modal, y3: Modal): ClebschGordanCoefficient = {
        //if (y2.l == 0) new KroneckerDeltaClebschGordanCoefficient(y1, y3)
        //else
        new Wigner3JBasedClebschGordanCoefficient(y1, y2, y3)
    }

}

class KroneckerDeltaClebschGordanCoefficient(val y1: Modal, val y3: Modal)
    extends ClebschGordanCoefficient {

    override def y2 = ZeroModal

    private lazy val value = KroneckerDelta(y1.l, y3.l) * KroneckerDelta(y1.m, y3.m)

    override def evaluate(precision: Precision) = value.evaluate(precision)

    override def isEmpty = value.isEmpty //TODO

}

class Wigner3JBasedClebschGordanCoefficient(val y1: Modal, val y2: Modal, val y3: Modal)
    extends ClebschGordanCoefficient {

    private lazy val wigner: Wigner3j = Wigner3j((y1.l, y1.m), (y2.l, y2.m), (y3.l, -y3.m))
    private lazy val value = (MinusOne ^ (y2.l - y1.l - y3.m)) * PositiveSquareRoot((2 * y3.l) + 1) * wigner

    override def evaluate(precision: Precision) = value.evaluate(precision)

    override def isEmpty = wigner.isEmpty

}
