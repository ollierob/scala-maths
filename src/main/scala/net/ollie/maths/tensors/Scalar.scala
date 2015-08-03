package net.ollie.maths.tensors

import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.{Constant, Expression}

/**
 * Created by Ollie on 09/03/14.
 */
trait Scalar
        extends ContravariantTensor[Zero.type]
        with CovariantTensor[Zero.type] {

    def valence = (Zero, Zero)

    def dual: Scalar = this

    override def +(that: Tensor[Zero.type, Zero.type]): Scalar = this + Scalar(that)

    def +(that: Scalar): Scalar = this.value + that.value

    def value: Expression

    def toConstant: Option[ConstantScalar] = value.toConstant match {
        case Some(n) => Some(Scalar(n))
        case _ => None
    }

}

object Scalar {

    implicit def apply(expr: Expression): Scalar = expr.toConstant match {
        case Some(n) => apply(n)
        case _ => new ScalarExpression(expr)
    }

    def apply(n: Constant): ConstantScalar = ???

    implicit def apply(tensor: Tensor[Zero.type, Zero.type]): Scalar = {
        tensor match {
            case s: Scalar => s
            case _ => new ScalarTensor(tensor)
        }
    }

}

trait ConstantScalar
        extends Scalar
        with ConstantTensor[Zero.type, Zero.type] {

    def value: Element

    override def toConstant = Some(this)

}

private class ScalarExpression(val value: Expression)
        extends Scalar

private class ScalarTensor(val tensor: Tensor[Zero.type, Zero.type])
        extends Scalar {

    def value = ??? //TODO

}