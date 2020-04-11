package net.ollie.maths.tensors.vectors

import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.FunctionBuilder
import net.ollie.maths.numbers.constants.{One, Zero}
import net.ollie.maths.tensors.{ContravariantTensor, Tensor}

/**
 * Created by Ollie on 08/03/14.
 */

trait Vector[B <: VectorBasis]
        extends ContravariantTensor[One.type] {

    require(terms.size == magnitude.toInt.get)

    def valence = (Zero, One)

    implicit def basis: B

    def magnitude = basis.magnitude

    def terms: Seq[Expression]

    def toConstant: Option[ConstantVector[B]]

    def dual: Covector[B] = ???

    override def +(that: Tensor[Zero.type, One.type]): Vector[B] = this + Vector(that)

    def +(that: Vector[B]): Vector[B] = Vector.add(this, that)

    def dot(that: Vector[B]): Expression = Vector.dot(this, that)

    def cross(that: Vector[B]): Vector[B] = Vector.cross(this, that)

    def *(that: Expression): Vector[B] = Vector.multiply(this, that)

}

object Vector {

    def apply[B <: VectorBasis](tensor: Tensor[Zero.type, One.type])(implicit basis: B): Vector[B] = {
        tensor match {
            //case v: Vector[B] => v
            case _ => ???
        }
    }

    def add[B <: VectorBasis](left: Vector[B], right: Vector[B]): Vector[B] = {
        ???
    }

    def dot[B <: VectorBasis](left: Vector[B], right: Vector[B]): Expression = {
        ???
    }

    def cross[B <: VectorBasis](left: Vector[B], right: Vector[B]): Vector[B] = {
        ???
    }

    def multiply[B <: VectorBasis](vector: Vector[B], scalar: Expression): Vector[B] = {
        ???
    }

    implicit class VectorApplication(val f: FunctionBuilder) {

        def of[B <: VectorBasis](v: Vector[B]): Vector[B] = new AppliedVector(f, v)

    }

}

class AppliedVector[B <: VectorBasis](val f: FunctionBuilder, val base: Vector[B])
        extends Vector[B] {

    def toConstant = ???

    def terms = base.terms.map(f(_))

    def basis = base.basis

}
