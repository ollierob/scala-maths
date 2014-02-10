package net.ollie.maths.numbers.complex

import net.ollie.maths.numbers.{MaybeReal, Real}
import net.ollie.maths.{NumberIdentityArithmetic, Number}

/**
 * Created by Ollie on 01/02/14.
 */
trait ComplexLike
        extends Number
        with MaybeReal {

    self =>

    type System >: this.type <: ComplexLike {type System = self.System}

    type Inverse = System

    implicit protected[this] def builder: ComplexBuilder[System]

    def re: Real

    def unre: Real

    override def isEmpty: Boolean = re.isEmpty && unre.isEmpty

    override def unary_-(): System = builder(-this.re, -this.unre)

    def conjugate: System = builder(this.re, -this.unre)

    def inverse: System = {
        val n: System = conjugate
        val d: Real = re.squared - (builder.unitSquared * unre.squared)
        n / d
    }

    def toReal: Option[Real] = if (unre.isEmpty) Some(re) else None

    def +(that: Real): System = builder(this.re + that, this.unre)

    def -(that: Real): System = builder(this.re - that, this.unre)

    def *(that: Real): System = builder(this.re * that, this.unre * that)

    def /(that: Real): System = builder(this.re / that, this.unre / that)

    def +(that: System): System = {
        val r: Real = this.re + that.re
        val u: Real = this.unre + that.unre
        builder(r, u)
    }

    def -(that: System): System = {
        val r: Real = this.re - that.re
        val u: Real = this.unre - that.unre
        builder(r, u)
    }

    def *(that: System): System = {
        val r: Real = (this.re * that.re) + (builder.unitSquared * this.unre * that.unre)
        val u: Real = (this.re * that.unre) + (this.unre * that.re)
        builder(r, u)
    }

    def /(that: System): System = this * that.inverse

    override def equals(n: Number) = n match {
        case re: Real => unre.isEmpty && this.re == re
        case z: ComplexLike => this.equals(z)
        case _ => super.equals(n)
    }

    def equals(z: ComplexLike): Boolean = super.equals(z)

}

trait ComplexBuilder[System]
        extends NumberIdentityArithmetic[System] {

    def unitSquared: Real

    def apply(pair: (Real, Real)): System

}
