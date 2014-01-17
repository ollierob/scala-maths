package net.ollie.maths

import net.ollie.maths.numbers.PositiveRealNumber

/**
 * Created by Ollie on 02/01/14.
 */
trait Number
        extends Nonvariate
        with Invertible
        with Differentiable {

    type System >: this.type <: Number with Expression

    final def narrow: System = this

    def abs: PositiveRealNumber

    def inverse: Number

    def ?+(that: Number): Option[Number]

    def +[R <: Number, Combined <: Number](that: R)
                                          (implicit addition: AdditionArithmetic[System, R#System, Combined]): Combined = {
        addition.add(this, that.narrow)
    }

    def ?*(that: Number): Option[Number]

    def *[R <: Number, Combined <: Number](that: R)
                                          (implicit multiplication: MultiplicationArithmetic[System, R#System, Combined]): Combined = {
        multiplication.multiply(this, that.narrow)
    }

    /**
     * Exponentiation.
     * @param power
     * @param exponentiation
     * @tparam R power type
     * @tparam Combined result type
     * @return a number
     */
    def ^[R <: Number, Combined <: Number](power: R)
                                          (implicit exponentiation: ExponentiationArithmetic[System, R#System, Combined]): Combined = {
        exponentiation.exponent(this, power.narrow)
    }

    def ?^(that: Number): Option[Number]

    /**
     * Tetration.
     * @param tower
     * @param tetration
     * @tparam R
     * @tparam Combined
     * @return
     * @see http://mathworld.wolfram.com/PowerTower.html
     */
    def ^^[R <: Number, Combined <: Number](tower: R)
                                           (implicit tetration: TetrationArithmetic[System, R#System, Combined]): Combined = {
        tetration.tetrate(this, tower.narrow)
    }

    override def df(x: Variable): System with Empty

    def toConstant: Option[System] = Some(narrow)

    def replace(variables: Map[Variable, Expression]): System = narrow

    final override def equals(expr: Expression) = expr match {
        case n: Number => this.equals(n)
        case _ => expr.toConstant match {
            case Some(n: Number) => this.equals(n)
            case _ => super.equals(expr)
        }
    }

    def equals(n: Number) = super.equals(n)

}