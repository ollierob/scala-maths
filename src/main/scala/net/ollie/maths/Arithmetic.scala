package net.ollie.maths

/**
 * Elementary arithmetic operators.
 * Created by Ollie on 01/01/14.
 */
trait AdditionArithmetic[-Left, -Right, +Combined] {

    def zero: Combined

    def add(left: Left, right: Right): Combined

}

trait MultiplicationArithmetic[-Left, -Right, +Combined] {

    def zero: Combined

    def one: Combined

    def multiply(left: Left, right: Right): Combined

}

trait ExponentiationArithmetic[-Left, -Right, +Combined] {

    def exponent(base: Left, power: Right): Combined

}

trait TetrationArithmetic[-Left, -Right, +Combined] {

    def tetrate(base: Left, tower: Right): Combined

}

trait IdentityArithmetic[-From, +To] {

    def convert(from: From): To

    def convert(from: Option[From]): Option[To] = from match {
        case Some(f) => Some(convert(f))
        case _ => None
    }

}

trait NumberIdentityArithmetic[To] {

    def convert(from: Number): Option[To]

    def convert(from: Option[Number]): Option[To] = from match {
        case Some(f) => convert(f)
        case _ => None
    }

}