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

trait NumberIdentityArithmetic[+To] {

    def convert(n: Number): Option[To]

    def convert(n: Option[Number]): Option[To] = n match {
        case Some(f) => convert(f)
        case _ => None
    }

}

trait IdentityArithmetic[-From, +To]
        extends NumberIdentityArithmetic[To] {

    def promote(from: From): To

    def promote(from: Option[From]): Option[To] = from match {
        case Some(f) => Some(promote(f))
        case _ => None
    }

}