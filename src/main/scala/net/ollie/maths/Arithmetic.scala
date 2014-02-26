package net.ollie.maths

import net.ollie.maths.numbers.constants.{Unity, One, Zero}

/**
 * Elementary arithmetic operators.
 * Created by Ollie on 01/01/14.
 */
trait AdditionArithmetic[-Left, -Right, +Result] {

    def zero: Result with Empty

    def add(left: Left, right: Right): Result

}

trait MultiplicationArithmetic[-Left, -Right, +Result] {

    def one: Result with Unity

    def multiply(left: Left, right: Right): Result

}

trait ExponentiationArithmetic[-Base, -Power, +Result] {

    def exponentiate(base: Base, power: Power): Result

}

trait TetrationArithmetic[-Base, -Tower, +Result] {

    def tetrate(base: Base, tower: Tower): Result

}

trait NumberIdentityArithmetic[+To] {

    def apply(n: Number): Option[To]

    def apply(n: Option[Number]): Option[To] = n match {
        case Some(f) => apply(f)
        case _ => None
    }

    def zero: To with Empty

    def one: To with Unity

}

trait IdentityArithmetic[-From, +To] {

    def promote(from: From): To

    def promote(from: Option[From]): Option[To] = from match {
        case Some(f) => Some(promote(f))
        case _ => None
    }

}