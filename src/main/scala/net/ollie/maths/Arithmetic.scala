package net.ollie.maths

import net.ollie.maths.numbers.constants.Unity

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

    def apply(n: Constant): Option[To]

    def apply(n: Option[Constant]): Option[To] = n match {
        case Some(f) => apply(f)
        case _ => None
    }

    def zero: To with Empty

    def one: To with Unity

}

trait NumberConversionArithmetic[-From, +To] {

    def apply(from: From): To

    def apply(from: Option[From]): Option[To] = from match {
        case Some(f) => Some(apply(f))
        case _ => None
    }

}