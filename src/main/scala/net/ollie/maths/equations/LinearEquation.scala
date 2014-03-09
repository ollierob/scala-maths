package net.ollie.maths.equations

import net.ollie.maths.{Expression, Linear, Constant}
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 09/03/14.
 * @see http://mathworld.wolfram.com/LinearEquation.html
 */
trait LinearEquation
        extends Equation {

    def left: Expression = {
        leftTerms.foldLeft(Zero.asInstanceOf[Expression])((current, term) => term match {
            case Left(c) => current + c
            case Right(l) => current + l
        })
    }

    def leftTerms: Seq[Either[Constant, Linear]]

    def right: Expression = {
        rightTerms.foldLeft(Zero.asInstanceOf[Expression])((current, term) => term match {
            case Left(c) => current + c
            case Right(l) => current + l
        })
    }

    def rightTerms: Seq[Either[Constant, Linear]]

}

/**
 *
 * @see http://mathworld.wolfram.com/LinearSystemofEquations.html
 */
class LinearEquationSystem(val equations: Seq[LinearEquation])
        extends EquationSystem {

    def trySolve = LinearEquationSystem.trySolve(equations)

}

object LinearEquationSystem {

    def trySolve(equations: Seq[LinearEquation]): Option[Solution] = {
        ???
    }

}
