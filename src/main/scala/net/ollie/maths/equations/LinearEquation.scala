package net.ollie.maths.equations

import net.ollie.maths.{Linear, Constant}

/**
 * Created by Ollie on 09/03/14.
 * @see http://mathworld.wolfram.com/LinearEquation.html
 */
trait LinearEquation
        extends Equation {

    def leftTerms: Seq[Either[Constant, Linear]]

    def rightTerms: Seq[Either[Constant, Linear]]

}

/**
 *
 * @see http://mathworld.wolfram.com/LinearSystemofEquations.html
 */
trait LinearEquationSystem
        extends EquationSystem {

    def equations: Seq[LinearEquation]

}
