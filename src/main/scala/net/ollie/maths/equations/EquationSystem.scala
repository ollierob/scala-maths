package net.ollie.maths.equations

import net.ollie.maths.{Variable, Expression}

/**
 * Created by Ollie on 09/03/14.
 */
trait EquationSystem {

    def equations: Seq[Equation]

    def variables = equations.map(_.variables).flatten.toSet

    def replace(variables: Map[Variable, Expression]): EquationSystem

    def trySolve: Option[Solution]

}
