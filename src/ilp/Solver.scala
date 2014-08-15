package edu.cmu.lti.nlp.amr.ilp

import net.sf.javailp
import javailp.{Operator, VarType, Term, Constraint, OptType}
import Operator.{EQ, LE, GE}
import VarType.{INT, BOOL, REAL}
import scala.collection.JavaConversions._


sealed abstract class Var(val name: String, val varType: VarType) {
  def * (coefficient: Number): Linear = Linear(Seq(new Term(this, coefficient)))
  override def toString: String = name
}

object Var {
  implicit def asLinear(v: Var): Linear = v * 1
}

case class IntVar(override val name: String) extends Var(name, INT)

case class BoolVar(override val name: String) extends Var(name, BOOL)

case class RealVar(override val name: String) extends Var(name, REAL)

case class Linear(terms: Seq[Term]) {
  def plus (other: Linear): Linear = Linear(terms ++ other.terms)
  def minus (other: Linear): Linear = {
    Linear(terms ++ other.terms.map(t => new Term(t.getVariable, -t.getCoefficient.doubleValue)))
  }

  def equiv(x: Number): Constraint = new Constraint(toString.take(255), this, EQ, x)
  def lteq(x: Number): Constraint = new Constraint(toString.take(255), this, LE, x)
  def gteq(x: Number): Constraint = new Constraint(toString.take(255), this, GE, x)
}

object Linear {
  implicit def toLinear(linear: Linear): javailp.Linear = new javailp.Linear(linear.terms)
}

object Ops {
  implicit def plus (left: Linear, right: Linear) = left plus right
  implicit def minus (left: Linear, right: Linear) = left minus right
}

sealed class Objective(val linear: javailp.Linear, val optType: OptType)
case class Minimize(override val linear: javailp.Linear) extends Objective(linear, OptType.MIN)
case class Maximize(override val linear: javailp.Linear) extends Objective(linear, OptType.MAX)

case class Problem(variables: Iterable[Var],
                   objective: Objective,
                   constraints: Iterable[Constraint])
object Problem {
  implicit def asJavaIlpProblem(p: Problem): javailp.Problem = {
    val problem = new javailp.Problem
    p.variables.foreach(v => problem.setVarType(v, v.varType))
    p.constraints.foreach(problem.add)
    problem.setObjective(p.objective.linear, p.objective.optType)
    problem
  }
}

case class Solver(solverFactory: javailp.SolverFactory) {
  def solve(problem: Problem): javailp.Result = solverFactory.get.solve(problem)
}
