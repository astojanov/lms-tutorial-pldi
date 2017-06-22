/**
  *      ____   __     ____   ____   ___   ____  ___ _____
  *     / __ \ / /    / __ \ /  _/  |__ \ / __ \<  //__  /
  *    / /_/ // /    / / / / / /    __/ // / / // /   / /
  *   / ____// /___ / /_/ /_/ /    / __// /_/ // /   / /
  *  /_/    /_____//_____//___/   /____/\____//_/   /_/
  *
  *  https://github.com/astojanov/lms-tutorial-pldi
  *  Copyright (C) 2017 Alen Stojanov (astojanov@inf.ethz.ch)
  *
  *  This program is free software: you can redistribute it and/or modify
  *  it under the terms of the GNU General Public License as published by
  *  the Free Software Foundation, either version 3 of the License, or
  *  (at your option) any later version.
  *
  *  This program is distributed in the hope that it will be useful,
  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  *  GNU General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program. If not, see http://www.gnu.org/licenses/.
  */

package ch.ethz.acl.commons.cir.extensions

import scala.lms.common.CLikeGenVariables

trait CLikeGenVariablesRename extends CLikeGenVariables {

  import IR._

  def quoteVar(x: Exp[Any]): String = x match {
    case Sym(x) if x >= 0 => "var" + x
    case _ => quote(x)
  }

  def emitVarDefRename(sym: Sym[Variable[Any]], rhs: String): Unit = {
    if(remap(sym.tp) != "void")
      stream.println(remap(sym.tp) + " " + quoteVar(sym) + " = " + rhs + ";")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ReadVar(Variable(a))             => emitValDef(sym, quoteVar(a))
    case NewVar(init)                     => emitVarDefRename(sym.asInstanceOf[Sym[Variable[Any]]], quote(init))
    case Assign(Variable(a), b)           => stream.println(quoteVar(a) + " = " + quote(b) + ";")
    case VarPlusEquals(Variable(a), b)    => stream.println(quoteVar(a) + " += " + quote(b) + ";")
    case VarMinusEquals(Variable(a), b)   => stream.println(quoteVar(a) + " -= " + quote(b) + ";")
    case VarTimesEquals(Variable(a), b)   => stream.println(quoteVar(a) + " *= " + quote(b) + ";")
    case VarDivideEquals(Variable(a), b)  => stream.println(quoteVar(a) + " /= " + quote(b) + ";")
    case _ => super.emitNode(sym, rhs)
  }

}
