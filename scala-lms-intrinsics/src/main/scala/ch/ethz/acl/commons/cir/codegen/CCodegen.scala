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

package ch.ethz.acl.commons.cir.codegen

import ch.ethz.acl.commons.cir.CIR
import ch.ethz.acl.commons.cir.extensions._

import scala.lms.common._

trait CCodegen extends CUnparser
  with GenCast
  with GenComment
  with CGenSwitch
  with CGenExceptionOps
  with CGenArrayOpsExpOptExtra
  with CGenForOps

  with CGenPrimitiveOps
  with CLikeGenNumericOps
  with CGenNumericMathOps
  with CLikeGenEqual
  with CLikeGenOrderingOps
  with CLikeGenBooleanOps

  with CLikeGenVariablesRename
  with CGenIfThenElseOpt
  with CGenWhileMirror
  with CGenHeapArray
{
  val IR: CIR
}