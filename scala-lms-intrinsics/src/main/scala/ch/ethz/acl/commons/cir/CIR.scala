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

package ch.ethz.acl.commons.cir

import ch.ethz.acl.commons.cir.codegen.CCodegen
import ch.ethz.acl.commons.cir.extensions._
import ch.ethz.acl.commons.util.{DSLUtils, Debugging, TheTyp, Utilities}

import scala.reflect.SourceContext
import scala.lms.common._
import java.io._

trait CIR extends BaseFatExp
  with CommentExp
  with SwitchExp
  with ExceptionOpsExp
  with ArrayOpsExpOptExtra
  with ForLoopExpOpt
  with CNumericExpOpt
  with NumericMathOpsExp
  with EqualExpBridgeOpt
  with BooleanOpsExpOpt
  with OrderingOpsExp
  with IfThenElseExpOpt
  with WhileExp with WhileMirror
  with VariablesExpOpt
  with HeapArrayExpOpt

  with DSLUtils
  with Debugging
{ self =>

  val codegen: CCodegen { val IR: self.type }

  object ImplicitLift {

    // Define methods to lift variables
    def __newVar[T:Typ](init: T)(implicit pos: SourceContext): Var[T] = var_new(unit(init))
    def __newVar[T](init: Rep[T])(implicit o: Overloaded1, mT: Typ[T], pos: SourceContext): Var[T] = var_new(init)
    def __newVar[T](init: Var[T])(implicit o: Overloaded2, mT: Typ[T], pos: SourceContext): Var[T] = var_new(init)

    // Lift all numeric variables
    implicit def numericToNumericRep[T:Numeric:Typ](x: T): Rep[T] = unit(x)
  }

  def compile1[A:Typ, R:Typ](f: Exp[A] => Exp[R], fName: String): Unit = {
    val arg0 = fresh[A]
    val block = reifyEffects[R](f(arg0))
    implicit val mL = List(typ[A]).asInstanceOf[List[Typ[Any]]]
    compile(List(arg0), block, fName)
  }

  def compile2[A:Typ, B:Typ, R:Typ](f: (Exp[A], Exp[B]) => Exp[R], fName: String): Unit = {
    val arg0 = fresh[A]
    val arg1 = fresh[B]
    val block = reifyEffects[R](f(arg0, arg1))
    implicit val mL = List(typ[A], typ[B]).asInstanceOf[List[Typ[Any]]]
    compile(List(arg0, arg1), block, fName)
  }

  def compile3[A:Typ, B:Typ, C:Typ, R:Typ](f: Function3[Exp[A], Exp[B], Exp[C], Exp[R]], fName: String): Unit = {
    val arg0 = fresh[A]
    val arg1 = fresh[B]
    val arg2 = fresh[C]
    val block = reifyEffects[R](f(arg0, arg1, arg2))
    implicit val mL = List(typ[A], typ[B], typ[C]).asInstanceOf[List[Typ[Any]]]
    compile(List(arg0, arg1, arg2), block, fName)
  }

  def compile4[A:Typ, B:Typ, C:Typ, D:Typ, R:Typ](f: (Exp[A], Exp[B], Exp[C], Exp[D]) => Exp[R], fName: String): Unit = {
    val arg0 = fresh[A]
    val arg1 = fresh[B]
    val arg2 = fresh[C]
    val arg3 = fresh[D]
    val block = reifyEffects[R](f(arg0, arg1, arg2, arg3))
    implicit val mL = List(typ[A], typ[B], typ[C], typ[D]).asInstanceOf[List[Typ[Any]]]
    compile(List(arg0, arg1, arg2, arg3), block, fName)
  }

  def compile[B](inputs: List[Sym[Any]], block: Block[B], fName: String): Unit = {
    val cApp = codegen.generateApplication(inputs, block, fName)
    println(cApp.generateSingleFile())
  }

  def emitBlock[B] (
    inputs: List[Sym[Any]],
    block: Block[B],
    stream: PrintWriter,
    funcName: String = "staged"
  ): Unit = {
    val app = codegen.generateApplication[B](inputs, block, funcName)
    stream.println(app.generateSingleFile())
    stream.close()
  }

  def dumpCode[B] (name: String, inputs: List[Sym[Any]], block: Block[B]): Unit = {
    emitBlock(inputs, block, new PrintWriter(name + ".c"))
  }

}
