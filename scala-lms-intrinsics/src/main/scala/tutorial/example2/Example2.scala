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

package tutorial.example2

import ch.ethz.acl.commons.cir.AVX2_IR
import ch.ethz.acl.commons.isa.ISA._

object Example2 extends SVectors {

  val IR = new AVX2_IR {}
  import IR._

  var currentISA = AVX

  def dot[T:Numeric:Typ](x: Rep[Array[T]], y: Rep[Array[T]], len: Rep[Int]): Rep[T] = {

    import ImplicitLift._
    val ut = new SUtil[T](currentISA)
    val acc = ut.zero().toVar()
    val sx = new SArray[T](currentISA, x, len)
    val sy = new SArray[T](currentISA, y, len)

    loop(sx.size(), (i: Rep[Int]) => {
      val tmp = sx(i) * sy(i)
      acc.assign(acc.read() + tmp)
    })

    var result = acc.read().reduce()

    loop(len - sx.scalar_size(), (i: Rep[Int]) => {
      val idx = sx.scalar_size() + i
      val tmp = x(idx) + y(idx)
      result += tmp
    })

    result
  }

  def compileWithParams[T:Numeric:Typ](isa: ISA, fName: String): Unit = {

    val tp = implicitly[Typ[T]]
    println()
    println("/* ======================================================================================= */")
    println("/* Dot Product using " + tp.toString + " ISA: " + isa.toString + " */")
    println("/* ======================================================================================= */")

    currentISA = isa
    val x   = fresh[Array[T]]
    val y   = fresh[Array[T]]
    val len = fresh[Int]
    val block = reifyEffects(dot[T](x, y, len))
    implicit val mL = List(typ[Array[T]], typ[Array[T]], typ[Int]).asInstanceOf[List[Typ[Any]]]
    compile(List(x, y, len), block, fName)
  }

  def main(arg: Array[String]): Unit = {
    compileWithParams[Double](AVX, "dot_AVX_Double")
    compileWithParams[Float](AVX, "dot_AVX_Float")
    compileWithParams[Double](SSE, "dot_SSE_Double")
    compileWithParams[Byte](SSE, "dot_SSE_Int")
  }

}


