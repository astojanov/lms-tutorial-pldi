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

package tutorial.example0

import ch.ethz.acl.commons.cir.extensions.{CGenForOps, ForLoopExp}
import ch.ethz.acl.intrinsics._
import ch.ethz.acl.passera.unsigned._

import scala.lms.common._

object Example0 {
  //
  // Create the DSL or the LMS Intermediate Representation (IR).
  // This IR will support AVX and AVX2 instructions. In order to
  // make it work, lets make sure that it also supports arrays,
  // booleans operations and primitve nummeric operations.
  //
  val AVX2_IR = new PrimitiveOpsExpOpt
    with NumericOpsExpOpt 
    with BooleanOpsExp
    with ArrayOpsExpOpt
    with SeqOpsExp
    with IntrinsicsBase
    with IntrinsicsArrays
    with AVX
    with AVX2
    with ForLoopExp
  { self =>
    //
    // Since the Intrinsics also work with unsigned types
    // we must include those in the DSL. Pure LMS does not
    // take care of unsigned yet.
    //
    implicit def anyTyp    : Typ[Any]    = manifestTyp
    implicit def uByteTyp  : Typ[UByte]  = manifestTyp
    implicit def uIntTyp   : Typ[UInt]   = manifestTyp
    implicit def uLongTyp  : Typ[ULong]  = manifestTyp
    implicit def uShortTyp : Typ[UShort] = manifestTyp
    //
    // Create a C code generator. And make sure you can
    // generate code for each included node IR that we
    // have mixed in the DSL
    //
    val codegen = new CGenNumericOps
      with CGenPrimitiveOps
      with CGenBooleanOps
      with CGenArrayOps
      with CGenSeqOps
      with CGenAVX
      with CGenAVX2
      with CGenForOps
    {
      val IR: self.type = self
      //
      // Make sure that we avoid references when those are not required
      //
      override def remapWithRef[A](m: Typ[A]) = remap(m) + " "
      override def remapWithRef(tpe: String) = tpe
      //
      // Make sure that the Arrays are remapped properly
      //
      override def remap[A](m: Typ[A]) : String = {
        if (m.erasure.isArray) {
          remap(m.typeArguments.head) + "*"
        } else {
          super.remap(m)
        }
      }
    }
  }
  //
  // Make each AVX intrinsic available in the context that we work
  //
  import AVX2_IR._
  //
  // Stage a simple function that takes two arrays of Floats, a and b
  // Extracts 8 elements from the first array, adds them, and writes
  // the result to array b. LMS assumes that both a and b are immutable.
  // Therefore, lets inform LMS that b will hold our results.
  //
  def pointwise_add_simd(x: Rep[Array[Double]], y: Rep[Array[Double]], z_sym: Rep[Array[Double]], N: Rep[Int]): Rep[Unit] = {
    val z = reflectMutableSym(z_sym.asInstanceOf[Sym[Array[Double]]])
    loop(N, Const(4), (i: Rep[Int]) => {
      val a = _mm256_loadu_pd(x, i)
      val b = _mm256_loadu_pd(y, i)
      val r = _mm256_add_pd(a, b)
      _mm256_storeu_pd(z, r, i)
    })
  }

  def main(args: Array[String]) : Unit = {
    val source = new java.io.StringWriter()
    val writer = new java.io.PrintWriter(source)
    //
    // generate the C code for the add_first_8elements
    //
    codegen.emitSource4(pointwise_add_simd, "pointwise_add_simd", writer)
    //
    // Print the AVX2 headers
    //
    codegen.getIntrinsicsHeaders().map(h => {
      println("#include <" + h + ">")
    })
    //
    // Print the rest of the code to System.out
    //
    println(source.toString)
  }

}
