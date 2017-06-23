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

package tutorial.example1

import ch.ethz.acl.commons.cir.AVX2_IR

object Example1 {

  val IR = new AVX2_IR {

    override def _mm256_add_pd(a: Exp[__m256d], b: Exp[__m256d]) = {
      (a, b) match {
        case (Def(MM256_SHUFFLE_PD(x1, y1, inm1)), Def(MM256_SHUFFLE_PD(x2, y2, inm2)))
          if inm1 == inm2 && (inm1 == Const(0x0) || inm2 == Const(0xF)) => {
          val x = _mm256_add_pd(x1, x2)
          val y = _mm256_add_pd(y1, y2)
          _mm256_shuffle_pd(x, y, inm1)
        }
        case _ => super._mm256_add_pd(a, b)
      }
    }

    override def _mm256_unpacklo_pd(a: Exp[__m256d], b: Exp[__m256d]) = {
      (a, b) match {
        case (Def(MM256_SHUFFLE_PD(x1, y1, inm1)), Def(MM256_SHUFFLE_PD(x2, y2, inm2)))
          if x1 == x2 && y1 == y2 && (inm1 == Const(0x0) && inm2 == Const(0xF)) => x1
        case _ => super._mm256_unpacklo_pd(a, b)
      }
    }

    override def _mm256_unpackhi_pd(a: Exp[__m256d], b: Exp[__m256d]) = {
      (a, b) match {
        case (Def(MM256_SHUFFLE_PD(x1, y1, inm1)), Def(MM256_SHUFFLE_PD(x2, y2, inm2)))
          if x1 == x2 && y1 == y2 && (inm1 == Const(0x0) && inm2 == Const(0xF)) => y1
        case _ => super._mm256_unpackhi_pd(a, b)
      }
    }

    override def _mm256_sub_pd(a: Exp[__m256d], b: Exp[__m256d]) = {
      (a, b) match {
        case (x, y) if x == y => _mm256_setzero_pd()
        case _ => super._mm256_sub_pd(a, b)
      }
    }
  }
  import IR._


  case class ComplexElement(re: Rep[__m256d], im: Rep[__m256d]) {

    def add(a: ComplexElement) = {
      val result_re = _mm256_add_pd(a.re, re)
      val result_im = _mm256_add_pd(a.im, im)
      ComplexElement (result_re, result_im)
    }

    def sub(a: ComplexElement) = {
      val result_re = _mm256_sub_pd(re, a.re)
      val result_im = _mm256_sub_pd(im, a.im)
      ComplexElement (result_re, result_im)
    }

    def mul (a: ComplexElement) = {
      val m1 = _mm256_mul_pd(re, a.re)
      val m2 = _mm256_mul_pd(im, a.im)
      val m3 = _mm256_mul_pd(re, a.im)
      val m4 = _mm256_mul_pd(im, a.re)
      val result_re = _mm256_sub_pd(m1, m2)
      val result_im = _mm256_add_pd(m3, m4)
      ComplexElement(result_re, result_im)
    }

  }


  class ComplexArray(val data: Rep[Array[Double]], val length: Rep[Int])
  {
    import IR.ImplicitLift._

    def size(): Rep[Int] = length / 8

    def apply (i: Rep[Int]): ComplexElement =
    {
      val i2 = i * 8
      val x = _mm256_loadu_pd(data, i2)
      val y = _mm256_loadu_pd(data, i2 + 4)
      val re = _mm256_shuffle_pd(x, y, 0x0)
      val im = _mm256_shuffle_pd(x, y, 0xF)
      ComplexElement (re, im)
    }


    def update(i: Rep[Int], a: ComplexElement): Rep[Unit] =
    {
      val i2 = i * 8
      val re = _mm256_unpacklo_pd(a.re, a.im)
      val im = _mm256_unpackhi_pd(a.re, a.im)
      _mm256_storeu_pd(data, re, i2)
      _mm256_storeu_pd(data, im, i2 + 4)
    }
  }


  def pointwise_generated(x: Rep[Array[Double]], y: Rep[Array[Double]], z: Rep[Array[Double]], N: Rep[Int])
  {
    val x_arr = new ComplexArray (x, N)
    val y_arr = new ComplexArray (y, N)
    val z_arr = new ComplexArray (reflectMutableSym(z.asInstanceOf[Sym[Array[Double]]]), N)

    loop (x_arr.size(), (i: Rep[Int]) => {
      val e1 = x_arr.apply(i)
      val e2 = y_arr.apply(i)
      val ss = e1.add(e2)
      z_arr.update(i, ss)
    })
  }


  def main(arg: Array[String]): Unit = {
    compile4(pointwise_generated, "pointwise_generated")
  }

}
