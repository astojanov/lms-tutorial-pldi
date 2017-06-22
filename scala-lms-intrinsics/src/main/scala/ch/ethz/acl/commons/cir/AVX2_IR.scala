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

import ch.ethz.acl.commons.cir.codegen.{CApplication, CCodegen}
import ch.ethz.acl.commons.cir.extensions.UnsignedBaseExp
import ch.ethz.acl.intrinsics._

trait AVX2_IR extends CIR
  with UnsignedBaseExp
  with IntrinsicsBase with IntrinsicsArrays
  with SSE with SSE2 with SSE3 with SSSE3 with SSE41 with SSE42
  with AVX with AVX2
  with FMA
{ self =>

  val codegen = new CCodegen
    with CGenSSE with CGenSSE2 with CGenSSE3 with CGenSSSE3 with CGenSSE41 with CGenSSE42
    with CGenAVX with CGenAVX2
    with CGenFMA
  {
    val IR: self.type = self

    override def generateApplication [B] (
      syms: List[Sym[Any]], block: Block[B], fName: String
    ): CApplication = {
      val cApp = super.generateApplication(syms, block, fName)
      getIntrinsicsHeaders().foreach { h =>
        cApp.addSystemHeader(h)
      }
      cApp
    }

  }
}
