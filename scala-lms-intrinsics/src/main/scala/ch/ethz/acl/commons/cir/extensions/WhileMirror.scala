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

import scala.lms.common.{CGenWhile, WhileExp}
import scala.reflect.SourceContext

trait WhileMirror extends WhileExp {

  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case While(a,b) => While(f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }

  override def mirror[A:Typ](d: Def[A], f: Transformer) (implicit pos: SourceContext): Exp[A] = (d match {
    case Reflect(While(a, b), u, es) =>
      if (f.hasContext)
        __whileDo(f.reflectBlock(a), f.reflectBlock(b))
      else
        reflectMirrored(Reflect(While(f(a), f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case While(a, b) =>
      if (f.hasContext)
        __whileDo(f.reflectBlock(a), f.reflectBlock(b))
      else
        While(f(a), f(b))
    case _ => super.mirror(d, f)
  }).asInstanceOf[Exp[A]]

}


trait CGenWhileMirror extends CGenWhile {
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case While(c,b) =>
      stream.println("while (1) {")
      emitBlock(c)
      stream.println("if (!"+quote(getBlockResult(c))+") break;")
      emitBlock(b)
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}


