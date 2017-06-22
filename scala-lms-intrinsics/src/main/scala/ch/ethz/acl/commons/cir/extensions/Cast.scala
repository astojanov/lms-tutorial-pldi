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

import ch.ethz.acl.commons.util.NumericExtensions._
import ch.ethz.acl.commons.util.TheTyp

import scala.lms.common.{Base, BaseExp}
import scala.lms.internal.GenericCodegen
import scala.reflect.SourceContext

trait Cast extends Base {
  def infix_cast[T:Typ](s: Rep[Any]): Rep[T]
}

trait CastExp extends Cast with BaseExp {

  case class Cast[T:Typ](s: Exp[Any]) extends Def[T] {
    val m = manifest[T]
  }


  def infix_cast[T:Typ](s: Exp[Any]): Exp[T] = {
    Cast[T](s)
  }


  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Cast(s) => infix_cast(f(s))(e.m)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
}

trait CastExpOpt extends CastExp { self =>

  override def infix_cast[T](s: Exp[Any])(implicit m: Typ[T]): Exp[T] = s match {
    case _ if m <:< s.tp && s.tp <:< m => s.asInstanceOf[Exp[T]]
    case Const(v) => {
      val ms = TheTyp.toManifest(self)(s.tp)
      val mt = TheTyp.toManifest(self)(m)
      (manifestToNumeric(ms), manifestToNumeric(mt)) match {
        case (Some(n), Some(_)) => Const(convert(v)(n, mt))
        case _ => super.infix_cast[T](s)
      }
    }
    case _ => super.infix_cast[T](s)
  }
}

trait GenCast extends GenericCodegen {

  val IR: CastExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Cast(s) => emitValDef(sym, "(" + remap(sym.tp) + ") " + quote(s))
    case _ => super.emitNode(sym, rhs)
  }
}
