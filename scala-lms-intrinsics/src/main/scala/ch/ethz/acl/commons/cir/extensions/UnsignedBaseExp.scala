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

import ch.ethz.acl.passera.unsigned._

import scala.lms.common.BaseExp

trait UnsignedBaseExp extends BaseExp {
  implicit def anyTyp   : Typ[Any]    = manifestTyp[Any]
  implicit def uByteTyp : Typ[UByte]  = manifestTyp[UByte]
  implicit def uShortTyp: Typ[UShort] = manifestTyp[UShort]
  implicit def uIntTyp  : Typ[UInt]   = manifestTyp[UInt]
  implicit def uLongTyp : Typ[ULong]  = manifestTyp[ULong]
}
