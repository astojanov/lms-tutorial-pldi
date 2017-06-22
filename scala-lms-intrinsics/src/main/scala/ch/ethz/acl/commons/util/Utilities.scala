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

package ch.ethz.acl.commons.util

import java.io._

import com.github.abrarsyed.jastyle.ASFormatter
import com.github.abrarsyed.jastyle.constants.SourceMode


object Utilities {

  def findExec(executableName: String): File = {
    val systemPath = System.getenv("PATH");
    val pathDirs = systemPath.split(File.pathSeparator).reverse
    var fullyQualifiedExecutable:File = null
    pathDirs.foreach ( pathDir => {
      val file = new File(pathDir, executableName)
      if (file.isFile()) {
        fullyQualifiedExecutable = file
      }
    })
    fullyQualifiedExecutable
  }

  def indent (code: String): String = {
    val in  = new StringReader(code)
    val out = new StringWriter()
    val formatter = new ASFormatter ()
    formatter.setSourceStyle(SourceMode.C)
    formatter.setPreprocessorIndent(true)
    formatter.format(in, out)
    out.flush()
    out.toString
  }

  def getTempFile (fileName: String = "staged") : java.io.File = {
    java.io.File.createTempFile(fileName, ".c")
  }

  def dumpCode(code: String, name: String): java.io.File = {
    val file = getTempFile(name)
    val stream = new java.io.PrintWriter(file)
    stream.println(code); stream.close(); file
  }

  def manifestSimple[T](tp: Manifest[T]): String = {
    val name = tp match {
      case _ if tp <:< manifest[Double]  => "Double"
      case _ if tp <:< manifest[Float]   => "Float"
      case _ if tp <:< manifest[Char]    => "Char"
      case _ if tp <:< manifest[Boolean] => "Boolean"
      case _ if tp <:< manifest[Long]    => "Long"
      case _ if tp <:< manifest[Int]     => "Int"
      case _ if tp <:< manifest[Short]   => "Short"
      case _ if tp <:< manifest[Byte]    => "Byte"
      case _ => tp.erasure.getSimpleName
    }
    tp.typeArguments.isEmpty match {
      case true => name
      case _ => name + "[" + tp.typeArguments.map(t => manifestSimple(t)).mkString(", ") + "]"
    }
  }

  def getTimeStamp() : String = {
    val today = java.util.Calendar.getInstance.getTime
    new java.text.SimpleDateFormat("dd.MM.yyyy HH:mm:ss").format(today)
  }

}
