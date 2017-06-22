package ch.ethz.acl.commons.util

import java.io.PrintStream
import scala.reflect.runtime.{universe => ru}

object DebuggingConfig {
  /**
   * Enable or disable debugging globally
   */
  val debug     = true
  /**
   * verbosity defines the wordines of the output:
   * 0 - required debugging for error reporting
   * 1 - high-level illustration of the execution process
   * 2 - output inside transformations (for example IR rewrites, etc)
   * 3 - output everything, including informative and auxilary computations
   */
  var verbosity = 3
}

object StaticDebugging {

  var stream: PrintStream = System.out

  def setStream (s: PrintStream) = stream = s
  def getStream () = stream

  private def getTime() : String = {
    val today = java.util.Calendar.getInstance.getTime
    new java.text.SimpleDateFormat("HH:mm:ss").format(today)
  }

  private def getClassName (c: Any) : String = {
    try {
      val mirror = ru.runtimeMirror(c.getClass.getClassLoader)
      val instance = mirror.reflect(c)
      val baseClasses = instance.symbol.toType.erasure.baseClasses
      val classType   = baseClasses.dropWhile(_.toString.contains("anonymous class"))
      classType(0).toString
    } catch {
      case _: Throwable => c.getClass.getSimpleName
    }
  }

  def printDebugGeneric[T] (v: Int, s: T, c: Any, debug: Boolean): Unit = if (debug) {
    if (v <= DebuggingConfig.verbosity)
      getStream().println("[" + v + "][" + getTime() + " " + getClassName(c) + "] " + s.toString)
  }
}

trait Debugging {

  protected var debug: Boolean = DebuggingConfig.debug

  private def printDebug[T] (v: Int, s: T) = StaticDebugging.printDebugGeneric(v, s, this, debug)

  protected def printDebug0[T] (s: T) = printDebug(0, s)
  protected def printDebug1[T] (s: T) = printDebug(1, s)
  protected def printDebug2[T] (s: T) = printDebug(2, s)
  protected def printDebug3[T] (s: T) = printDebug(3, s)


  def setDebugging     (d: Boolean) = debug = d
  def enableDebugging  () = debug = true
  def disableDebugging () = debug = false
}
