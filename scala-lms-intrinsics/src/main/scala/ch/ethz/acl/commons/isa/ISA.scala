package ch.ethz.acl.commons.isa

object ISA extends Enumeration {

  type ISA = Value

  val None   = Value
  val MMX    = Value
  val SSE    = Value
  val SSE2   = Value
  val SSE3   = Value
  val SSSE3  = Value
  val SSE41  = Value
  val SSE42  = Value
  val AVX    = Value
  val AVX2   = Value
  val FMA    = Value
  val KNC    = Value
  val AVX512 = Value

  def getInstructionSetVectorSize[T](isa: ISA) (implicit m: Manifest[T]): Int = (isa, m.toString) match {

    case (AVX, "Int")    => 8
    case (AVX, "Float")  => 8
    case (AVX, "Long")   => 4
    case (AVX, "Double") => 4

    case (SSE2, _) | (SSE3, _) | (SSE3, _) | (SSSE3, _) | (SSE41, _) | (SSE42, _) => m.toString() match {
      case "Int"    => 4
      case "Float"  => 4
      case "Long"   => 2
      case "Double" => 2
    }

    case (SSE, "Float") => 4

    case _ => 1
  }

}
