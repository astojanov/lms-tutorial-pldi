package tutorial.example2

import ch.ethz.acl.commons.cir.AVX2_IR
import ch.ethz.acl.commons.isa.ISA._
import ch.ethz.acl.commons.util.TheTyp
import ch.ethz.acl.passera.unsigned._

trait SVectors {

  val IR: AVX2_IR
  import IR._

  class OperationNotSupported(m: String) extends RuntimeException(m)

  /* ================================================================================================================ */
  /* = SVector / SVectorVar - The main interface                                                                      */
  /* ================================================================================================================ */

  abstract class SVector[T:Typ] {
    val tp = implicitly[Typ[T]]

    def add(v: SVector[T]): SVector[T]
    def mul(v: SVector[T]): SVector[T]
    def reduce(): Exp[T]

    def toVar(): SVectorVar[T]

    def +(that: SVector[T]) = add(that)
    def *(that: SVector[T]) = mul(that)
  }

  abstract class SVectorVar[T:Typ]() {
    val tp = implicitly[Typ[T]]
    def assign (that: SVector[T])
    def read   (): SVector[T]
  }

  /* ================================================================================================================ */
  /* = SVector / SVectorVar Implementation - AVX                                                                      */
  /* ================================================================================================================ */

  case class SVectorDoubleAVX(a: Exp[__m256d]) extends SVector[Double] {
    override def add(v: SVector[Double]): SVector[Double] = v match {
      case SVectorDoubleAVX(b) => SVectorDoubleAVX(_mm256_add_pd(a, b))
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }
    override def mul(v: SVector[Double]): SVector[Double] = v match {
      case SVectorDoubleAVX(b) => SVectorDoubleAVX(_mm256_mul_pd(a, b))
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }
    override def reduce(): Exp[Double] = { import ImplicitLift._
      val t1 = _mm256_hadd_pd(a, a)
      val t2 = _mm256_extractf128_pd(t1,1)
      val t3 = _mm_add_sd(_mm256_castpd256_pd128(t1),t2)
      _mm_cvtsd_f64(t3)

    }
    override def toVar(): SVectorVar[Double] = {
      SVectorDoubleAVXVar(a)
    }
  }

  case class SVectorDoubleAVXVar(a: Exp[__m256d]) extends SVectorVar[Double] {
    val varExp = var_new(a)
    override def assign (v: SVector[Double]) = v match {
      case SVectorDoubleAVX(b) => var_assign(varExp, b)
    }
    override def read (): SVector[Double] = {
      SVectorDoubleAVX(readVar(varExp))
    }
  }

  case class SVectorFloatAVX (a: Exp[__m256])  extends SVector[Float] {
    override def add(v: SVector[Float]): SVector[Float] = v match {
      case SVectorFloatAVX(b) => SVectorFloatAVX(_mm256_add_ps(a, b))
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }
    override def mul(v: SVector[Float]): SVector[Float] = v match {
      case SVectorFloatAVX(b) => SVectorFloatAVX(_mm256_mul_ps(a, b))
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }
    override def reduce (): Exp[Float] = { import ImplicitLift._
      val t1 = _mm256_hadd_ps(a,a)
      val t2 = _mm256_hadd_ps(t1,t1)
      val t3 = _mm256_extractf128_ps(t2,1)
      val t4 = _mm_add_ss(_mm256_castps256_ps128(t2),t3)
      _mm_cvtss_f32(t4)
    }
    override def toVar(): SVectorVar[Float] = {
      SVectorFloatAVXVar(a)
    }
  }

  case class SVectorFloatAVXVar(a: Exp[__m256]) extends SVectorVar[Float] {
    val varExp = var_new(a)
    override def assign (v: SVector[Float]) = v match {
      case SVectorFloatAVX(b) => var_assign(varExp, b)
    }
    override def read (): SVector[Float] = {
      SVectorFloatAVX(readVar(varExp))
    }
  }

  case class SVectorIntegralAVX[T:Typ](a: Exp[__m256i])  extends SVector[T] {

    def selectb (s1: Exp[__m256i], a1: Exp[__m256i], b1: Exp[__m256i]): Exp[__m256i] = {
      _mm256_blendv_epi8 (b1, a1, s1)
    }

    override def add (v: SVector[T]): SVector[T] = v match {
      case SVectorIntegralAVX(b) => tp match {
        case _ if tp <:< TheTyp.toTyp[IR.type, Long  ](IR) => SVectorIntegralAVX[T](_mm256_add_epi64(a, b))
        case _ if tp <:< TheTyp.toTyp[IR.type, Int   ](IR) => SVectorIntegralAVX[T](_mm256_add_epi32(a, b))
        case _ if tp <:< TheTyp.toTyp[IR.type, Short ](IR) => SVectorIntegralAVX[T](_mm256_add_epi16(a, b))
        case _ if tp <:< TheTyp.toTyp[IR.type, Byte  ](IR) => SVectorIntegralAVX[T](_mm256_add_epi8 (a, b))
      }
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }

    override def mul (v: SVector[T]): SVector[T] = v match {
      case SVectorIntegralAVX(b) => tp match {
        case _ if tp <:< TheTyp.toTyp[IR.type, Long  ](IR) => {
          import ImplicitLift._
          // instruction does not exist. Split into 32-bit multiplies
          val bswap   = _mm256_shuffle_epi32(b,0xB1)           // swap H<->L
          val prodlh  = _mm256_mullo_epi32(a,bswap)            // 32 bit L*H products
          val zero    = _mm256_setzero_si256()                 // 0
          val prodlh2 = _mm256_hadd_epi32(prodlh,zero)         // a0Lb0H+a0Hb0L,a1Lb1H+a1Hb1L,0,0
          val prodlh3 = _mm256_shuffle_epi32(prodlh2,0x73)     // 0, a0Lb0H+a0Hb0L, 0, a1Lb1H+a1Hb1L
          val prodll  = _mm256_mul_epu32(a,b)                  // a0Lb0L,a1Lb1L, 64 bit unsigned products
          val prod    = _mm256_add_epi64(prodll,prodlh3)       // a0Lb0L+(a0Lb0H+a0Hb0L)<<32, a1Lb1L+(a1Lb1H+a1Hb1L)<<32
          SVectorIntegralAVX[T](prod)
        }
        case _ if tp <:< TheTyp.toTyp[IR.type, Int   ](IR) => SVectorIntegralAVX[T](_mm256_mullo_epi32(a, b))
        case _ if tp <:< TheTyp.toTyp[IR.type, Short ](IR) => SVectorIntegralAVX[T](_mm256_mullo_epi16(a, b))
        case _ if tp <:< TheTyp.toTyp[IR.type, Byte  ](IR) => {
          import ImplicitLift._
          val aodd    = _mm256_srli_epi16(a,8)                 // odd numbered elements of a
          val bodd    = _mm256_srli_epi16(b,8)                 // odd numbered elements of b
          val muleven = _mm256_mullo_epi16(a,b)                // product of even numbered elements
          val mulodd0 = _mm256_mullo_epi16(aodd,bodd)          // product of odd  numbered elements
          val mulodd  = _mm256_slli_epi16(mulodd0,8)           // put odd numbered elements back in place
          val mask    = _mm256_set1_epi32(0x00FF00FF)          // mask for even positions
          val product = selectb(mask,muleven,mulodd)           // interleave even and odd
          SVectorIntegralAVX[T](product)
        }
      }
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }

    override def reduce (): Exp[T] = (tp match {
      case _ if tp <:< TheTyp.toTyp[IR.type, Long  ](IR) => { import ImplicitLift._
        val sum1 = _mm256_shuffle_epi32(a,0x0E);                     // high element
        val sum2 = _mm256_add_epi64(a,sum1);                         // sum
        val sum3 = _mm256_extracti128_si256(sum2, 1);                // get high part
        val sum4  = _mm_add_epi64(_mm256_castsi256_si128(sum2),sum3)
        _mm_cvtsi128_si64(sum4);                                     // 64 bit mode
      }
      case _ if tp <:< TheTyp.toTyp[IR.type, Int   ](IR) => { import ImplicitLift._
        val sum1 = _mm256_hadd_epi32(a,a)                            // horizontally add 2x4 elements in 2 steps
        val sum2 = _mm256_hadd_epi32(sum1,sum1)
        val sum3 = _mm256_extracti128_si256(sum2,1)                  // get high part
        val sum4 = _mm_add_epi32(_mm256_castsi256_si128(sum2),sum3)  // add low and high parts
        _mm_cvtsi128_si32(sum4)
      }
      case _ if tp <:< TheTyp.toTyp[IR.type, Short ](IR) => { import ImplicitLift._
        val sum1 = _mm256_hadd_epi16(a,a);                           // horizontally add 2x8 elements in 3 steps
        val sum2 = _mm256_hadd_epi16(sum1,sum1)
        val sum3 = _mm256_hadd_epi16(sum2,sum2)
        val sum4 = _mm256_extracti128_si256(sum3,1)                  // get high part
        val sum5 = _mm_add_epi16(_mm256_castsi256_si128(sum3),sum4)  // add low and high parts
        val sum6 = _mm_cvtsi128_si32(sum5);                          // truncate to 16 bits
        infix_cast[T](sum6)                                           // sign extend to 32 bits
      }
      case _ if tp <:< TheTyp.toTyp[IR.type, Byte  ](IR) => { import ImplicitLift._
        val sum1 = _mm256_sad_epu8(a,_mm256_setzero_si256())
        val sum2 = _mm256_shuffle_epi32(sum1,2)
        val sum3 = _mm256_add_epi16(sum1,sum2)
        val sum4 = _mm256_extracti128_si256(sum3,1)
        val sum5 = _mm_add_epi16(_mm256_castsi256_si128(sum3),sum4)
        val sum6 = _mm_cvtsi128_si32(sum5)
        infix_cast[T](sum6)
      }
    }).asInstanceOf[Exp[T]]

    override def toVar(): SVectorVar[T] = {
      SVectorIntegralAVXVar(a)
    }
  }

  case class SVectorIntegralAVXVar[T:Typ](a: Exp[__m256i]) extends SVectorVar[T] {
    val varExp = var_new(a)
    override def assign (v: SVector[T]) = v match {
      case SVectorIntegralAVX(b) => var_assign(varExp, b)
    }
    def read () = {
      SVectorIntegralAVX[T](readVar(varExp))
    }
  }


  /* ================================================================================================================ */
  /* = SVector / SVectorVar Implementation - SSE                                                                      */
  /* ================================================================================================================ */

  case class SVectorDoubleSSE(a: Exp[__m128d]) extends SVector[Double] {
    def add(v: SVector[Double]): SVector[Double] = v match {
      case SVectorDoubleSSE(b) => SVectorDoubleSSE(_mm_add_pd(a, b))
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }
    def mul(v: SVector[Double]): SVector[Double] = v match {
      case SVectorDoubleSSE(b) => SVectorDoubleSSE(_mm_mul_pd(a, b))
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }
    override def reduce (): Exp[Double] = {
      val t1 = _mm_hadd_pd(a,a)
      _mm_cvtsd_f64(t1)
    }
    override def toVar(): SVectorVar[Double] = {
      SVectorDoubleSSEVar(a)
    }
  }

  case class SVectorDoubleSSEVar(a: Exp[__m128d]) extends SVectorVar[Double] {
    val varExp = var_new(a)
    override def assign (v: SVector[Double]) = v match {
      case SVectorDoubleSSE(b) => var_assign(varExp, b)
    }
    def read (): SVector[Double] = {
      SVectorDoubleSSE(readVar(varExp))
    }
  }

  case class SVectorFloatSSE (a: Exp[__m128])  extends SVector[Float] {
    override def add(v: SVector[Float]): SVector[Float] = v match {
      case SVectorFloatSSE(b) => SVectorFloatSSE(_mm_add_ps(a, b))
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }
    override def mul(v: SVector[Float]): SVector[Float] = v match {
      case SVectorFloatSSE(b) => SVectorFloatSSE(_mm_mul_ps(a, b))
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }
    override def reduce(): Exp[Float] = {
      val t1 = _mm_hadd_ps(a,a)
      val t2 = _mm_hadd_ps(t1,t1)
      _mm_cvtss_f32(t2)

    }
    override def toVar(): SVectorVar[Float] = {
      SVectorFloatSSEVar(a)
    }
  }

  case class SVectorFloatSSEVar(a: Exp[__m128]) extends SVectorVar[Float] {
    val varExp = var_new(a)
    override def assign (v: SVector[Float]) = v match {
      case SVectorFloatSSE(b) => var_assign(varExp, b)
    }
    override def read (): SVector[Float] = {
      SVectorFloatSSE(readVar(varExp))
    }
  }

  case class SVectorIntegralSSE[T:Typ](a: Exp[__m128i])  extends SVector[T] {

    def selectb (s: Exp[__m128i], a: Exp[__m128i], b: Exp[__m128i]) = {
      _mm_blendv_epi8 (b, a, s)
      //_mm_or_si128(_mm_and_si128(s,a), _mm_andnot_si128(s,b))
    }

    override def add(v: SVector[T]): SVector[T] = v match {
      case SVectorIntegralSSE(b) => tp match {
        case _ if tp <:< TheTyp.toTyp[IR.type, Long  ](IR) => SVectorIntegralSSE[T](_mm_add_epi64(a, b))
        case _ if tp <:< TheTyp.toTyp[IR.type, Int   ](IR) => SVectorIntegralSSE[T](_mm_add_epi32(a, b))
        case _ if tp <:< TheTyp.toTyp[IR.type, Short ](IR) => SVectorIntegralSSE[T](_mm_add_epi16(a, b))
        case _ if tp <:< TheTyp.toTyp[IR.type, Byte  ](IR) => SVectorIntegralSSE[T](_mm_add_epi8 (a, b))
      }
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }

    override def mul(v: SVector[T]): SVector[T] = v match {
      case SVectorIntegralSSE(b) => tp match {
        case _ if tp <:< TheTyp.toTyp[IR.type, Long  ](IR) => { import ImplicitLift._
          val bswap   = _mm_shuffle_epi32(b,0xB1);           // b0H,b0L,b1H,b1L (swap H<->L)
          val prodlh  = _mm_mullo_epi32(a,bswap);            // a0Lb0H,a0Hb0L,a1Lb1H,a1Hb1L, 32 bit L*H products
          val zero    = _mm_setzero_si128();                 // 0
          val prodlh2 = _mm_hadd_epi32(prodlh,zero);         // a0Lb0H+a0Hb0L,a1Lb1H+a1Hb1L,0,0
          val prodlh3 = _mm_shuffle_epi32(prodlh2,0x73);     // 0, a0Lb0H+a0Hb0L, 0, a1Lb1H+a1Hb1L
          val prodll  = _mm_mul_epu32(a,b);                  // a0Lb0L,a1Lb1L, 64 bit unsigned products
          val prod    = _mm_add_epi64(prodll,prodlh3);       // a0Lb0L+(a0Lb0H+a0Hb0L)<<32, a1Lb1L+(a1Lb1H+a1Hb1L)<<32
          SVectorIntegralSSE[T](prod)
        }
        case _ if tp <:< TheTyp.toTyp[IR.type, Int   ](IR) => SVectorIntegralSSE[T](_mm_mullo_epi32(a, b))
        case _ if tp <:< TheTyp.toTyp[IR.type, Short ](IR) => SVectorIntegralSSE[T](_mm_mullo_epi16(a, b))
        case _ if tp <:< TheTyp.toTyp[IR.type, Byte  ](IR) => { import ImplicitLift._
          val aodd    = _mm_srli_epi16(a,8)                 // odd numbered elements of a
          val bodd    = _mm_srli_epi16(b,8)                 // odd numbered elements of b
          val muleven = _mm_mullo_epi16(a,b)                // product of even numbered elements
          val mulodd0 = _mm_mullo_epi16(aodd,bodd)         // product of odd  numbered elements
          val mulodd  = _mm_slli_epi16(mulodd0,8)           // put odd numbered elements back in place
          val mask    = _mm_set1_epi32(0x00FF00FF)          // mask for even positions
          val product = selectb(mask,muleven,mulodd)        // interleave even and odd
          SVectorIntegralSSE[T](product)
        }
      }
      case _ => throw new OperationNotSupported(this.toString + " " + v.toString)
    }

    override def reduce (): Exp[T] = (tp match {
      case _ if tp <:< TheTyp.toTyp[IR.type, Long  ](IR) => { import ImplicitLift._
        val sum1  = _mm_shuffle_epi32(a,0x0E)  // high element
        val sum2  = _mm_add_epi64(a,sum1)      // sum
        _mm_cvtsi128_si64(sum2)                // 64 bit mode
      }
      case _ if tp <:< TheTyp.toTyp[IR.type, Int   ](IR) => { import ImplicitLift._
        val sum1  = _mm_hadd_epi32(a,a)        // horizontally add 4 elements in 2 steps
        val sum2  = _mm_hadd_epi32(sum1,sum1)
        _mm_cvtsi128_si32(sum2)                // 32 bit sum
      }
      case _ if tp <:< TheTyp.toTyp[IR.type, Short ](IR) => { import ImplicitLift._
        val sum1  = _mm_hadd_epi16(a,a)         // horizontally add 8 elements in 3 steps
        val sum2  = _mm_hadd_epi16(sum1,sum1)
        val sum3  = _mm_hadd_epi16(sum2,sum2)
        val sum4  = _mm_cvtsi128_si32(sum3)     // 16 bit sum
        infix_cast[T](sum4);                    // sign extend to 32 bits
      }
      case _ if tp <:< TheTyp.toTyp[IR.type, Byte  ](IR) => { import ImplicitLift._
        val sum1 = _mm_sad_epu8(a,_mm_setzero_si128())
        val sum2 = _mm_shuffle_epi32(sum1,2)
        val sum3 = _mm_add_epi16(sum1,sum2)
        val sum4 = _mm_cvtsi128_si32(sum3)
        infix_cast[T](sum4)
      }
    }).asInstanceOf[Exp[T]]


    override def toVar(): SVectorVar[T] = {
      SVectorIntegralSSEVar(a)
    }
  }

  case class SVectorIntegralSSEVar[T:Typ](a: Exp[__m128i]) extends SVectorVar[T] {
    val varExp = var_new(a)
    override def assign (v: SVector[T]) = v match {
      case SVectorIntegralSSE(b) => var_assign(varExp, b)
    }
    def read (): SVector[T] = {
      SVectorIntegralSSE[T](readVar(varExp))
    }
  }

  /* ================================================================================================================ */
  /* = SArray Implementation that can adapt on ISA during runtime                                                     */
  /* ================================================================================================================ */

  class SUtil[T:Numeric:Typ](isa: ISA) {
    lazy val tp  = implicitly[Typ[T]]

    def zero(): SVector[T] = (isa match {
      case SSE | SSE2 | SSE3 | SSSE3 | SSE41 | SSE42 => tp match {
        case _ if tp <:< TheTyp.toTyp[IR.type, Double](IR) => SVectorDoubleSSE(_mm_setzero_pd())
        case _ if tp <:< TheTyp.toTyp[IR.type, Float](IR) => SVectorFloatSSE(_mm_setzero_ps())
        case _ => SVectorIntegralSSE[T](_mm_setzero_si128())
      }
      case AVX | AVX2 => tp match {
        case _ if tp <:< TheTyp.toTyp[IR.type, Double](IR) => SVectorDoubleAVX(_mm256_setzero_pd())
        case _ if tp <:< TheTyp.toTyp[IR.type, Float](IR) => SVectorFloatAVX(_mm256_setzero_ps())
        case _ => SVectorIntegralAVX[T](_mm256_setzero_si256())
      }
    }).asInstanceOf[SVector[T]]
  }


  class SArray[T:Numeric:Typ](isa: ISA, data: Rep[Array[T]], init_len: Exp[Int])(implicit cont: Container[Array])
  {
    lazy val tp  = implicitly[Typ[T]]
    lazy val mul = Const(getISAVectorLength[T](isa))
    lazy val len = init_len / mul

    def size () : Rep[Int] = len
    def scalar_size () : Rep[Int] = len * mul

    def apply(i: Rep[Int]): SVector[T] = {
      val idx = i * mul
      (isa match {
        case SSE | SSE2 | SSE3 | SSSE3 | SSE41 | SSE42 => tp match {
          case _ if tp <:< TheTyp.toTyp[IR.type, Double](IR) =>
            SVectorDoubleSSE(_mm_loadu_pd(data.asInstanceOf[Exp[Array[Double]]], idx))
          case _ if tp <:< TheTyp.toTyp[IR.type, Float](IR)  =>
            SVectorFloatSSE(_mm_loadu_ps(data.asInstanceOf[Exp[Array[Float]]], idx))
          case _ =>
            SVectorIntegralSSE[T](_mm_loadu_si128[Array, Int](data.asInstanceOf[Exp[Array[__m128i]]], idx))
        }
        case AVX | AVX2 => tp match {
          case _ if tp <:< TheTyp.toTyp[IR.type, Double](IR) =>
            SVectorDoubleAVX(_mm256_loadu_pd(data.asInstanceOf[Exp[Array[Double]]], idx))
          case _ if tp <:< TheTyp.toTyp[IR.type, Float](IR)  =>
            SVectorFloatAVX(_mm256_loadu_ps(data.asInstanceOf[Exp[Array[Float]]], idx))
          case _ =>
            SVectorIntegralAVX[T](_mm256_loadu_si256[Array, Int](data.asInstanceOf[Exp[Array[__m256i]]], idx))
        }
      }).asInstanceOf[SVector[T]]
    }


    def update(i: Rep[Int], v: SVector[T]): Unit = {
      val idx = i * mul
      // check type equality, to make sure that those match
      if (!(v.tp <:< tp && tp <:< v.tp)) {
        throw new OperationNotSupported("Update: " + this.toString + " " + v.toString)
      }
      isa match {
        case SSE | SSE2 | SSE3 | SSSE3 | SSE41 | SSE42 => tp match {
          case _ if tp <:< TheTyp.toTyp[IR.type, Double](IR) => {
            val cv = v.asInstanceOf[SVectorDoubleSSE]
            _mm_storeu_pd(data.asInstanceOf[Exp[Array[Double]]], cv.a, idx)
          }
          case _ if tp <:< TheTyp.toTyp[IR.type, Float](IR) => {
            val cv = v.asInstanceOf[SVectorFloatSSE]
            _mm_storeu_ps(data.asInstanceOf[Exp[Array[Float]]], cv.a, idx)
          }
          case _  => {
            val cv = v.asInstanceOf[SVectorIntegralSSE[T]]
            _mm_storeu_si128(data.asInstanceOf[Exp[Array[__m128i]]], cv.a, idx)
          }
        }
        case AVX | AVX2 => v match {
          case _ if tp <:< TheTyp.toTyp[IR.type, Double](IR) => {
            val cv = v.asInstanceOf[SVectorDoubleAVX]
            _mm256_storeu_pd(data.asInstanceOf[Exp[Array[Double]]], cv.a, idx)
          }
          case _ if tp <:< TheTyp.toTyp[IR.type, Float](IR) => {
            val cv = v.asInstanceOf[SVectorFloatAVX]
            _mm256_storeu_ps(data.asInstanceOf[Exp[Array[Float]]], cv.a, idx)
          }
          case _  => {
            val cv = v.asInstanceOf[SVectorIntegralAVX[T]]
            _mm256_storeu_si256(data.asInstanceOf[Exp[Array[__m256i]]], cv.a, idx)
          }
        }
      }
    }
  }


  /* ================================================================================================================ */
  /* = Utility functions used to calculate the length of the SSE / AVX vectors                                        */
  /* ================================================================================================================ */

  def sizeOf[T:Numeric](tp: Typ[T]): Int = tp match {
    case _ if tp <:< TheTyp.toTyp[IR.type, Double](IR) => 64
    case _ if tp <:< TheTyp.toTyp[IR.type, Float ](IR) => 32
    case _ if tp <:< TheTyp.toTyp[IR.type, Long  ](IR) => 64
    case _ if tp <:< TheTyp.toTyp[IR.type, Int   ](IR) => 32
    case _ if tp <:< TheTyp.toTyp[IR.type, Short ](IR) => 16
    case _ if tp <:< TheTyp.toTyp[IR.type, Byte  ](IR) => 8
    case _ if tp <:< TheTyp.toTyp[IR.type, ULong ](IR) => 64
    case _ if tp <:< TheTyp.toTyp[IR.type, UInt  ](IR) => 32
    case _ if tp <:< TheTyp.toTyp[IR.type, UShort](IR) => 16
    case _ if tp <:< TheTyp.toTyp[IR.type, UByte ](IR) => 8
    case _ => 1
  }

  def getISAVectorLength[T:Numeric](isa: ISA)(implicit tp: Typ[T]): Int = isa match {
    case SSE | SSE2 | SSE3 | SSSE3 | SSE41 | SSE42 => 128 / sizeOf(tp)
    case AVX | AVX2 => 256 / sizeOf(tp)
    case _ => 1
  }


}
