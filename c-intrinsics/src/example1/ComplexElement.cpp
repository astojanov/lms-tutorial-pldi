#include "ComplexElement.h"

ComplexElement::ComplexElement ():
        re(_mm256_setzero_pd()), im(_mm256_setzero_pd())
{

}

ComplexElement::ComplexElement (__m256d const ere, __m256d const eim):
        re(ere), im(eim)
{

}

ComplexElement ComplexElement::add(const ComplexElement& a) const
{
    const __m256d result_re = _mm256_add_pd(re, a.re);
    const __m256d result_im = _mm256_add_pd(im, a.im);
    return ComplexElement (result_re, result_im);
}

ComplexElement ComplexElement::sub(const ComplexElement &a) const
{
    const __m256d result_re = _mm256_sub_pd(re, a.re);
    const __m256d result_im = _mm256_sub_pd(im, a.im);
    return ComplexElement (result_re, result_im);
};

ComplexElement ComplexElement::mul(const ComplexElement &a) const
{
    const __m256d m1 = _mm256_mul_pd(re, a.re);
    const __m256d m2 = _mm256_mul_pd(im, a.im);
    const __m256d m3 = _mm256_mul_pd(re, a.im);
    const __m256d m4 = _mm256_mul_pd(im, a.re);
    const __m256d result_re = _mm256_sub_pd(m1, m2);
    const __m256d result_im = _mm256_add_pd(m3, m4);
    return ComplexElement(result_re, result_im);
}

ComplexElement & ComplexElement::operator= (const ComplexElement & other)
{
    this->re = other.re;
    this->im = other.im;
    return *this;
}

ComplexElement::~ComplexElement ()
{

};

