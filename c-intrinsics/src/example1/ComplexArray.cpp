#include "ComplexArray.h"

ComplexArray::ComplexArray(double * arr, int size) {
    data = arr;
    length = size;
}

int ComplexArray::size ()  {
    return length / 8;
}

ComplexElement ComplexArray::apply (int i) const
{
    int i2 = i * 8;
    const __m256d x = _mm256_loadu_pd(data + i2);
    const __m256d y = _mm256_loadu_pd(data + i2 + 4);
    const __m256d re = _mm256_shuffle_pd(x, y, 0x0);
    const __m256d im = _mm256_shuffle_pd(x, y, 0xF);
    return ComplexElement (re, im);
}

void ComplexArray::update (int i, const ComplexElement &a)
{
    int i2 = i * 8;
    const __m256d re = _mm256_unpacklo_pd(a.re, a.im);
    const __m256d im = _mm256_unpackhi_pd(a.re, a.im);
    _mm256_storeu_pd(data + i2, re);
    _mm256_storeu_pd(data + i2 + 4, im);
}

ComplexArray::~ComplexArray() {

}