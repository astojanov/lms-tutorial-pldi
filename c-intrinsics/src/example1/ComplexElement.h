#ifndef C_INTRINSICS_COMPLEXELEMENT_H
#define C_INTRINSICS_COMPLEXELEMENT_H

#include <immintrin.h>

class ComplexElement
{
public:
    __m256d re;
    __m256d im;

    ComplexElement ();
    ComplexElement (__m256d const ere, __m256d const eim);
    ComplexElement add(const ComplexElement& a) const;
    ComplexElement sub(const ComplexElement &a) const;
    ComplexElement mul(const ComplexElement &a) const;
    ComplexElement & operator= (const ComplexElement & other);
    ~ComplexElement ();
};


#endif //C_INTRINSICS_COMPLEXELEMENT_H
