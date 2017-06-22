#ifndef C_INTRINSICS_COMPLEXARRAY_H_H
#define C_INTRINSICS_COMPLEXARRAY_H_H

#include "ComplexElement.h"

class ComplexArray
{
public:

    double * data;
    int length;

    ComplexArray(double * data, int len);
    int size ();
    ComplexElement apply (int i) const;
    void update (int i, const ComplexElement &a);
    ~ComplexArray();
};

#endif //C_INTRINSICS_COMPLEXARRAY_H_H
