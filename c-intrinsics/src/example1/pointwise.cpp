#include <immintrin.h>

#include "ComplexElement.h"
#include "ComplexArray.h"

void pointwise_complied(double *x, double *y, double *z, int N)
{
    ComplexArray x_arr(x, N);
    ComplexArray y_arr(y, N);
    ComplexArray z_arr(z, N);

    for (int i = 0; i < x_arr.size(); i += 1) {
        const ComplexElement& e1 = x_arr.apply(i);
        const ComplexElement& e2 = y_arr.apply(i);
        const ComplexElement& e3 = e1.mul(e2);
        z_arr.update(i, e3);
    }
}

void pointwise_generated (double *x, double *y, double *z, int N)
{
    pointwise_complied(x, y, z, N);
}
