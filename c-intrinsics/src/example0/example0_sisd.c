#include "example0.h"

void pointwise_add_sisd(double * x, double * y, double * z, int N) {
    for(int i = 0; i < N; ++i) {
        double x1, y1, z1;
        x1 = x[i];
        y1 = y[i];
        z1 = x1 + y1;
        z[i] = z1;
    }
}
