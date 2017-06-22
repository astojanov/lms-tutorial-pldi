#include <stdio.h>
#include <util.h>
#include <math.h>
#include <stdlib.h>
#include "example1.h"

#define MEASURE_THRESHOLD 1e7
#define MEASURE_REPETITIONS 15

typedef void (*pointwise_add)(double *x, double* y, double *z, int N);

double measure(pointwise_add f, double *x, double *y, double *z, int N)
{
    int runs = 1;
    int multiplier = 1;
    uint64_t cycles;

    double results[MEASURE_REPETITIONS + 1];

    do {
        runs = runs * multiplier;
        cycles_count_start();
        for (int i = 0; i < runs; i += 1) {
            f(x, y, z, N);
        }
        cycles = cycles_count_stop();
        multiplier = (int) ceil (  (MEASURE_THRESHOLD * 100.0) / (cycles * 1.0 * runs)  + 1.0 );
    } while (multiplier > 2);


    for (int i = 0; i < MEASURE_REPETITIONS; i += 1) {
        cycles_count_start();
        for (int j = 0; j < runs; ++j) {
            f(x, y, z, N);
        }
        cycles = cycles_count_stop();
        results[i] = (double) cycles / (double) runs;
    }

    qsort(results, MEASURE_REPETITIONS, sizeof(double), cmp_doubles);
    return results[MEASURE_REPETITIONS / 2];
}


int main()
{
    char str_size [1024];

    print_compiler_and_system_info();
    printf("\n");

    printf("=======================================================================================================\n");
    printf("= example1: Multiplication of complex numbers\n");
    printf("=======================================================================================================\n");
    printf("\n");


    printf("     N    |      size     |     LMS Generated      |            C++         |  Speedup  \n");
    printf("----------------------------------------------------------------------------------------\n");

    for (int N = 8; N < 262144; N *= 2)
    {
        double * x  = (double *) malloc(sizeof(double) * N);
        double * y  = (double *) malloc(sizeof(double) * N);
        double * z  = (double *) malloc(sizeof(double) * N);
        double * zv = (double *) malloc(sizeof(double) * N);

        for (int i = 0; i < N; i += 1) {
            x[i]  = get_random(10);
            y[i]  = get_random(10);
            z[i]  = 0;
            zv[i] = 0;
        }
        // perform validation
        pointwise_complied (x, y,  z, N);
        pointwise_generated(x, y, zv, N);

        for (int i = 0; i < N; i += 1)
        {
            if (z[i] != zv[i])
            {
                printf("x  : "); print_array_double(x , N); printf("\n");
                printf("y  : "); print_array_double(y , N); printf("\n");
                printf("z  : "); print_array_double(z , N); printf("\n");
                printf("zv : "); print_array_double(zv, N); printf("\n");
                printf("\n");
                printf("Validation fails for i=%d (z[i] = %lf && zv[i] = %lf )\n", i, z[i], zv[i]);
                printf("Execting ...\n");
                exit(1);
            }
        }

        double compiled_cycles  = measure(pointwise_complied , x, y, z, N);
        double generated_cycles = measure(pointwise_generated, x, y, z, N);

        print_size(3 * N * sizeof(double), str_size);

        printf("  %7d | %s | %15.2lf cycles | %15.2lf cycles | %10.4lf\n",
               N, str_size, generated_cycles, compiled_cycles, compiled_cycles / generated_cycles);

        free(x);
        free(y);
        free(z);
    }
    printf("\n");
    return 0;
}