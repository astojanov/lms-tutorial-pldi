#include <stdio.h>
#include <util.h>
#include <math.h>
#include <stdlib.h>
#include "example0.h"

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
    printf("= example0\n");
    printf("=======================================================================================================\n");
    printf("\n");

    printf("     N    |      size     |             SIMD             |             SISD            \n");
    printf("---------------------------------------------------------------------------------------\n");
    for (int N = 32; N < 262144; N *= 2) {

        double * x = (double *) malloc(sizeof(double) * N);
        double * y = (double *) malloc(sizeof(double) * N);
        double * z = (double *) malloc(sizeof(double) * N);

        for (int i = 0; i < N; i += 1) {
            x[i] = get_random(10);
            y[i] = get_random(10);
        }

        double sisd_cycles = measure(pointwise_add_sisd, x, y, z, N);
        double simd_cycles = measure(pointwise_add_simd, x, y, z, N);

        double perf_sisd = (double) N / sisd_cycles;
        double perf_simd = (double) N / simd_cycles;

        print_size(3 * N * sizeof(double), str_size);

        printf("  %7d | %s | %3.2lf F/C %12.2lf cycles | %3.2lf F/C %12.2lf cycles \n",
                       N, str_size, perf_simd, simd_cycles, perf_sisd, sisd_cycles);

        free(x);
        free(y);
        free(z);
    }
    printf("\n");
    return 0;
}