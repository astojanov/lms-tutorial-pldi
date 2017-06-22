#ifndef UTIL_H
#define UTIL_H

#include <immintrin.h>
#include "perf.h"

#ifdef __cplusplus
extern "C" {
#endif

void  print_compiler_and_system_info();
int   cmp_doubles (const void * a, const void * b);
int   get_random(int limit);
void  print_size(size_t size, char* buf);
void  print_array_double(double *x, int N);
void  print_m256d (__m256d v);

#ifdef __cplusplus
}
#endif


#endif /* UTIL_H */