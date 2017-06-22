#include <stdio.h>
#include <string.h>
#include <math.h>
#include <immintrin.h>
#include "cpuid.h"
#include "util.h"

#define STRINGIZE(x) #x
#define STRINGIZE_VALUE_OF(x) STRINGIZE(x)


/**
 * The vendor ID of the CPU. For example: GenuineIntel
 *
 * Note that the string must be malloc-ed before return.
 */
char * getVendorID () {

    char * vendorID = (char *) malloc (sizeof(char) * (12 + 1));
    cpuid_t info = cpuid(0);

    memcpy(vendorID + 0, &(info.ebx), sizeof(char) * 4);
    memcpy(vendorID + 4, &(info.edx), sizeof(char) * 4);
    memcpy(vendorID + 8, &(info.ecx), sizeof(char) * 4);

    vendorID[12] = '\0';
    return vendorID;
}

/**
 *  The brand name returns the string of the computer mode.
 *  For example: Intel(R) Core(TM) i7-3720QM CPU @ 2.60GHz
 *
 *  Note that the string must be malloc-ed before return.
 */
char * getBrandName () {

    int32_t i;
    char * brandName = (char *) malloc (sizeof(char) * 48);

    for (i = 0; i <= 2; i += 1) {

        cpuid_t info = cpuid(i + 0x80000002);
        char * str = brandName + i * 16;

        memcpy(str +  0, &(info.eax), sizeof(char) * 4);
        memcpy(str +  4, &(info.ebx), sizeof(char) * 4);
        memcpy(str +  8, &(info.ecx), sizeof(char) * 4);
        memcpy(str + 12, &(info.edx), sizeof(char) * 4);
    }

    return brandName;
}

void print_compiler_and_system_info()
{
    printf("=======================================================================================================\n");
    printf("= Compiler & System info\n");
    printf("=======================================================================================================\n");

    char * cpu_brand = getBrandName ();
    printf("Current CPU                        : %s\n", cpu_brand);
    free(cpu_brand);

#ifdef CMAKE_C_COMPILER_ID
    printf("C Compiler ID                      : %s\n", STRINGIZE_VALUE_OF(CMAKE_C_COMPILER_ID));
#endif

#ifdef CMAKE_C_COMPILER
    printf("C Compiler Path                    : %s\n", STRINGIZE_VALUE_OF(CMAKE_C_COMPILER));
#endif

#ifdef CMAKE_C_COMPILER_VERSION
    printf("C Compiler Version                 : %s\n", STRINGIZE_VALUE_OF(CMAKE_C_COMPILER_VERSION));
#endif

#ifdef CMAKE_C_FLAGS_SISD
    printf("C Compiler Flags (scalar code)     : %s\n", STRINGIZE_VALUE_OF(CMAKE_C_FLAGS_SISD));
#endif

#ifdef CMAKE_C_FLAGS_SIMD
    printf("C Compiler Flags (vectorized code) : %s\n", STRINGIZE_VALUE_OF(CMAKE_C_FLAGS_SIMD));
#endif

}

int get_random(int limit)
{
    double r = (double) rand() / (double) RAND_MAX;
    double t = round(r * limit);
    return (int) t;
}

int cmp_doubles (const void * a, const void * b)
{
    double da = * (double *) a;
    double db = * (double *) b;

   if (da < db) {
       return -1;
   } else if (da > db) {
       return 1;
   } else {
       return 0;
   }
}

void print_size(size_t size, char* buf)
{
    if (size < 1024) {
        sprintf(buf, "%10.2lf  B", (double) size);
    } else {
        double kb = (double) size / 1024.0;
        if (kb < 1024.0) {
            sprintf(buf, "%10.2lf kB", kb);
        } else {
            double mb = kb / 1024.0;
            sprintf(buf, "%10.2lf MB", mb);
        }
    }
}

void print_array_double(double *x, int N)
{
    for (int i = 0; i < N; i += 1) {
        printf("%.2lf ", x[i]);
    }
    printf("\n");
}


void print_m256d (__m256d v) {
    double arr[4];
    _mm256_storeu_pd(arr, v);
    for (int i = 0; i < 4;  i += 1) {
        printf("%.2lf ", arr[i]);
    }
    printf("\n");
}
