#ifndef INTELPCM_PERF_H
#define INTELPCM_PERF_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _MSC_VER
    typedef          __int32  int32_t;
    typedef unsigned __int32 uint32_t;
    typedef          __int64  int64_t;
    typedef unsigned __int64 uint64_t;
#else
    #include <stdint.h>
#endif

void     perf_init ();
void     perf_done ();
void     cycles_count_start ();
uint64_t cycles_count_stop  ();


#ifdef __cplusplus
}
#endif


#endif //INTELPCM_PERF_H_H
