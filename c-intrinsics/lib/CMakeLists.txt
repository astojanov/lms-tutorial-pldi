set(PERF_SOURCES
        ../include/cpuid.h
        cpuid.c
        ../include/perf.h
        perf.c
        ../include/util.h
        util.c
)
include_directories(../include/)
add_library(perf ${PERF_SOURCES})
set_target_properties(perf PROPERTIES COMPILE_FLAGS "${CMAKE_C_FLAGS_SIMD}")