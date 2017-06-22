#
# Set the main file for compilation
#
set(SOURCE_FILES_EXAMPLE0 example0.c)
#
# Compile the SISD library, disable vectorization on purpose,
# and make sure to use the non-vectorizing C flags
#
set(SISD_FILES_EXAMPLE0 example0_sisd.c)
add_library(example0_sisd ${SISD_FILES_EXAMPLE0})
set_target_properties(example0_sisd PROPERTIES COMPILE_FLAGS "${CMAKE_C_FLAGS_SISD}")
#
# Enable auto vectorization, and use the SIMD flags
#
set(SIMD_FILES_EXAMPLE0 example0_simd.c)
add_library(example0_simd ${SIMD_FILES_EXAMPLE0})
set_target_properties(example0_simd PROPERTIES COMPILE_FLAGS "${CMAKE_C_FLAGS_SIMD}")
#
# Create the binary bundle
#
add_executable(example0 ${SOURCE_FILES_EXAMPLE0})
target_link_libraries(example0 perf example0_sisd example0_simd)