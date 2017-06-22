cmake_minimum_required(VERSION 3.7)
project(c_intrinsics)
#
# Well, its 2017, we can use a newer standard
#
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 11)
#
# Make sure we output every executable in the bin folder
#
set(EXECUTABLE_OUTPUT_PATH  ${CMAKE_CURRENT_LIST_DIR}/bin)
#
# Make sure that the header files in the include directory are accessible everywhere
#
include_directories(include)
#
# Setup the architecture that the compiler will be used. 'native' would work in many cases
# however, some precompiled binaries of GCC tend to have the wrong setting for the 'native'
# argument, thus specifying the proper arhitecture is quite important, particularly on some
# Linux distributions.
#
set(CPU_ARCH haswell)
#
# Setup the SIMD and SISD flags used for the compilation of the examples, and make sure
# that they meatch each compiler thatmight be invoked.
#
if (CMAKE_C_COMPILER_ID MATCHES "Clang")

    set(CMAKE_C_FLAGS_SIMD "-O3 -march=${CPU_ARCH}")
    set(CMAKE_C_FLAGS_SISD "-O3 -march=${CPU_ARCH} -fno-vectorize -fno-slp-vectorize")

elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")

    set(CMAKE_C_FLAGS_SIMD "-O3 -march=${CPU_ARCH}")
    set(CMAKE_C_FLAGS_SISD "-O3 -march=${CPU_ARCH} -fno-vectorize")

elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "Intel")

    set(CMAKE_C_FLAGS_SIMD "-O3 -xHost")
    set(CMAKE_C_FLAGS_SISD "-O3 -xHost -no-vec")

elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "MSVC")

    set(CMAKE_C_FLAGS_SIMD "-O3 -xHost")
    set(CMAKE_C_FLAGS_SISD "-O3 -xHost -no-vec")

endif()
#
# Copy the C-flags into a macro, so we can visulize them nicely on the screen.
#
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   -DCMAKE_C_FLAGS_SISD=\"${CMAKE_C_FLAGS_SISD}\"")
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   -DCMAKE_C_FLAGS_SIMD=\"${CMAKE_C_FLAGS_SIMD}\"")
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   -DCMAKE_C_COMPILER_ID=\"${CMAKE_C_COMPILER_ID}\"")
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   -DCMAKE_C_COMPILER=\"${CMAKE_C_COMPILER}\"")
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   -DCMAKE_C_COMPILER_VERSION=\"${CMAKE_C_COMPILER_VERSION}\"")

add_subdirectory(lib)
add_subdirectory(src/example0)
add_subdirectory(src/example1)

