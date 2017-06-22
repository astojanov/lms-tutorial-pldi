set(SOURCE_FILES_EXAMPLE1
    example1.cpp
    pointwise.cpp
    ComplexArray.h
    ComplexElement.cpp
    ComplexElement.h
        ComplexArray.cpp)

add_executable(example1 ${SOURCE_FILES_EXAMPLE1})
target_link_libraries(example1 perf)
set_target_properties(example1 PROPERTIES COMPILE_FLAGS "${CMAKE_C_FLAGS_SIMD}")