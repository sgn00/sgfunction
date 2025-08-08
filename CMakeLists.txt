cmake_minimum_required(VERSION 3.20)

project(SimpleFunction VERSION 1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(test_simple_function
        test_simple_function.cpp
)
target_compile_options(test_simple_function PRIVATE
        -fsanitize=address
)
target_link_options(test_simple_function PRIVATE
        -fsanitize=address
)

add_executable(run_benchmark bench.cpp)
target_include_directories(run_benchmark PRIVATE /opt/homebrew/include)
target_link_directories(run_benchmark PRIVATE /opt/homebrew/lib)
target_link_libraries(run_benchmark PRIVATE benchmark pthread)
target_compile_options(run_benchmark PRIVATE -O3)