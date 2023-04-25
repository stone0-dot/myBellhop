cmake_minimum_required(VERSION 3.26)
project(testproject)

set(CMAKE_BUILD_TYPE Debug)

add_subdirectory(${CMAKE_SOURCE_DIR}/Cbellhop)
add_subdirectory(${CMAKE_SOURCE_DIR}/fortran_prj)