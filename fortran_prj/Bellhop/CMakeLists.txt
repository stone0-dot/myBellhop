project(bellhopLib)

set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/out)
# set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/fortran_prj/bellhop/archive)
set (CMAKE_VERBOSE_MAKEFILE ON)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/test)

file(GLOB srcs ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)

add_library(${PROJECT_NAME}
    SHARED
        ${srcs}
)