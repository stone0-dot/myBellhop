project(bellhopLib)
enable_language(Fortran)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/fortran_prj/module)

# set (CMAKE_VERBOSE_MAKEFILE ON)

# add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/test)

file(GLOB srcs ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)

add_library(${PROJECT_NAME}
    SHARED
        ${srcs}
)

set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out
)