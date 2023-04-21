project(test_bellhop)
enable_language(Fortran)
# set (CMAKE_VERBOSE_MAKEFILE ON)
# file(GLOB srcs ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
add_executable(${PROJECT_NAME} bellhop.f90)

set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out
)

set(CMAKE_EXE_LINKER_FLAGS "-Wl,--allow-multiple-definition")

target_link_libraries(${PROJECT_NAME} bellhopLib)