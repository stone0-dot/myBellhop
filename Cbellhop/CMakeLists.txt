cmake_minimum_required(VERSION 3.26)
project(Cbellhop)
enable_language(C)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/inc)

file(GLOB srcs ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

add_library(${PROJECT_NAME}
    SHARED
        ${srcs}
)

set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out
)

add_custom_target(
    generate_interface_header
    COMMAND ${CMAKE_COMMAND} -E echo \#pragma once > ${CMAKE_CURRENT_SOURCE_DIR}/inc/interface.h
    COMMAND gfortran -fc-prototypes -fsyntax-only -I${CMAKE_BINARY_DIR}/fortran_prj/module ${CMAKE_CURRENT_SOURCE_DIR}/../fortran_prj/Cbellhoplib/interface.f90 >> ${CMAKE_CURRENT_SOURCE_DIR}/inc/interface.h
    DEPENDS Cbellhoplib
)

add_dependencies(${PROJECT_NAME} generate_interface_header)

target_link_libraries(${PROJECT_NAME} Cbellhoplib)