project(test_Cbellhoplib)
enable_language(C)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/inc)

file(GLOB srcs ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

add_executable(${PROJECT_NAME} ${srcs})

set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out
)

add_custom_target(
    test_generate_interface_header
    COMMAND ${CMAKE_COMMAND} -E echo \#pragma once > ${CMAKE_CURRENT_SOURCE_DIR}/inc/interface.h
    COMMAND gfortran -fc-prototypes -fsyntax-only -I${CMAKE_BINARY_DIR}/fortran_prj/module ${CMAKE_CURRENT_SOURCE_DIR}/../interface.f90 >> ${CMAKE_CURRENT_SOURCE_DIR}/inc/interface.h
    DEPENDS Cbellhoplib
)

add_dependencies(${PROJECT_NAME} test_generate_interface_header)

target_link_libraries(${PROJECT_NAME} Cbellhoplib)