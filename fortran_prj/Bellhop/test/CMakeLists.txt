project(test_bellhop)

# set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/build/out)

file(GLOB srcs ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)

add_executable(${PROJECT_NAME} bellhop.f90)

set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/build/out
)

target_link_libraries(${PROJECT_NAME} bellhopLib)