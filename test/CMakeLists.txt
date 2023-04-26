project(testCbellhop)
enable_language(C)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/inc)
file(GLOB srcs ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

add_executable(${PROJECT_NAME} ${srcs})

set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out
)

target_link_libraries(${PROJECT_NAME} Cbellhop)