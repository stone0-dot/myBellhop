cmake_minimum_required(VERSION 3.26)
# set(CMAKE_GNUtoMS ON)
project(testproject)
set(version 1.1)
# set(CMAKE_BUILD_TYPE Debug)

add_subdirectory(${CMAKE_SOURCE_DIR}/Cbellhop)
add_subdirectory(${CMAKE_SOURCE_DIR}/fortran_prj)
# add_subdirectory(${CMAKE_SOURCE_DIR}/test)

install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/Cbellhop/inc/Cbellhop.h
        DESTINATION include/Cbellhop-${version}
)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/Cbellhop/inc/Vector.h
        DESTINATION include/Cbellhop-${version}
)
install(TARGETS Cbellhop bellhopLib Cbellhoplib
        DESTINATION lib/Cbellhop-${version}
        EXPORT Cbellhop-targets
)
install(EXPORT Cbellhop-targets
        DESTINATION lib/Cbellhop-${version}
)

configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/pkg/Cbellhop-config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/pkg/Cbellhop-config.cmake @ONLY
)
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/Cbellhop-config-version.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/Cbellhop-config-version.cmake @ONLY
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/pkg/Cbellhop-config.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/Cbellhop-config-version.cmake
        DESTINATION lib/Cbellhop-${version}
)