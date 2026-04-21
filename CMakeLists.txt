cmake_minimum_required(VERSION 3.15.0 FATAL_ERROR)

project(bn-v850-arch CXX)

# Allow standalone builds by pulling in binaryninja-api (defines both the
# `binaryninjaapi` target and the `bn_install_plugin` helper function).
if(NOT TARGET binaryninjaapi)
    if(NOT DEFINED BN_API_PATH)
        set(BN_API_PATH "$ENV{HOME}/src/binaryninja-api"
            CACHE PATH "Path to a binaryninja-api checkout")
    endif()
    if(NOT EXISTS "${BN_API_PATH}/CMakeLists.txt")
        message(FATAL_ERROR "binaryninja-api not found at BN_API_PATH=${BN_API_PATH}")
    endif()
    add_subdirectory("${BN_API_PATH}" "${CMAKE_BINARY_DIR}/binaryninja-api"
                     EXCLUDE_FROM_ALL)
endif()

add_library(${PROJECT_NAME} SHARED
        src/architecture.cpp
        src/architecture.h
        src/conditions.h
        src/flags.h
        src/info.cpp
        src/instructions.cpp
        src/instructions.h
        src/lift.cpp
        src/opcodes.cpp
        src/opcodes.h
        src/registers.h
        src/text.cpp
        src/util.cpp
        src/util.h
        src/sizes.h)

target_link_libraries(${PROJECT_NAME}
        binaryninjaapi)

set_target_properties(${PROJECT_NAME} PROPERTIES
        CXX_STANDARD 20
        LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out/bin)

set(CMAKE_CXX_FLAGS_DEBUG "-Og -g")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

bn_install_plugin(${PROJECT_NAME})

if(BN_V850_BUILD_HARNESS)
    # Use a physical (non-symlinked) path so relative link-time fixup stays
    # anchored to the real filesystem layout. When bn-v850-arch is built via
    # add_subdirectory from a symlinked plugin slot inside binaryninja-api,
    # CMake walks '..' through the symlink and breaks libfmt.a / libbinary-
    # ninjaapi.a paths otherwise.
    get_filename_component(V850_REAL_ROOT "${CMAKE_CURRENT_SOURCE_DIR}" REALPATH)
    add_subdirectory("${V850_REAL_ROOT}/test/rosetta"
                     "${CMAKE_CURRENT_BINARY_DIR}/test/rosetta")
endif()
