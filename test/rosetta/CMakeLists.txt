# Build a standalone rosetta_harness binary that links the plugin's TUs
# directly against libbinaryninjaapi. No BN core / license needed for
# disassembly text — LLIL lift invocations require core and will be gated
# behind a --lift flag once we validate the Personal edition can instantiate
# LowLevelILFunction.
# Resolve symlinks so the target's source paths stay anchored at the plugin
# root even when this directory is reached through a symlink (e.g. when
# bn-v850-arch is a standalone checkout linked into binaryninja-api/plugins).
get_filename_component(V850_PLUGIN_ROOT
    "${CMAKE_CURRENT_SOURCE_DIR}/../.." REALPATH)

add_executable(rosetta_harness
    ${CMAKE_CURRENT_SOURCE_DIR}/harness.cpp
    ${V850_PLUGIN_ROOT}/src/architecture.cpp
    ${V850_PLUGIN_ROOT}/src/info.cpp
    ${V850_PLUGIN_ROOT}/src/instructions.cpp
    ${V850_PLUGIN_ROOT}/src/lift.cpp
    ${V850_PLUGIN_ROOT}/src/opcodes.cpp
    ${V850_PLUGIN_ROOT}/src/text.cpp
    ${V850_PLUGIN_ROOT}/src/util.cpp)

target_link_libraries(rosetta_harness PRIVATE binaryninjaapi)

set_target_properties(rosetta_harness PROPERTIES
    CXX_STANDARD 20
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out/bin)
