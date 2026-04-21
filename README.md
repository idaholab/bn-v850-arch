# V850 Architecture Plugin for Binary Ninja
A Binary Ninja architecture plugin providing support for the V850 family of instructions.

## Overview
This architecture plugin provides support for the disassembling and lifting of V850 instructions.

### Features
- Disassembly support for V850, V850E1, V850E2, and V850E3/RH850 G3MH instruction sets
- Near-complete lifting to Binary Ninja's Low Level IL, including FPU, saturated arithmetic, synchronization barriers, 64-bit load/store, and banked system registers
- Correct CC-RH ABI calling convention (argument, caller-saved, and callee-saved registers)
- ELF loader support for EM_V800 binaries

## Installation
### Binary Download
The easiest way to use this plugin is to download and install a pre-compiled release binary.
1. Dowload the shared library corresponding to your OS/arch from the latest [GitHub Release](https://github.com/idaholab/bn-v850-arch/releases)
2. Copy the downloaded binary to your Binary Ninja plugin directory (e.g. `~/.binaryninja/plugins/`)
3. Start Binary Ninja

### Manual Build and Installation
#### Prerequisites
- Binary Ninja (minimum version: `5.1.8104`)
- CMake 3.15 or higher
- C++20 compatible compiler

#### Building from Source & Install
```bash
# Clone the binja API
git clone https://github.com/Vector35/binaryninja-api.git
cd binaryninja-api

# Set up CMake files
echo -e "\nadd_subdirectory(plugins)" >> CMakeLists.txt
echo -e "\nadd_subdirectory(bn-v850-arch)" >> plugins/CMakeLists.txt

# Download V850 architecture source
cd plugins
git clone https://github.com/idaholab/bn-v850-arch.git
cd ..

# Build
cmake -DCMAKE_BUILD_TYPE=release -DHEADLESS=yes -DBN_ALLOW_STUBS=ON -B build .
cmake --build build -j --target bn-v850-arch

# Install
cp build/out/bin/libbn-v850-arch.so ~/.binaryninja/plugins/
```

## Usage
### Opening Files
1. Open Binary Ninja and select "Open with Options..."
2. Under "Load Options", set your entry point offset and image base
3. Choose "V850" from the "Platform" dropdown menu
4. Open the binary file and wait for auto-analysis to complete

> Note that you may need to manually define functions or customize your binary view for the file to load properly

## Development
### Building for Development
To build with debug symbols, follow the instructions above to build from source but change the build type to debug:
```bash
cmake -DCMAKE_BUILD_TYPE=debug -DHEADLESS=yes .
```

### Standalone Build
The plugin can be built without modifying the binaryninja-api tree by pointing
`BN_API_PATH` at an existing checkout:

```bash
cmake -DBN_API_PATH=/path/to/binaryninja-api \
      -DCMAKE_BUILD_TYPE=release -DHEADLESS=yes -DBN_ALLOW_STUBS=ON \
      -B build .
cmake --build build -j --target bn-v850-arch
```

### Rosetta Test Harness
A side-by-side disassembly comparison harness lives under `test/rosetta/`. It
compiles C fixtures with the Renesas CC-RH compiler (requires a CC-RH Docker
image), extracts instruction bytes from the resulting object, and diffs the
plugin's disassembly output against the compiler's own assembly listing.

To build the harness binary alongside the plugin:
```bash
cmake -DBN_API_PATH=/path/to/binaryninja-api \
      -DBN_V850_BUILD_HARNESS=ON \
      -DCMAKE_BUILD_TYPE=release -DHEADLESS=yes -DBN_ALLOW_STUBS=ON \
      -B build .
cmake --build build -j
```

To run against a fixture:
```bash
test/rosetta/run.sh test/rosetta/fixtures/smoke.c
```

### Contributing
Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a [Pull Request](https://github.com/idaholab/bn-v850-arch/pulls)

## Limitations
While every effort was made to ensure the accuracy of this plugin, you may still encounter bugs such as:
- Improperly disassembled instructions
- Missing instructions
- Typos

If you encounter a bug, please consider opening an issue or pull request!

Additionally, not all instructions have been lifted to LLIL. We welcome pull requests to help complete this task!

## Resources
- [Binary Ninja API Documentation](https://api.binary.ninja/cpp/index.html)
- [V850E1 Architecture Instruction Set Reference](https://www.renesas.com/en/document/mah/v850-familytm-architecture)
- [RH850G3MH User's Manual: Software (R01US0143EJ0130)](https://www.renesas.com/en/document/man/rh850g3mh-users-manual-software-0)

## License
Licensed under MIT.

See [LICENSE](LICENSE) file for details.

## Credits
Please see the [NOTICE](NOTICE.txt) file for details.

## Contributors
- **[ghostdev137](https://github.com/ghostdev137)** — V850E3/RH850 G3MH lifter uplift: comprehensive overhaul bringing instruction coverage from ~8% to near-complete; SLEIGH parity pass porting ~43 opcodes from Ghidra's V850 SLEIGH spec; bug fixes for SAT flag stickiness, SCH/FPU intrinsics, MUL-imm, divide remainder ordering, and calling convention (CC-RH ABI); LD.DW/ST.DW 64-bit load/store lifting; rosetta test harness.

## Support
If you encounter issues with this repository, please create an [issue](https://github.com/idaholab/bn-st10-arch/issues).
