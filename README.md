# V850 Architecture Plugin for Binary Ninja
A Binary Ninja architecture plugin providing support for the V850 family of instructions.

## Overview
This architecture plugin provides support for the disassembling and lifting of V850 instructions.

### Features
- Full disassembly support for the V850/V850E1 instruction sets
- Partial lifting to Binary Ninja's low level IL

## Installation
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
echo -e "\nadd_subdirectory(v850)" >> plugins/CMakeLists.txt

# Download V850 architecture source
cd plugins
git clone https://github.com/idaholab/bn-v850-arch.git
cd ..

# Build
cmake -DCMAKE_BUILD_TYPE=release -DHEADLESS=yes .
cmake --build . --target all -j

# Install
cp out/bin/libv850.so ~/.binaryninja/plugins/libv850.so
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

## License
Licensed under MIT.

See [LICENSE](LICENSE) file for details.

## Credits
Please see the [NOTICE](NOTICE.txt) file for details.

## Support
If you encounter issues with this repository, please create an [issue](https://github.com/idaholab/bn-st10-arch/issues).
