// Quick-iteration rosetta harness: feeds raw V850 bytes through the plugin's
// Architecture class and prints the disassembly + LLIL for each instruction.
//
// Usage:
//   rosetta_harness <hex-bytes> [addr=0]
//   rosetta_harness @file.json         # read records from extract.py output
//
// Links the plugin's TUs directly (not the .dylib) so edits to src/*.cpp are
// picked up by a single CMake rebuild and we skip the BN license gate that
// blocks Python headless on the Personal edition.

#include <binaryninjaapi.h>

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#include "../../src/architecture.h"
#include "../../src/instructions.h"

namespace BN = BinaryNinja;

static std::vector<uint8_t> hex_to_bytes(const std::string &s) {
  std::vector<uint8_t> out;
  for (size_t i = 0; i + 1 < s.size(); i += 2) {
    out.push_back(static_cast<uint8_t>(std::stoul(s.substr(i, 2), nullptr, 16)));
  }
  return out;
}

static void print_tokens(const std::vector<BN::InstructionTextToken> &toks) {
  for (const auto &t : toks) std::fputs(t.text.c_str(), stdout);
}

int main(int argc, char **argv) {
  if (argc < 2) {
    std::fprintf(stderr,
                 "usage: %s <hex-bytes> [addr]\n"
                 "       %s @records.json\n",
                 argv[0], argv[0]);
    return 2;
  }

  // Single-shot hex mode (no core init needed for disasm text).
  std::string input = argv[1];
  uint64_t base_addr = argc > 2 ? std::strtoull(argv[2], nullptr, 0) : 0;
  auto bytes = hex_to_bytes(input);

  V850::V850E1Architecture arch("v850-rosetta");

  size_t off = 0;
  while (off < bytes.size()) {
    size_t len = 0;
    std::vector<BN::InstructionTextToken> tokens;
    bool ok = arch.GetInstructionText(bytes.data() + off, base_addr + off,
                                      len, tokens);
    std::printf("%08llx  ", (unsigned long long)(base_addr + off));
    for (size_t i = 0; i < len && i < 8; ++i)
      std::printf("%02x ", bytes[off + i]);
    for (size_t i = len; i < 8; ++i) std::printf("   ");
    std::printf(" ");
    if (ok) print_tokens(tokens);
    else    std::fputs("<decode failed>", stdout);
    std::putchar('\n');
    // Skip a halfword on decode failure so coverage sweeps can keep going.
    off += (ok && len > 0) ? len : 2;
  }
  return 0;
}
