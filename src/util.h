// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#ifndef BINARYNINJA_API_V850_UTIL_H
#define BINARYNINJA_API_V850_UTIL_H

#include <binaryninjaapi.h>

#include <cstdint>

namespace BN = BinaryNinja;

namespace V850 {

const char *RegToStr(uint8_t rid);
const char *SystemRegToStr(uint8_t reg_id);
const char *FlagToStr(uint32_t flag_id);
const char *ConditionToStr(uint8_t condition);
BN::ExprId ConditionToIL(uint8_t condition, BN::LowLevelILFunction &il);
void GenerateTextForRegisterList12(
    uint64_t opcode, std::vector<BN::InstructionTextToken> &result);
void GenerateILToSaveRegisters(uint64_t opcode, BN::LowLevelILFunction &il);
void GenerateILToRestoreRegisters(uint64_t opcode, BN::LowLevelILFunction &il);

}  // namespace V850
#endif  // BINARYNINJA_API_V850_UTIL_H
