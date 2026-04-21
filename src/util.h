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
/* Banked (RH850 G3MH) system register name. Returns a thread-local static
 * string buffer; callers must copy before the next call. Falls back to the
 * RH850 assembler-style "sr<regID>_<selID>" spelling for banked registers
 * not explicitly documented in the G3MH software manual. */
const char *SystemRegToStrBanked(uint8_t regID, uint8_t selID);
/* Decode a sysreg register handle (>= SYSTEM_REG_BASE) into its printed
 * name. Used by Architecture::GetRegisterName so BN's decompiler can
 * resolve banked register references without emitting INVALID_REG_ID. */
const char *SysregHandleToStr(uint32_t handle);
const char *FlagToStr(uint32_t flag_id);
const char *ConditionToStr(uint8_t condition);
BN::ExprId ConditionToIL(uint8_t condition, BN::LowLevelILFunction &il);
void GenerateTextForRegisterList12(
    uint64_t opcode, std::vector<BN::InstructionTextToken> &result);
void GenerateILToSaveRegisters(uint64_t opcode, BN::LowLevelILFunction &il);
void GenerateILToRestoreRegisters(uint64_t opcode, BN::LowLevelILFunction &il);

}  // namespace V850
#endif  // BINARYNINJA_API_V850_UTIL_H
