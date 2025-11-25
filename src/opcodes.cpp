// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#include "opcodes.h"

#include <cstdint>

namespace V850 {
static uint64_t mask_and_shift_right(const uint64_t opcode, const uint64_t mask,
                                     const uint8_t shift) {
  return (opcode & mask) >> shift;
}

static uint64_t mask_and_shift_left(const uint64_t opcode, const uint64_t mask,
                                    const uint8_t shift) {
  return (opcode & mask) << shift;
}

uint8_t Extract4BitOpcode(const uint16_t opcode) {
  return mask_and_shift_right(opcode, OpcodeFields::OPCODE_4_BITS,
                              OpcodeFields::SHIFT_4BIT_OPCODE);
}

uint8_t Extract5BitOpcode(const uint16_t opcode) {
  return mask_and_shift_right(opcode, OpcodeFields::OPCODE_5_BITS,
                              OpcodeFields::SHIFT_5BIT_OPCODE);
}

uint8_t Extract6BitOpcode(const uint16_t opcode) {
  return mask_and_shift_right(opcode, OpcodeFields::OPCODE_6_BITS,
                              OpcodeFields::SHIFT_6BIT_OPCODE);
}

uint8_t Extract7BitOpcode(const uint16_t opcode) {
  return mask_and_shift_right(opcode, OpcodeFields::OPCODE_7_BITS,
                              OpcodeFields::SHIFT_7BIT_OPCODE);
}

uint8_t ExtractTypeIIIBranchCond(const uint16_t opcode) {
  return opcode & OpcodeFields::MASK_III_COND;
}

uint8_t ExtractTypeIVSubop(const uint16_t opcode) {
  return opcode & OpcodeFields::MASK_IV_SUBOP;
}

int32_t ExtractTypeVDisp22(const uint32_t opcode) {
  return mask_and_shift_left(opcode, OpcodeFields::MASK_V_DISP_H,
                             OpcodeFields::SHIFT_V_DISP_H) |
         opcode >> 16 & OpcodeFields::MASK_V_DISP_L;
}

uint8_t ExtractTypeVIOpcode(const uint16_t opcode) {
  return mask_and_shift_right(opcode, OpcodeFields::MASK_VI_OPCODE,
                              OpcodeFields::SHIFT_VI_OPCODE);
}

uint16_t ExtractTypeVIImm16(const uint32_t opcode) {
  return opcode >> 16 & OpcodeFields::MASK_VI_IMM;
}

uint32_t ExtractTypeVIImm32(const uint64_t opcode) { return opcode >> 16; }

uint8_t ExtractTypeVIIOpcode(const uint16_t opcode) {
  return mask_and_shift_right(opcode, OpcodeFields::MASK_VII_OPCODE,
                              OpcodeFields::SHIFT_VII_OPCODE);
}

uint8_t ExtractTypeVIISubop(const uint16_t opcode) {
  return opcode & OpcodeFields::MASK_VII_SUBOP;
}

uint16_t ExtractTypeVIIDisp16(const uint32_t opcode) {
  return mask_and_shift_right(opcode, OpcodeFields::MASK_VII_DISP0,
                              OpcodeFields::SHIFT_VII_DISP0) |
         opcode >> 16 & OpcodeFields::MASK_VII_DISP;
}

uint16_t ExtractTypeVIIDisp16B(const uint32_t opcode) { return opcode >> 16; }

uint16_t ExtractTypeVIIDisp16H(const uint32_t opcode) {
  return opcode >> 16 & OpcodeFields::MASK_VII_DISP_H;
}

uint16_t ExtractTypeVIIDisp16W(const uint32_t opcode) {
  return opcode >> 16 & OpcodeFields::MASK_VII_DISP_W;
}

uint8_t ExtractTypeVIIISubop(const uint16_t opcode) {
  return mask_and_shift_right(opcode, OpcodeFields::MASK_VIII_SUBOP,
                              OpcodeFields::SHIFT_VIII_SUBOP);
}

uint8_t ExtractTypeVIIIBitnum(const uint16_t opcode) {
  return mask_and_shift_right(opcode, OpcodeFields::MASK_VIII_BITNUM,
                              OpcodeFields::SHIFT_VIII_BITNUM);
}

uint8_t ExtractTypeXICond(const uint32_t opcode) {
  return mask_and_shift_right(opcode >> 16, OpcodeFields::MASK_XI_COND,
                              OpcodeFields::SHIFT_XI_COND);
}

uint8_t ExtractTypeXIIImm5(const uint16_t opcode) {
  return opcode & OpcodeFields::MASK_XII_IMM9_LOW_OR_IMM5;
}

uint16_t ExtractTypeXIIImm9(const uint32_t opcode) {
  return mask_and_shift_left(opcode >> 16, OpcodeFields::MASK_XII_IMM9_HI,
                             OpcodeFields::SHIFT_LEFT_XII_IMM9_HI);
}

uint8_t ExtractTypeXIIIImm5(const uint32_t opcode) {
  return mask_and_shift_right(opcode >> 16, OpcodeFields::MASK_XIII_IMM5,
                              OpcodeFields::SHIFT_XIII_IMM5);
}

uint8_t ExtractReg1OpcodeField(const uint16_t opcode) {
  return static_cast<uint8_t>(opcode & OpcodeFields::MASK_REG1);
}

uint8_t ExtractReg2OpcodeField(const uint16_t opcode) {
  return mask_and_shift_right(opcode, OpcodeFields::MASK_REG2,
                              OpcodeFields::SHIFT_REG2);
}

uint8_t ExtractReg3OpcodeField(const uint32_t opcode) {
  return mask_and_shift_right(opcode >> 16, OpcodeFields::MASK_XI_REG3,
                              OpcodeFields::SHIFT_XI_REG3);
}

uint8_t ExtractDisp7OpcodeField(const uint16_t opcode) {
  return opcode & OpcodeFields::MASK_IV_7BIT_DISP;
}

uint8_t Extract6BitDisp8OpcodeField(const uint16_t opcode) {
  return mask_and_shift_left(opcode, OpcodeFields::MASK_IV_6BIT_DISP,
                             OpcodeFields::SHIFT_IV_DISP);
}

uint8_t Extract7BitDisp8OpcodeField(const uint16_t opcode) {
  return mask_and_shift_left(opcode, OpcodeFields::MASK_IV_7BIT_DISP,
                             OpcodeFields::SHIFT_IV_DISP);
}

uint8_t ExtractImm5OpcodeField(const uint16_t opcode) {
  return opcode & OpcodeFields::MASK_II_IMM5;
}

uint8_t ExtractImm6OpcodeField(const uint16_t opcode) {
  return opcode & OpcodeFields::MASK_II_IMM6;
}

}  // namespace V850