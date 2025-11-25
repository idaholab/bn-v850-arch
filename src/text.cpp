// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#include <binaryninjaapi.h>

#include <cstddef>
#include <cstdint>

#include "conditions.h"
#include "instructions.h"
#include "opcodes.h"
#include "registers.h"
#include "sizes.h"
#include "util.h"

#define ITEXT(m)                            \
  result.emplace_back(InstructionToken, m); \
  result.emplace_back(TextToken, " ");

namespace V850 {

bool Text_I_Generic_Reg1_Reg2(const char *mnemonic, const uint16_t opcode,
                              size_t &len,
                              std::vector<BN::InstructionTextToken> &result) {
  /* Extract fields */
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  ITEXT(mnemonic)

  char buf[32];
  std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
  result.emplace_back(RegisterToken, buf, reg1);
  result.emplace_back(OperandSeparatorToken, ", ");

  std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
  result.emplace_back(RegisterToken, buf, reg2);

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_I_MOV_NOP(const uint16_t opcode, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  /* Extract fields*/
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  if (reg1 == Registers::R0 && reg2 == Registers::R0) {
    // No operation
    // Text format: nop
    ITEXT("nop")

  } else {
    // Move register
    // Text format: mov reg1, reg2
    ITEXT("mov")

    char buf[32];
    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);
    result.emplace_back(OperandSeparatorToken, ", ");

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
    result.emplace_back(RegisterToken, buf, reg2);
  }

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_I_SWITCH_DBTRAP_DIVH(const uint16_t opcode, size_t &len,
                               std::vector<BN::InstructionTextToken> &result) {
  /* Extract fields*/
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  char buf[32];

  if (reg2 == Registers::R0) {
    // Switch; jump with table lookup
    // Text format: switch reg1
    ITEXT("switch")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);

  } else if (opcode == Opcodes::EXACT_OP_I_DBTRAP) {
    // Debug trap
    // Text format: dbtrap
    ITEXT("dbtrap")

  } else {
    // Divide halfword
    // Text format: divh reg1, reg2
    ITEXT("divh")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);
    result.emplace_back(OperandSeparatorToken, ", ");

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
    result.emplace_back(RegisterToken, buf, reg2);
  }

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_I_SATSUBR_ZXB(const uint16_t opcode, size_t &len,
                        std::vector<BN::InstructionTextToken> &result) {
  /* Extract fields*/
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  char buf[32];
  if (reg2 == Registers::R0) {
    // Zero extend byte
    // Text format: zxb reg1
    ITEXT("zxb")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);

    len = Sizes::LEN16BIT;
    return true;

  } else {
    // Saturated subtract reverse
    // Text format: satsubr reg1, reg2
    ITEXT("satsubr")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);
    result.emplace_back(OperandSeparatorToken, ", ");

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
    result.emplace_back(RegisterToken, buf, reg2);
  }

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_I_SATSUB_SXB(const uint16_t opcode, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  /* Extract fields*/
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  char buf[32];
  if (reg2 == Registers::R0) {
    // Sign extend byte
    // sxb reg1
    ITEXT("sxb")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);

  } else {
    // Saturated subtract
    // Text format: satsub reg1, reg2
    ITEXT("satsub")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);
    result.emplace_back(OperandSeparatorToken, ", ");

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
    result.emplace_back(RegisterToken, buf, reg2);
  }

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_I_SATADD_ZXH(const uint16_t opcode, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  /* Extract fields*/
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  char buf[32];
  if (reg2 == Registers::R0) {
    // Zero extend halfword
    // Text format: zxh reg1
    ITEXT("zxh")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);

  } else {
    // Saturated add register
    // Text format: satadd reg1, reg2
    ITEXT("satadd")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);
    result.emplace_back(OperandSeparatorToken, ", ");

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
    result.emplace_back(RegisterToken, buf, reg2);
  }

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_I_MULH_SXH(const uint16_t opcode, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  /* Extract fields*/
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  char buf[32];
  if (reg2 == Registers::R0) {
    // Sign extend halfword
    // Text format: sxh reg1
    ITEXT("sxh")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);

    len = Sizes::LEN16BIT;

  } else {
    // Multiply halfword by register
    // Text format: mulh reg1, reg2
    ITEXT("mulh")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);
    result.emplace_back(OperandSeparatorToken, ", ");

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
    result.emplace_back(RegisterToken, buf, reg2);
  }

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_I_JMP_IV_SLDHU_SLDBU(const uint16_t opcode, size_t &len,
                               std::vector<BN::InstructionTextToken> &result) {
  /* Extract fields */
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  char buf[32];
  if (reg2 == Registers::R0) {
    // Jump register
    // Text format: jmp [reg1]
    ITEXT("jmp")

    result.emplace_back(TextToken, "[");
    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);
    result.emplace_back(TextToken, "]");

  } else {
    if (const auto opcode_7bit = Extract7BitOpcode(opcode);
        opcode_7bit == Opcodes::OP_IV_SLD_BU) {
      // Short format load byte unsigned
      // Text format: sld.bu disp4[ep], reg2
      ITEXT("sld.bu")
      auto disp =
          static_cast<uint8_t>(opcode & OpcodeFields::MASK_IV_4BIT_DISP);
      std::snprintf(buf, sizeof(buf), "%#x", disp);
      result.emplace_back(IntegerToken, buf, disp, sizeof(disp));

      result.emplace_back(TextToken, "[");
      std::snprintf(buf, sizeof(buf), "%s", RegToStr(Registers::EP));
      result.emplace_back(RegisterToken, buf, Registers::EP);
      result.emplace_back(TextToken, "]");

      result.emplace_back(OperandSeparatorToken, ", ");
      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
      result.emplace_back(RegisterToken, buf, reg2);

    } else if (opcode_7bit == Opcodes::OP_IV_SLD_HU) {
      // Short format load halfword unsigned
      // Text format: sld.hu disp5[ep], reg2
      ITEXT("sld.hu")

      auto disp =
          static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_4BIT_DISP)
                               << OpcodeFields::SHIFT_IV_DISP);
      std::snprintf(buf, sizeof(buf), "%#x", disp);
      result.emplace_back(IntegerToken, buf, disp, sizeof(disp));

      result.emplace_back(TextToken, "[");
      std::snprintf(buf, sizeof(buf), "%s", RegToStr(Registers::EP));
      result.emplace_back(RegisterToken, buf, Registers::EP);
      result.emplace_back(TextToken, "]");

      result.emplace_back(OperandSeparatorToken, ", ");
      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
      result.emplace_back(RegisterToken, buf, reg2);
    }
  }
  len = Sizes::LEN16BIT;
  return true;
}

bool Text_II_Generic_Imm5_Reg2(const char *mnemonic, const uint16_t opcode,
                               size_t &len,
                               std::vector<BN::InstructionTextToken> &result) {
  /* Extract fields */
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto imm5 = ExtractImm5OpcodeField(opcode);

  ITEXT(mnemonic)

  char buf[32];
  std::snprintf(buf, sizeof(buf), "%#x", imm5);
  result.emplace_back(IntegerToken, buf, imm5, sizeof(imm5));
  result.emplace_back(OperandSeparatorToken, ", ");

  std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
  result.emplace_back(RegisterToken, buf, reg2);

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_II_CALLT_SATADD_MOV(const uint16_t opcode, size_t &len,
                              std::vector<BN::InstructionTextToken> &result) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);

  char buf[32];
  if (reg2 == Registers::R0) {
    // Call with table lookup
    // Text format: callt imm6
    ITEXT("callt")

    const auto imm6 = ExtractImm6OpcodeField(opcode);
    std::snprintf(buf, sizeof(buf), "%#x", imm6);
    result.emplace_back(IntegerToken, buf, imm6, sizeof(imm6));

  } else {
    if (opcode & OpcodeFields::OPCODE_BIT_6) {
      // Saturated add
      // Text format: satadd imm5,
      ITEXT("satadd")

    } else {
      // Move 5-bit immediate
      // Text format: mov imm5, reg2
      ITEXT("mov")
    }

    const auto imm5 = ExtractImm5OpcodeField(opcode);
    std::snprintf(buf, sizeof(buf), "%#x", imm5);
    result.emplace_back(IntegerToken, buf, imm5, sizeof(imm5));
    result.emplace_back(OperandSeparatorToken, ", ");

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
    result.emplace_back(RegisterToken, buf, reg2);
  }

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_III_BCOND(const uint16_t opcode, const uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  int32_t displacement =
      static_cast<int16_t>(((&opcode)[0] & OpcodeFields::MASK_III_DISP_H) >>
                               OpcodeFields::SHIFT_III_DISP_H |
                           ((&opcode)[0] & OpcodeFields::MASK_III_DISP_L) >>
                               OpcodeFields::SHIFT_III_DISP_L);
  // Sign-extend 9-bit displacement
  if (displacement & (1 << 8)) {
    displacement = static_cast<int32_t>(0xFFFFFF00) | displacement;
  }

  uint32_t target = addr + displacement;

  /* Determine branch instruction based on the condition code */
  switch (auto condition = ExtractTypeIIIBranchCond(opcode); condition) {
    // Note: where condition codes have multiple meanings, the most general one
    // is used for the mnemonic e.g., between "bz" (zero) and "be" (equal), "bz"
    // is used because it describes flag status rather than ascribing meaning to
    // the flag status
    case Conditions::CONDITION_CODE_BGT:
      ITEXT("bgt")
      break;
    case Conditions::CONDITION_CODE_BGE:
      ITEXT("bge")
      break;
    case Conditions::CONDITION_CODE_BLT:
      ITEXT("blt")
      break;
    case Conditions::CONDITION_CODE_BLE:
      ITEXT("ble")
      break;
    case Conditions::CONDITION_CODE_BH:
      ITEXT("bh")
      break;
    case Conditions::CONDITION_CODE_BNH:
      ITEXT("bnh")
      break;
    case Conditions::CONDITION_CODE_BV:
      ITEXT("bv")
      break;
    case Conditions::CONDITION_CODE_BNV:
      ITEXT("bnv")
      break;
    case Conditions::CONDITION_CODE_BN:
      ITEXT("bn")
      break;
    case Conditions::CONDITION_CODE_BP:
      ITEXT("bp")
      break;
    case Conditions::CONDITION_CODE_BC:  // Note: same condition code as
                                         // CONDITION_CODE_BL
      ITEXT("bc")
      break;
    case Conditions::CONDITION_CODE_BNC:  // Note: same condition code as
                                          // CONDITION_CODE_BNL
      ITEXT("bnc")
      break;
    case Conditions::CONDITION_CODE_BZ:  // Note: same condition code as
                                         // CONDITION_CODE_BE
      ITEXT("bz")
      break;
    case Conditions::CONDITION_CODE_BNZ:  // Note: same condition code as
                                          // CONDITION_CODE_BNZ
      ITEXT("bnz")
      break;
    case Conditions::CONDITION_CODE_BR:
      ITEXT("br")
      break;
    case Conditions::CONDITION_CODE_BSA:
      ITEXT("bsa")
      break;
    default:
      return false;
  }

  char buf[32];
  std::snprintf(buf, sizeof(buf), "%#x", target);
  result.emplace_back(IntegerToken, buf, target, sizeof(target));

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_IV_SLDB(const uint16_t opcode, size_t &len,
                  std::vector<BN::InstructionTextToken> &result) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto disp7 = ExtractDisp7OpcodeField(opcode);

  // Short format load byte
  // Text format: sld.b disp7[ep], reg2
  ITEXT("sld.b")

  char buf[32];
  std::snprintf(buf, sizeof(buf), "%#x", disp7);
  result.emplace_back(IntegerToken, buf, disp7, sizeof(disp7));
  result.emplace_back(TextToken, "[");
  std::snprintf(buf, sizeof(buf), "%s", RegToStr(Registers::EP));
  result.emplace_back(RegisterToken, buf, Registers::EP);
  result.emplace_back(TextToken, "]");
  result.emplace_back(OperandSeparatorToken, ", ");

  std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
  result.emplace_back(RegisterToken, buf, reg2);

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_IV_SSTB(const uint16_t opcode, size_t &len,
                  std::vector<BN::InstructionTextToken> &result) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto disp7 = ExtractDisp7OpcodeField(opcode);

  // Short format store byte
  // Text format: sst.b reg2, disp7[ep]
  ITEXT("sst.b")

  char buf[32];
  std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
  result.emplace_back(RegisterToken, buf, reg2);
  result.emplace_back(OperandSeparatorToken, ", ");

  std::snprintf(buf, sizeof(buf), "%#x", disp7);
  result.emplace_back(IntegerToken, buf, disp7, sizeof(disp7));
  result.emplace_back(TextToken, "[");
  std::snprintf(buf, sizeof(buf), "%s", RegToStr(Registers::EP));
  result.emplace_back(RegisterToken, buf, Registers::EP);
  result.emplace_back(TextToken, "]");

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_IV_SLDH(const uint16_t opcode, size_t &len,
                  std::vector<BN::InstructionTextToken> &result) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto disp8 = Extract7BitDisp8OpcodeField(opcode);

  // Short format load halfword
  // Text format: sld.h disp8[ep], reg2
  ITEXT("sld.h")

  char buf[32];
  std::snprintf(buf, sizeof(buf), "%#x", disp8);
  result.emplace_back(IntegerToken, buf, disp8, sizeof(disp8));
  result.emplace_back(TextToken, "[");
  std::snprintf(buf, sizeof(buf), "%s", RegToStr(Registers::EP));
  result.emplace_back(RegisterToken, buf, Registers::EP);
  result.emplace_back(TextToken, "]");
  result.emplace_back(OperandSeparatorToken, ", ");

  std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
  result.emplace_back(RegisterToken, buf, reg2);

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_IV_SSTH(const uint16_t opcode, size_t &len,
                  std::vector<BN::InstructionTextToken> &result) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto disp8 = Extract7BitDisp8OpcodeField(opcode);

  // Short format store halfword
  // Text format: sst.b reg2, disp8[ep]
  ITEXT("sst.h")

  char buf[32];
  std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
  result.emplace_back(RegisterToken, buf, reg2);
  result.emplace_back(OperandSeparatorToken, ", ");

  std::snprintf(buf, sizeof(buf), "%#x", disp8);
  result.emplace_back(IntegerToken, buf, disp8, sizeof(disp8));
  result.emplace_back(TextToken, "[");
  std::snprintf(buf, sizeof(buf), "%s", RegToStr(Registers::EP));
  result.emplace_back(RegisterToken, buf, Registers::EP);
  result.emplace_back(TextToken, "]");

  len = Sizes::LEN16BIT;
  return true;
}

bool Text_IV_SLDW_SSTW(const uint16_t opcode, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  const auto subop = ExtractTypeIVSubop(opcode);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto disp8 = Extract6BitDisp8OpcodeField(opcode);

  char buf[32];
  if (subop == Opcodes::SUBOP_IV_SLD_W) {
    // Short format load word
    // Text format: sld.w disp8[ep], reg2
    ITEXT("sld.w")

    std::snprintf(buf, sizeof(buf), "%#x", disp8);
    result.emplace_back(IntegerToken, buf, disp8, sizeof(disp8));
    result.emplace_back(TextToken, "[");
    std::snprintf(buf, sizeof(buf), "%s", RegToStr(Registers::EP));
    result.emplace_back(RegisterToken, buf, Registers::EP);
    result.emplace_back(TextToken, "]");
    result.emplace_back(OperandSeparatorToken, ", ");

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
    result.emplace_back(RegisterToken, buf, reg2);
    len = Sizes::LEN16BIT;
    return true;
  }
  if (subop == Opcodes::SUBOP_IV_SST_W) {
    // Short format store word
    // Text format: sst.w reg2, disp8[ep]
    ITEXT("sst.w")

    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
    result.emplace_back(RegisterToken, buf, reg2);
    result.emplace_back(OperandSeparatorToken, ", ");

    std::snprintf(buf, sizeof(buf), "%#x", disp8);
    result.emplace_back(IntegerToken, buf, disp8, sizeof(disp8));
    result.emplace_back(TextToken, "[");
    std::snprintf(buf, sizeof(buf), "%s", RegToStr(Registers::EP));
    result.emplace_back(RegisterToken, buf, Registers::EP);
    result.emplace_back(TextToken, "]");
    len = Sizes::LEN16BIT;
    return true;
  }
  return false;
}

bool Text_V_JARL_JR_VII_LDBU_XIII_PREPARE(
    const uint64_t opcode, size_t &len,
    std::vector<BN::InstructionTextToken> &result) {
  char buf[32];

  uint8_t subop = opcode >> 16 & OpcodeFields::MASK_V_SUBOP_BIT_16;
  if (subop == Opcodes::SUBOP_V_JARL_JR) {  // jarl, jr

    const auto reg2 = ExtractReg2OpcodeField(opcode);
    const auto disp22 = ExtractTypeVDisp22(opcode);

    if (reg2 == Registers::R0) {  // if reg2 is r0, is jr
      // Jump relative
      // Text format: jr disp22
      ITEXT("jr")

      std::snprintf(buf, sizeof(buf), "%#x", disp22);
      result.emplace_back(IntegerToken, buf, disp22, sizeof(disp22));

    } else {
      // Jump and register link
      // Text format: jarl disp22, reg2
      ITEXT("jarl")

      std::snprintf(buf, sizeof(buf), "%#x", disp22);
      result.emplace_back(IntegerToken, buf, disp22, sizeof(disp22));
      result.emplace_back(OperandSeparatorToken, ", ");

      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
      result.emplace_back(RegisterToken, buf, reg2);

      len = Sizes::LEN32BIT;
      return true;
    }
  } else {
    subop = opcode >> 16 & Opcodes::MASK_XIII_SUBOP_PREPARE;
    if (subop == Opcodes::SUBOP_XIII_PREPARE_001) {
      // Function prepare
      // Text format: prepare list12, imm5
      ITEXT("prepare")

      GenerateTextForRegisterList12(opcode, result);
      result.emplace_back(OperandSeparatorToken, ", ");

      const auto imm5 = ExtractTypeXIIIImm5(opcode);
      std::snprintf(buf, sizeof(buf), "%#x", imm5);
      result.emplace_back(IntegerToken, buf, imm5, sizeof(imm5));

      len = Sizes::LEN32BIT;
      return true;
    }
    if (subop == Opcodes::SUBOP_XIII_PREPARE_011) {
      // Function prepare
      // Text format: prepare list12, imm5, sp/imm
      ITEXT("prepare")

      GenerateTextForRegisterList12(opcode, result);
      result.emplace_back(OperandSeparatorToken, ", ");

      int32_t imm = 0;
      auto imm5 =
          static_cast<uint8_t>((opcode >> 16 & OpcodeFields::MASK_XIII_IMM5) >>
                               OpcodeFields::SHIFT_XIII_IMM5);
      std::snprintf(buf, sizeof(buf), "%#x", imm5);
      result.emplace_back(IntegerToken, buf, imm5, sizeof(imm5));
      result.emplace_back(OperandSeparatorToken, ", ");

      switch (const auto prepare_format = static_cast<uint8_t>(
                  opcode >> 16 & Opcodes::MASK_XIII_PREPARE_FORMAT);
              prepare_format) {
        case Opcodes::PREPARE_LOAD_SP:
          result.emplace_back(RegisterToken, "sp", Registers::SP);

          len = Sizes::LEN32BIT;
          return true;

        case Opcodes::PREPARE_LOAD_SIGN_EXTENDED_IMM16:
          // Sign-extend 16-bit immediate to 32-bit // TODO make functions and
          // use everywhere
          if (imm & (1 << 15)) {  // MSB is set
            imm = static_cast<int32_t>(0xFFFF0000) | opcode >> 32;
          } else {  // MSB is not set
            imm = 0x00000000 | opcode >> 32;
          }
          std::snprintf(buf, sizeof(buf), "%#x", imm);
          result.emplace_back(IntegerToken, buf, imm, sizeof(imm));

          len = Sizes::LEN48BIT;
          return true;

        case Opcodes::PREPARE_LOAD_LSL_IMM16:
          // Logically shift 16-bit immediate left by 16
          imm = static_cast<int32_t>(opcode >> 32 << 16);

          std::snprintf(buf, sizeof(buf), "%#x", imm);
          result.emplace_back(IntegerToken, buf, imm, sizeof(imm));

          len = Sizes::LEN48BIT;
          return true;

        case Opcodes::PREPARE_LOAD_IMM32:
          // 32-bit immediate, formed from bits 32-63 of the instruction
          imm = static_cast<int32_t>(opcode >> 48 << 16 | opcode >> 32);

          std::snprintf(buf, sizeof(buf), "%#x", imm);
          result.emplace_back(IntegerToken, buf, imm, sizeof(imm));

          len = Sizes::LEN64BIT;
          return true;
        default:
          return false;
      }
    } else {  // ld.bu
      // Load byte unsigned
      // Text format: ld.bu disp16[reg1], reg2
      const auto reg2 = ExtractReg2OpcodeField(opcode);
      const auto reg1 = ExtractReg1OpcodeField(opcode);
      uint16_t disp16 = ExtractTypeVIIDisp16(opcode);

      ITEXT("ld.bu")

      std::snprintf(buf, sizeof(buf), "%#x", disp16);
      result.emplace_back(IntegerToken, buf, disp16, sizeof(disp16));
      result.emplace_back(TextToken, "[");
      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
      result.emplace_back(RegisterToken, buf, reg1);
      result.emplace_back(TextToken, "]");
      result.emplace_back(OperandSeparatorToken, ", ");

      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
      result.emplace_back(RegisterToken, buf, reg2);
    }
  }
  len = Sizes::LEN32BIT;
  return true;
}

bool Text_VI(const uint64_t opcode, size_t &len,
             std::vector<BN::InstructionTextToken> &result) {
  const auto opcode2 = ExtractTypeVIOpcode(opcode);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);
  const auto imm = ExtractTypeVIImm16(opcode);
  const auto imm32 = ExtractTypeVIImm32(opcode);

  char buf[32];
  switch (opcode2) {
    case Opcodes::OP_VI_6BIT_ADDI:
      ITEXT("addi")
      break;

    case Opcodes::OP_VI_6BIT_MOVEA_OR_MOV:
      if (reg2 == Registers::R0) {
        // Move 32-bit immediate
        // Text format: mov imm32, reg1
        ITEXT("mov")

        std::snprintf(buf, sizeof(buf), "%#x", imm32);
        result.emplace_back(IntegerToken, buf, imm32, sizeof(imm32));
        result.emplace_back(OperandSeparatorToken, ", ");

        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
        result.emplace_back(RegisterToken, buf, reg1);

        len = Sizes::LEN48BIT;  // Format VI mov is 48-bit
        return true;

      } else {
        // Move effective address; reg2 cannot be 0
        // Text format: movea imm16, reg1, reg2
        ITEXT("movea")
        break;
      }
    case Opcodes::OP_VI_6BIT_ORI:
      ITEXT("ori")
      break;
    case Opcodes::OP_VI_6BIT_XORI:
      ITEXT("xori")
      break;
    case Opcodes::OP_VI_6BIT_ANDI:
      ITEXT("andi")
      break;
    case Opcodes::OP_VI_6BIT_MULHI:
      ITEXT("mulhi")
      break;
    default:
      return false;
  }

  std::snprintf(buf, sizeof(buf), "%#x", imm);
  result.emplace_back(IntegerToken, buf, imm, sizeof(imm));
  result.emplace_back(OperandSeparatorToken, ", ");

  std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
  result.emplace_back(RegisterToken, buf, reg1);
  result.emplace_back(OperandSeparatorToken, ", ");

  std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
  result.emplace_back(RegisterToken, buf, reg2);

  len = Sizes::LEN32BIT;
  return true;
}

bool Text_VI_MOVHI_SATSUBI_XIII_DISPOSE(
    const uint64_t opcode, size_t &len,
    std::vector<BN::InstructionTextToken> &result) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  char buf[32];
  if (reg2 == Registers::R0) {
    // Function dispose; has several forms
    ITEXT("dispose")

    auto imm5 = ExtractTypeXIIIImm5(opcode);
    std::snprintf(buf, sizeof(buf), "%#x", imm5);
    result.emplace_back(IntegerToken, buf, imm5, sizeof(imm5));
    result.emplace_back(OperandSeparatorToken, ", ");

    GenerateTextForRegisterList12(opcode, result);

    if ((opcode >> 16 & Opcodes::MASK_XIII_SUBOP_DISPOSE) != 0) {
      // Text format: dispose imm5, list12, [reg1]
      result.emplace_back(OperandSeparatorToken, ", ");
      result.emplace_back(TextToken, "[");
      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
      result.emplace_back(RegisterToken, buf, reg1);
      result.emplace_back(TextToken, "]");
    }  // Else, text format: dispose imm5, list12

    len = Sizes::LEN32BIT;
    return true;
  }
  uint16_t imm16 = ExtractTypeVIImm16(opcode);

  switch (Extract6BitOpcode(opcode)) {
    case Opcodes::OP_VI_6BIT_MOVHI:
      // Move high halfword
      // Text format: movhi imm16, reg1, reg2
      ITEXT("movhi")

      std::snprintf(buf, sizeof(buf), "%#x", imm16);
      result.emplace_back(IntegerToken, buf, imm16, sizeof(imm16));
      result.emplace_back(OperandSeparatorToken, ", ");

      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
      result.emplace_back(RegisterToken, buf, reg1);
      result.emplace_back(OperandSeparatorToken, ", ");

      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
      result.emplace_back(RegisterToken, buf, reg2);
      break;

    case Opcodes::OP_VI_6BIT_SATSUBI:
      // Saturated subtract immediate
      // Text format: satsubi imm16, reg1, reg2
      ITEXT("satsubi")

      std::snprintf(buf, sizeof(buf), "%#x", imm16);
      result.emplace_back(IntegerToken, buf, imm16, sizeof(imm16));
      result.emplace_back(OperandSeparatorToken, ", ");

      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
      result.emplace_back(RegisterToken, buf, reg1);
      result.emplace_back(OperandSeparatorToken, ", ");

      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
      result.emplace_back(RegisterToken, buf, reg2);
      break;

    default:
      return false;
  }
  len = Sizes::LEN32BIT;
  return true;
}

bool Text_VII_LDB_LDH_LDW_STB_STH_STW(
    const uint64_t opcode, size_t &len,
    std::vector<BN::InstructionTextToken> &result) {
  const auto opcode2 = ExtractTypeVIIOpcode(opcode);
  const auto subop = ExtractTypeVIISubop(opcode);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);
  int16_t disp16;

  char buf[32];
  switch (opcode2) {
    case Opcodes::OP_VII_6BIT_LD_B:
      ITEXT("ld.b")

      disp16 = static_cast<int16_t>(ExtractTypeVIIDisp16B(opcode));
      std::snprintf(buf, sizeof(buf), "%#x", disp16);
      result.emplace_back(IntegerToken, buf, disp16, sizeof(disp16));

      result.emplace_back(TextToken, "[");
      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
      result.emplace_back(RegisterToken, buf, reg1);
      result.emplace_back(TextToken, "]");

      result.emplace_back(OperandSeparatorToken, ", ");
      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
      result.emplace_back(RegisterToken, buf, reg2);

      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VII_6BIT_LD_H_OR_LD_W:
      if (subop == Opcodes::SUBOP_VII_LD_H) {
        ITEXT("ld.h")

        disp16 = static_cast<int16_t>(ExtractTypeVIIDisp16H(opcode));
        std::snprintf(buf, sizeof(buf), "%#x", disp16);
        result.emplace_back(IntegerToken, buf, disp16, sizeof(disp16));

        result.emplace_back(TextToken, "[");
        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
        result.emplace_back(RegisterToken, buf, reg1);
        result.emplace_back(TextToken, "]");

        result.emplace_back(OperandSeparatorToken, ", ");
        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
        result.emplace_back(RegisterToken, buf, reg2);

        len = Sizes::LEN32BIT;
        return true;

      } else if (subop == Opcodes::SUBOP_VII_LD_W) {
        ITEXT("ld.w")

        disp16 = static_cast<int16_t>(ExtractTypeVIIDisp16W(opcode));
        std::snprintf(buf, sizeof(buf), "%#x", disp16);
        result.emplace_back(IntegerToken, buf, disp16, sizeof(disp16));

        result.emplace_back(TextToken, "[");
        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
        result.emplace_back(RegisterToken, buf, reg1);
        result.emplace_back(TextToken, "]");

        result.emplace_back(OperandSeparatorToken, ", ");
        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
        result.emplace_back(RegisterToken, buf, reg2);

        len = Sizes::LEN32BIT;
        return true;
      }

    case Opcodes::OP_VII_6BIT_ST_B:
      ITEXT("st.b")

      disp16 = static_cast<int16_t>(ExtractTypeVIIDisp16B(opcode));
      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
      result.emplace_back(RegisterToken, buf, reg2);
      result.emplace_back(OperandSeparatorToken, ", ");

      std::snprintf(buf, sizeof(buf), "%#x", disp16);
      result.emplace_back(IntegerToken, buf, disp16, sizeof(disp16));

      result.emplace_back(TextToken, "[");
      std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
      result.emplace_back(RegisterToken, buf, reg1);
      result.emplace_back(TextToken, "]");

      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VII_6BIT_ST_H_OR_ST_W:
      if (subop == Opcodes::SUBOP_VII_ST_H) {
        ITEXT("st.h")

        disp16 = static_cast<int16_t>(ExtractTypeVIIDisp16H(opcode));
        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
        result.emplace_back(RegisterToken, buf, reg2);
        result.emplace_back(OperandSeparatorToken, ", ");

        std::snprintf(buf, sizeof(buf), "%#x", disp16);
        result.emplace_back(IntegerToken, buf, disp16, sizeof(disp16));

        result.emplace_back(TextToken, "[");
        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
        result.emplace_back(RegisterToken, buf, reg1);
        result.emplace_back(TextToken, "]");

        len = Sizes::LEN32BIT;
        return true;

      } else if (subop == Opcodes::SUBOP_VII_ST_W) {
        ITEXT("st.w")

        disp16 = static_cast<int16_t>(ExtractTypeVIIDisp16W(opcode));
        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
        result.emplace_back(RegisterToken, buf, reg2);
        result.emplace_back(OperandSeparatorToken, ", ");

        std::snprintf(buf, sizeof(buf), "%#x", disp16);
        result.emplace_back(IntegerToken, buf, disp16, sizeof(disp16));

        result.emplace_back(TextToken, "[");
        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
        result.emplace_back(RegisterToken, buf, reg1);
        result.emplace_back(TextToken, "]");

        len = Sizes::LEN32BIT;
        return true;
      }
    default:
      return false;
  }
}

bool Text_VIII_SET1_NOT1_CLR1_TST1(
    const uint64_t opcode, size_t &len,
    std::vector<BN::InstructionTextToken> &result) {
  auto subop = ExtractTypeVIIISubop(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);
  auto bitNum = ExtractTypeVIIIBitnum(opcode);
  auto disp16 = ExtractTypeVIIDisp16B(opcode);

  switch (subop) {
    case Opcodes::SUBOP_SET1:
      // Set bit
      // Text format: set1 bit#3, disp16[reg1]
      ITEXT("set1")
      break;

    case Opcodes::SUBOP_NOT1:
      // Not bit
      // Text format: not1 bit#3, disp16[reg1]
      ITEXT("not1")
      break;

    case Opcodes::SUBOP_CLR1:
      // Clear bit
      // Text format: clr1 bit#3, disp16[reg1]
      ITEXT("clr1")
      break;

    case Opcodes::SUBOP_TST1:
      // Test bit
      // Text format: tst1 bit#3, disp16[reg1]
      ITEXT("tst1")
      break;
    default:
      break;
  }

  char buf[32];
  std::snprintf(buf, sizeof(buf), "%#x", bitNum);
  result.emplace_back(IntegerToken, buf, bitNum, sizeof(bitNum));
  result.emplace_back(OperandSeparatorToken, ", ");

  std::snprintf(buf, sizeof(buf), "%#x", disp16);
  result.emplace_back(IntegerToken, buf, disp16, sizeof(disp16));
  result.emplace_back(TextToken, "[");
  std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
  result.emplace_back(RegisterToken, buf, reg1);
  result.emplace_back(TextToken, "]");

  len = Sizes::LEN32BIT;
  return true;
}

bool Format_Ext_Text(const uint64_t opcode, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  char buf[32];
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  if ((opcode >> 16 & OpcodeFields::MASK_VII_SUBOP) ==
      Opcodes::SUBOP_VII_LD_BU_HU) {
    // ld.hu: load halfword unsigned, actually a format VII

    uint16_t disp16 = ExtractTypeVIIDisp16H(opcode);

    ITEXT("ld.hu")

    std::snprintf(buf, sizeof(buf), "%#x", disp16);
    result.emplace_back(IntegerToken, buf, disp16, sizeof(disp16));

    result.emplace_back(TextToken, "[");
    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
    result.emplace_back(RegisterToken, buf, reg1);
    result.emplace_back(TextToken, "]");

    result.emplace_back(OperandSeparatorToken, ", ");
    std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
    result.emplace_back(RegisterToken, buf, reg2);

    len = Sizes::LEN32BIT;
    return true;
  }

  // Note: not all of these are used by all instructions
  const auto reg3 = ExtractReg3OpcodeField(opcode);

  auto imm5 = ExtractTypeXIIImm5(opcode);
  auto imm9 = ExtractTypeXIIImm9(opcode);

  // Note: extended opcode bit 1 is always 0
  if (opcode >> 16 & OpcodeFields::OPCODE_BIT_2) {    // 01
    if (opcode >> 16 & OpcodeFields::OPCODE_BIT_3) {  // 011
      // There aren't any instructions with 0111, so these opcodes will start
      // with 0110
      if (opcode >> 16 &
          OpcodeFields::OPCODE_BIT_5) {  // 01101; format XII bsw, bsh, hsw
        switch (opcode >> 16 & OpcodeFields::MASK_XII_SUBOP_BSW_BSH_HSW) {
          case Opcodes::SUBOP_XII_BSW:
            // Byte swap word; for endian translation
            // Text format: bsw reg2, reg3
            ITEXT("bsw")

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
            result.emplace_back(RegisterToken, buf, reg2);
            result.emplace_back(OperandSeparatorToken, ", ");

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg3));
            result.emplace_back(RegisterToken, buf, reg3);

            len = Sizes::LEN32BIT;
            return true;

          case Opcodes::SUBOP_XII_BSH:
            // Byte swap halfword; for endian translation
            // Text format: bsh reg2, reg3
            ITEXT("bsh")

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
            result.emplace_back(RegisterToken, buf, reg2);
            result.emplace_back(OperandSeparatorToken, ", ");

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg3));
            result.emplace_back(RegisterToken, buf, reg3);

            len = Sizes::LEN32BIT;
            return true;

          case Opcodes::SUBOP_XII_HSW:
            // Halfword swap word; for endian translation
            // Text format: hsw reg2, reg3
            ITEXT("hsw")

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
            result.emplace_back(RegisterToken, buf, reg2);
            result.emplace_back(OperandSeparatorToken, ", ");

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg3));
            result.emplace_back(RegisterToken, buf, reg3);

            len = Sizes::LEN32BIT;
            return true;
          default:
            return false;
        }
      } else {  // 01100; conditional move (two forms)
        const auto condition_CMOV = ExtractTypeXICond(opcode);

        if (opcode >> 16 &
            OpcodeFields::OPCODE_BIT_6) {  // 011001, format XI cmov
          // Conditional move
          // Text format: cmov cccc, reg1, reg2, reg3
          ITEXT("cmov")

          result.emplace_back(TextToken, ConditionToStr(condition_CMOV));
          result.emplace_back(OperandSeparatorToken, ", ");

          std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
          result.emplace_back(RegisterToken, buf, reg1);
          result.emplace_back(OperandSeparatorToken, ", ");

          std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
          result.emplace_back(RegisterToken, buf, reg2);
          result.emplace_back(OperandSeparatorToken, ", ");

          std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg3));
          result.emplace_back(RegisterToken, buf, reg3);

          len = Sizes::LEN32BIT;
          return true;

        } else {  // 011000, format XII cmov
          // Conditional move
          // Text format: cmov ccc, imm5, reg2, reg3
          ITEXT("cmov")

          result.emplace_back(TextToken, ConditionToStr(condition_CMOV));
          result.emplace_back(OperandSeparatorToken, ", ");

          std::snprintf(buf, sizeof(buf), "%#x", imm5);
          result.emplace_back(IntegerToken, buf, imm5, sizeof(imm5));
          result.emplace_back(OperandSeparatorToken, ", ");

          std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
          result.emplace_back(RegisterToken, buf, reg2);
          result.emplace_back(OperandSeparatorToken, ", ");

          std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg3));
          result.emplace_back(RegisterToken, buf, reg3);

          len = Sizes::LEN32BIT;
          return true;
        }
      }
    } else {                                            // 010
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_4) {  // 0101
        if (opcode >> 16 &
            OpcodeFields::OPCODE_BIT_5) {  // 01011; format XI div, divu
          if (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) {  // divu
            // Divide word unsigned; divide reg2 by reg1, quotient in reg2 and
            // remainder in reg3 Text format: divu reg1, reg2, reg3
            ITEXT("divu")

          } else {  // div
            // Divide word; divide reg2 by reg1, quotient in reg2 and remainder
            // in reg3 Text format: div reg1, reg2, reg3
            ITEXT("div")
          }
        } else {  // 01010; format XI divh, divhu
          if (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) {  // divhu
            // Divide halfword unsigned; divide reg2 by lower half of reg1;
            // quotient in reg2 ad remainder in reg3 Text format: divhu reg1,
            // reg2, reg3
            ITEXT("divhu")

          } else {  // divh
            // Divide halfword; divide reg2 by lower half of reg1, quotient in
            // reg2 and remainder in reg3 Text format: divh reg1, reg2, reg3
            ITEXT("divh")
          }
        }
        /* This part applies to format XI mul, mulu, divh, divhu, div, and divu
         */
        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
        result.emplace_back(RegisterToken, buf, reg1);
        result.emplace_back(OperandSeparatorToken, ", ");

        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
        result.emplace_back(RegisterToken, buf, reg2);
        result.emplace_back(OperandSeparatorToken, ", ");

        std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg3));
        result.emplace_back(RegisterToken, buf, reg3);

        len = Sizes::LEN32BIT;
        return true;

      } else {  // 0100
        if (opcode >> 16 &
            OpcodeFields::OPCODE_BIT_5) {  // 01001; format XII mul, mulu
          if (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) {  // mulu
            // Multiply word unsigned by 9-bit immediate
            // Text format: mulu imm9, reg2, reg3
            ITEXT("mulu")

          } else {  // mul
            // Multiply word by 9-bit immediate
            // Text format: mul imm9, reg2, reg3
            ITEXT("mul")
          }
          std::snprintf(buf, sizeof(buf), "%#x", imm9);
          result.emplace_back(IntegerToken, buf, imm9, sizeof(imm9));
          result.emplace_back(OperandSeparatorToken, ", ");

          std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
          result.emplace_back(RegisterToken, buf, reg2);
          result.emplace_back(OperandSeparatorToken, ", ");

          std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg3));
          result.emplace_back(RegisterToken, buf, reg3);

          len = Sizes::LEN32BIT;
          return true;

        } else {  // 01000
          if (opcode >> 16 &
              OpcodeFields::OPCODE_BIT_6) {  // 010001; format XI mul, mulu
            if (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) {  // mulu
              // Multiply word unsigned by register; stores higher 32bits in
              // reg3 and lower 32 bits in reg2 Text format: mulu reg1, reg2,
              // reg3
              ITEXT("mulu")

            } else {  // mul
              // Multiply word by register;  stores higher 32bits in reg3 and
              // lower 32 bits in reg2 Text format: mul reg1, reg2, reg3
              ITEXT("mul")
            }
            /* This part applies to format XI mul, mulu, divh, divhu, div, and
             * divu */
            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
            result.emplace_back(RegisterToken, buf, reg1);
            result.emplace_back(OperandSeparatorToken, ", ");

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
            result.emplace_back(RegisterToken, buf, reg2);
            result.emplace_back(OperandSeparatorToken, ", ");

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg3));
            result.emplace_back(RegisterToken, buf, reg3);

            len = Sizes::LEN32BIT;
            return true;
          } else {  // 010000; sasf
            // Shift and set flag condition; reg2 shifted left by 1, and if
            // condition is satisfied, LSB set to 1,
            //  else if condition not satisfied, LSB set to 0
            // Text format: sasf cond, reg2
            ITEXT("sasf")

            auto condition_sasf =
                static_cast<uint8_t>(opcode >> 16 & OpcodeFields::MASK_IX_COND);
            result.emplace_back(TextToken, ConditionToStr(condition_sasf));
            result.emplace_back(OperandSeparatorToken, ", ");

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
            result.emplace_back(RegisterToken, buf, reg2);

            len = Sizes::LEN32BIT;
            return true;
          }
        }
      }
    }
  } else {                                            // 00
    if (opcode >> 16 & OpcodeFields::OPCODE_BIT_3) {  // 001
      // No opcodes have 0011, so these all begin with 0010
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_5) {      // 00101
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {    // 001011
          if (opcode & OpcodeFields::MASK_X_SUBOP_EI_DI) {  // ei
            // Enable interrupt
            ITEXT("ei")

            len = Sizes::LEN32BIT;
            return true;

          } else {  // di
            // Disable interrupt
            ITEXT("di")

            len = Sizes::LEN32BIT;
            return true;
          }
        } else {  // 001010; format X reti, ctret, dbret
          switch (opcode >> 16 & OpcodeFields::MASK_SUBOP_BITS_17_18) {
            case Opcodes::SUBOP_X_RETI:
              // Return from trap or interrupt
              ITEXT("reti")
              len = Sizes::LEN32BIT;
              return true;

            case Opcodes::SUBOP_X_CTRET:
              // Return from callt
              ITEXT("ctret")
              len = Sizes::LEN32BIT;
              return true;

            case Opcodes::SUBOP_X_DBRET:
              // Return from debug trap
              ITEXT("dbret")
              len = Sizes::LEN32BIT;
              return true;
            default:
              return false;
          }
        }
      } else {                                            // 00100
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {  // 001001; halt
          // Halt; stop operating clock of CPU and place into halt mode
          // Caused by reset input/NMI/unmasked maskable interrupt request
          ITEXT("halt")

          len = Sizes::LEN32BIT;
          return true;

        } else {  // 001000; trap
          // Trap
          // Text format: trap vector
          ITEXT("trap")

          auto vector =
              static_cast<uint8_t>(opcode & OpcodeFields::MASK_X_IMM_VECTOR);
          std::snprintf(buf, sizeof(buf), "%#x", vector);
          result.emplace_back(IntegerToken, buf, vector, sizeof(vector));

          len = Sizes::LEN32BIT;
          return true;
        }
      }
    } else {                                                // 000
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_4) {      // 0001
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_5) {    // 00011
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {  // 000111
            // Determine opcode by last 3 bits of halfword (bits 16-18 of
            // instruction)
            switch ((opcode >> 16 & OpcodeFields::MASK_SUBOP_BITS_17_18) >>
                    OpcodeFields::SHIFT_SUBOP_BITS_17_18) {
              case Opcodes::SUBOP_SET1:
                // Set single bit
                // Text format: set1 reg2, [reg1]
                ITEXT("set1")

                std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
                result.emplace_back(RegisterToken, buf, reg2);
                result.emplace_back(OperandSeparatorToken, ", ");

                result.emplace_back(TextToken, "[");
                std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
                result.emplace_back(RegisterToken, buf, reg1);
                result.emplace_back(TextToken, "]");

                len = Sizes::LEN32BIT;
                return true;

              case Opcodes::SUBOP_NOT1:
                // Not operation on single bit
                // Text format: not1 reg2, [reg1]
                ITEXT("not1")

                std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
                result.emplace_back(RegisterToken, buf, reg2);
                result.emplace_back(OperandSeparatorToken, ", ");

                result.emplace_back(TextToken, "[");
                std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
                result.emplace_back(RegisterToken, buf, reg1);
                result.emplace_back(TextToken, "]");

                len = Sizes::LEN32BIT;
                return true;

              case Opcodes::SUBOP_CLR1:
                // Clear single bit
                // Text format: clr1 reg2, [reg1]
                ITEXT("clr1")

                std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
                result.emplace_back(RegisterToken, buf, reg2);
                result.emplace_back(OperandSeparatorToken, ", ");

                result.emplace_back(TextToken, "[");
                std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
                result.emplace_back(RegisterToken, buf, reg1);
                result.emplace_back(TextToken, "]");

                len = Sizes::LEN32BIT;
                return true;

              case Opcodes::SUBOP_TST1:
                // Test single bit
                // Text format: tst1 reg2, [reg1]
                ITEXT("tst1")

                std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
                result.emplace_back(RegisterToken, buf, reg2);
                result.emplace_back(OperandSeparatorToken, ", ");

                result.emplace_back(TextToken, "[");
                std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
                result.emplace_back(RegisterToken, buf, reg1);
                result.emplace_back(TextToken, "]");

                len = Sizes::LEN32BIT;
                return true;
              default:
                return false;
            }
          } else {  // 000110; shl
            // Logical shift left
            // Text format: shl reg1, reg2
            ITEXT("shl")

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
            result.emplace_back(RegisterToken, buf, reg1);
            result.emplace_back(OperandSeparatorToken, ", ");

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
            result.emplace_back(RegisterToken, buf, reg2);

            len = Sizes::LEN32BIT;
            return true;
          }
        } else {                                            // 00010
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {  // 000101; sar
            // Arithmetic shift right
            // Text format: sar reg1, reg2
            ITEXT("sar")

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
            result.emplace_back(RegisterToken, buf, reg1);
            result.emplace_back(OperandSeparatorToken, ", ");

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
            result.emplace_back(RegisterToken, buf, reg2);

            len = Sizes::LEN32BIT;
            return true;

          } else {  // 000100; shr
            // Logical shift right
            // Text format: shr reg1, reg2
            ITEXT("shr")

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg1));
            result.emplace_back(RegisterToken, buf, reg1);
            result.emplace_back(OperandSeparatorToken, ", ");

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
            result.emplace_back(RegisterToken, buf, reg2);

            len = Sizes::LEN32BIT;
            return true;
          }
        }
      } else {  // 0000
        if (opcode >> 16 &
            OpcodeFields::OPCODE_BIT_5) {  // 00001; Only option is 000010; stsr
          // Store contents of system register
          // Text format: stsr regID, reg2
          ITEXT("stsr")

          // RegID the same field as reg1; the difference is that it identifies
          // a system register instead
          std::snprintf(buf, sizeof(buf), "%s", SystemRegToStr(reg1));
          result.emplace_back(
              PossibleAddressToken, buf,
              (Registers::SYSTEM_REG_BASE + reg1 * Registers::REGISTER_SIZE));
          result.emplace_back(OperandSeparatorToken, ", ");

          std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
          result.emplace_back(RegisterToken, buf, reg2);

          len = Sizes::LEN32BIT;
          return true;

        } else {                                            // 00000
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {  // 000001; ldsr
            // Load to system register
            // Text format: ldsr reg2, regID
            ITEXT("ldsr")

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
            result.emplace_back(RegisterToken, buf, reg2);
            result.emplace_back(OperandSeparatorToken, ", ");

            // RegID the same field as reg1; the difference is that it
            // identifies a system register instead
            std::snprintf(buf, sizeof(buf), "%s", SystemRegToStr(reg1));
            result.emplace_back(
                PossibleAddressToken, buf,
                (Registers::SYSTEM_REG_BASE + reg1 * Registers::REGISTER_SIZE));

            len = Sizes::LEN32BIT;
            return true;

          } else {  // 000000; setf
            // Set flag condition; if condition is met, sets reg2 to 1; if not,
            // sets to 0 Text format: setf cond, reg2
            ITEXT("setf")

            auto condition_setf =
                static_cast<uint8_t>(opcode & OpcodeFields::MASK_IX_COND);
            result.emplace_back(TextToken, ConditionToStr(condition_setf));
            result.emplace_back(OperandSeparatorToken, ", ");

            std::snprintf(buf, sizeof(buf), "%s", RegToStr(reg2));
            result.emplace_back(RegisterToken, buf, reg2);

            len = Sizes::LEN32BIT;
            return true;
          }
        }
      }
    }
  }
}
/*
 * Instruction text methods
 */
bool AddImm5R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Text_II_Generic_Imm5_Reg2("add", opcode, len, result);
}

bool AddR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Text_I_Generic_Reg1_Reg2("add", opcode, len, result);
}

bool AndiImm16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                         std::vector<BN::InstructionTextToken> &result) {
  return Text_VI(opcode, len, result);
}

bool AddiImm32R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                         std::vector<BN::InstructionTextToken> &result) {
  return Text_VI(opcode, len, result);
}

bool AndR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Text_I_Generic_Reg1_Reg2("and", opcode, len, result);
}

bool Bc::Text(const uint64_t opcode, uint64_t addr, size_t &len,
              std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bge::Text(const uint64_t opcode, uint64_t addr, size_t &len,
               std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bgt::Text(const uint64_t opcode, uint64_t addr, size_t &len,
               std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bh::Text(const uint64_t opcode, uint64_t addr, size_t &len,
              std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Ble::Text(const uint64_t opcode, uint64_t addr, size_t &len,
               std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Blt::Text(const uint64_t opcode, uint64_t addr, size_t &len,
               std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bn::Text(const uint64_t opcode, uint64_t addr, size_t &len,
              std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bnc::Text(const uint64_t opcode, uint64_t addr, size_t &len,
               std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bnh::Text(const uint64_t opcode, uint64_t addr, size_t &len,
               std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bnv::Text(const uint64_t opcode, uint64_t addr, size_t &len,
               std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bnz::Text(const uint64_t opcode, uint64_t addr, size_t &len,
               std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bp::Text(const uint64_t opcode, uint64_t addr, size_t &len,
              std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Br::Text(const uint64_t opcode, uint64_t addr, size_t &len,
              std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bsa::Text(const uint64_t opcode, uint64_t addr, size_t &len,
               std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool BshR2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool BswR2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool Bv::Text(const uint64_t opcode, uint64_t addr, size_t &len,
              std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool Bz::Text(const uint64_t opcode, uint64_t addr, size_t &len,
              std::vector<BN::InstructionTextToken> &result) {
  return Text_III_BCOND(opcode, addr, len, result);
}

bool CalltImm6::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Text_II_CALLT_SATADD_MOV(opcode, len, result);
}

bool Clr1Bit3Disp16R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                            std::vector<BN::InstructionTextToken> &result) {
  return Text_VIII_SET1_NOT1_CLR1_TST1(opcode, len, result);
}

bool Clr1R2R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool CmovCcccR1R2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                          std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool CmovCccImm5R2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                           std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool CmpImm5R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Text_II_Generic_Imm5_Reg2("cmp", opcode, len, result);
}

bool CmpR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Text_I_Generic_Reg1_Reg2("cmp", opcode, len, result);
}

bool Ctret::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                 std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool Dbret::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                 std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool Dbtrap::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                  std::vector<BN::InstructionTextToken> &result) {
  return Text_I_SWITCH_DBTRAP_DIVH(opcode, len, result);
}

bool Di::Text(const uint64_t opcode, uint64_t addr, size_t &len,
              std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool DisposeImm5List12::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                             std::vector<BN::InstructionTextToken> &result) {
  return Text_VI_MOVHI_SATSUBI_XIII_DISPOSE(opcode, len, result);
}

bool DisposeImm5List12R1::Text(const uint64_t opcode, uint64_t addr,
                               size_t &len,
                               std::vector<BN::InstructionTextToken> &result) {
  return Text_VI_MOVHI_SATSUBI_XIII_DISPOSE(opcode, len, result);
}

bool DivR1R2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool DivhR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  return Text_I_SWITCH_DBTRAP_DIVH(opcode, len, result);
}

bool DivhR1R2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                      std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool DivhuR1R2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool DivuR1R2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                      std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool Ei::Text(const uint64_t opcode, uint64_t addr, size_t &len,
              std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool Halt::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool HswR2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool JarlDisp22R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                        std::vector<BN::InstructionTextToken> &result) {
  return Text_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, len, result);
}

bool JmpR1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                 std::vector<BN::InstructionTextToken> &result) {
  return Text_I_JMP_IV_SLDHU_SLDBU(opcode, len, result);
}

bool JrDisp22::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  return Text_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, len, result);
}

bool LdbDisp16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                         std::vector<BN::InstructionTextToken> &result) {
  return Text_VII_LDB_LDH_LDW_STB_STH_STW(opcode, len, result);
}

bool LdbuDisp16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                          std::vector<BN::InstructionTextToken> &result) {
  return Text_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, len, result);
}

bool LdhDisp16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                         std::vector<BN::InstructionTextToken> &result) {
  return Text_VII_LDB_LDH_LDW_STB_STH_STW(opcode, len, result);
}

bool LdhuDisp16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                          std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool LdsrR1Rid::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool LdwDisp16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                         std::vector<BN::InstructionTextToken> &result) {
  return Text_VII_LDB_LDH_LDW_STB_STH_STW(opcode, len, result);
}

bool MovhiImm16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                          std::vector<BN::InstructionTextToken> &result) {
  return Text_VI_MOVHI_SATSUBI_XIII_DISPOSE(opcode, len, result);
}

bool MoveaImm16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                          std::vector<BN::InstructionTextToken> &result) {
  return Text_VI(opcode, len, result);
}

bool MovImm32R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                      std::vector<BN::InstructionTextToken> &result) {
  return Text_VI(opcode, len, result);
}

bool MovImm5R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Text_II_CALLT_SATADD_MOV(opcode, len, result);
}

bool MovR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Text_I_MOV_NOP(opcode, len, result);
}

bool MulhiImm16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                          std::vector<BN::InstructionTextToken> &result) {
  return Text_VI(opcode, len, result);
}

bool MulhImm5R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                      std::vector<BN::InstructionTextToken> &result) {
  return Text_II_Generic_Imm5_Reg2("mulh", opcode, len, result);
}

bool MulImm9R2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool MulhR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  return Text_I_MULH_SXH(opcode, len, result);
}

bool MulR1R2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool MuluImm9R2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                        std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool MuluR1R2R3::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                      std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool Nop::Text(const uint64_t opcode, uint64_t addr, size_t &len,
               std::vector<BN::InstructionTextToken> &result) {
  return Text_I_MOV_NOP(opcode, len, result);
}

bool Not1Bit3Disp16R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                            std::vector<BN::InstructionTextToken> &result) {
  return Text_VIII_SET1_NOT1_CLR1_TST1(opcode, len, result);
}

bool Not1R2R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool NotR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Text_I_Generic_Reg1_Reg2("not", opcode, len, result);
}

bool OriImm16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                        std::vector<BN::InstructionTextToken> &result) {
  return Text_VI(opcode, len, result);
}

bool OrR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                  std::vector<BN::InstructionTextToken> &result) {
  return Text_I_Generic_Reg1_Reg2("or", opcode, len, result);
}

bool PrepareList12Imm5::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                             std::vector<BN::InstructionTextToken> &result) {
  return Text_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, len, result);
}

bool PrepareList12Imm5Sp::Text(const uint64_t opcode, uint64_t addr,
                               size_t &len,
                               std::vector<BN::InstructionTextToken> &result) {
  return Text_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, len, result);
}

bool PrepareList12Imm5SpImm16SignExt::Text(
    const uint64_t opcode, uint64_t addr, size_t &len,
    std::vector<BN::InstructionTextToken> &result) {
  return Text_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, len, result);
}

bool PrepareList12Imm5SpImm16LogicShift::Text(
    const uint64_t opcode, uint64_t addr, size_t &len,
    std::vector<BN::InstructionTextToken> &result) {
  return Text_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, len, result);
}

bool PrepareList12Imm5SpImm32::Text(
    const uint64_t opcode, uint64_t addr, size_t &len,
    std::vector<BN::InstructionTextToken> &result) {
  return Text_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, len, result);
}

bool Reti::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool SarImm5R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Text_II_Generic_Imm5_Reg2("sar", opcode, len, result);
}

bool SarR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool SasfCondR2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                      std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool SataddImm5::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                      std::vector<BN::InstructionTextToken> &result) {
  return Text_II_CALLT_SATADD_MOV(opcode, len, result);
}

bool SataddR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                      std::vector<BN::InstructionTextToken> &result) {
  return Text_I_SATADD_ZXH(opcode, len, result);
}

bool SatsubiImm16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                            std::vector<BN::InstructionTextToken> &result) {
  return Text_VI_MOVHI_SATSUBI_XIII_DISPOSE(opcode, len, result);
}

bool SatsubR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                      std::vector<BN::InstructionTextToken> &result) {
  return Text_I_SATSUB_SXB(opcode, len, result);
}

bool SatsubrR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  return Text_I_SATSUBR_ZXB(opcode, len, result);
}

bool Set1Bit3Disp16R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                            std::vector<BN::InstructionTextToken> &result) {
  return Text_VIII_SET1_NOT1_CLR1_TST1(opcode, len, result);
}

bool Set1R2R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool SetfCondR2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                      std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool ShlImm5R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Text_II_Generic_Imm5_Reg2("shl", opcode, len, result);
}

bool ShlR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool ShrImm5R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Text_II_Generic_Imm5_Reg2("shr", opcode, len, result);
}

bool ShrR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool SldbuDisp4R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                        std::vector<BN::InstructionTextToken> &result) {
  return Text_I_JMP_IV_SLDHU_SLDBU(opcode, len, result);
}

bool SldbDisp7R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  return Text_IV_SLDB(opcode, len, result);
}

bool SldhuDisp5R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                        std::vector<BN::InstructionTextToken> &result) {
  return Text_I_JMP_IV_SLDHU_SLDBU(opcode, len, result);
}

bool SldhDisp8R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  return Text_IV_SLDH(opcode, len, result);
}

bool SldwDisp8R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  return Text_IV_SLDW_SSTW(opcode, len, result);
}

bool SstbR2Disp7::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  return Text_IV_SSTB(opcode, len, result);
}

bool SsthR2Disp8::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  return Text_IV_SSTH(opcode, len, result);
}

bool SstwR2Disp8::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                       std::vector<BN::InstructionTextToken> &result) {
  return Text_IV_SLDW_SSTW(opcode, len, result);
}

bool StbR2Disp16R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                         std::vector<BN::InstructionTextToken> &result) {
  return Text_VII_LDB_LDH_LDW_STB_STH_STW(opcode, len, result);
}

bool SthR2Disp26R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                         std::vector<BN::InstructionTextToken> &result) {
  return Text_VII_LDB_LDH_LDW_STB_STH_STW(opcode, len, result);
}

bool StsrRidR2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                     std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool StwR2Disp16R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                         std::vector<BN::InstructionTextToken> &result) {
  return Text_VII_LDB_LDH_LDW_STB_STH_STW(opcode, len, result);
}

bool SubR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Text_I_Generic_Reg1_Reg2("sub", opcode, len, result);
}

bool SubrR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  return Text_I_Generic_Reg1_Reg2("subr", opcode, len, result);
}

bool SwitchR1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  return Text_I_SWITCH_DBTRAP_DIVH(opcode, len, result);
}

bool SxbR1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                 std::vector<BN::InstructionTextToken> &result) {
  return Text_I_SATSUB_SXB(opcode, len, result);
}

bool SxhR1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                 std::vector<BN::InstructionTextToken> &result) {
  return Text_I_MULH_SXH(opcode, len, result);
}

bool Trap::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool Tst1Bit3Disp16R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                            std::vector<BN::InstructionTextToken> &result) {
  return Text_VIII_SET1_NOT1_CLR1_TST1(opcode, len, result);
}

bool Tst1R2R1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) {
  return Format_Ext_Text(opcode, len, result);
}

bool TstR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Text_I_Generic_Reg1_Reg2("tst", opcode, len, result);
}

bool XoriImm16R1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                         std::vector<BN::InstructionTextToken> &result) {
  return Text_VI(opcode, len, result);
}

bool XorR1R2::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                   std::vector<BN::InstructionTextToken> &result) {
  return Text_I_Generic_Reg1_Reg2("xor", opcode, len, result);
}

bool ZxbR1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                 std::vector<BN::InstructionTextToken> &result) {
  return Text_I_SATSUBR_ZXB(opcode, len, result);
}

bool ZxhR1::Text(const uint64_t opcode, uint64_t addr, size_t &len,
                 std::vector<BN::InstructionTextToken> &result) {
  return Text_I_SATADD_ZXH(opcode, len, result);
}
}  // namespace V850