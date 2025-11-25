// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#include "instructions.h"

#include "architecture.h"
#include "conditions.h"
#include "opcodes.h"
#include "registers.h"
#include "sizes.h"

namespace BN = BinaryNinja;

namespace V850 {
Instruction::Instruction(const IsaType &t, const uint8_t len) {
  this->isa_type = t;
  this->len = len;
}

IsaType Instruction::GetIsaType() const { return this->isa_type; }

uint8_t Instruction::GetInstrLen() const { return this->len; }

/*
 * Concrete instruction class constructors
 */

AddImm5R2::AddImm5R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
AddR1R2::AddR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
AndiImm16R1R2::AndiImm16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
AddiImm32R1R2::AddiImm32R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
AndR1R2::AndR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bc::Bc(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bge::Bge(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bgt::Bgt(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bh::Bh(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Ble::Ble(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Blt::Blt(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bn::Bn(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bnc::Bnc(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bnh::Bnh(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bnv::Bnv(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bnz::Bnz(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bp::Bp(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Br::Br(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bsa::Bsa(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
BshR2R3::BshR2R3(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
BswR2R3::BswR2R3(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bv::Bv(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Bz::Bz(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
CalltImm6::CalltImm6(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Clr1Bit3Disp16R1::Clr1Bit3Disp16R1(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Clr1R2R1::Clr1R2R1(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
CmovCcccR1R2R3::CmovCcccR1R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
CmovCccImm5R2R3::CmovCccImm5R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
CmpImm5R2::CmpImm5R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
CmpR1R2::CmpR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Ctret::Ctret(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Dbret::Dbret(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Dbtrap::Dbtrap(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Di::Di(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
DisposeImm5List12::DisposeImm5List12(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
DisposeImm5List12R1::DisposeImm5List12R1(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
DivR1R2R3::DivR1R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
DivhR1R2::DivhR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
DivhR1R2R3::DivhR1R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
DivhuR1R2R3::DivhuR1R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
DivuR1R2R3::DivuR1R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Ei::Ei(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Halt::Halt(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
HswR2R3::HswR2R3(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
JarlDisp22R2::JarlDisp22R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
JmpR1::JmpR1(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
JrDisp22::JrDisp22(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
LdbDisp16R1R2::LdbDisp16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
LdbuDisp16R1R2::LdbuDisp16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
LdhDisp16R1R2::LdhDisp16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
LdhuDisp16R1R2::LdhuDisp16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
LdsrR1Rid::LdsrR1Rid(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
LdwDisp16R1R2::LdwDisp16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MovhiImm16R1R2::MovhiImm16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MoveaImm16R1R2::MoveaImm16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MovImm32R1::MovImm32R1(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MovImm5R2::MovImm5R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MovR1R2::MovR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
MulhiImm16R1R2::MulhiImm16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MulhImm5R2::MulhImm5R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MulImm9R2R3::MulImm9R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MulhR1R2::MulhR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
MulR1R2R3::MulR1R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MuluImm9R2R3::MuluImm9R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MuluR1R2R3::MuluR1R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Nop::Nop(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Not1Bit3Disp16R1::Not1Bit3Disp16R1(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Not1R2R1::Not1R2R1(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
NotR1R2::NotR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
OriImm16R1R2::OriImm16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
OrR1R2::OrR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
PrepareList12Imm5::PrepareList12Imm5(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
PrepareList12Imm5Sp::PrepareList12Imm5Sp(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
PrepareList12Imm5SpImm16SignExt::PrepareList12Imm5SpImm16SignExt(
    const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
PrepareList12Imm5SpImm16LogicShift::PrepareList12Imm5SpImm16LogicShift(
    const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
PrepareList12Imm5SpImm32::PrepareList12Imm5SpImm32(const IsaType &t,
                                                   const uint8_t len)
    : Instruction(t, len) {}
Reti::Reti(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
SarImm5R2::SarImm5R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SarR1R2::SarR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
SasfCondR2::SasfCondR2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SataddImm5::SataddImm5(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SataddR1R2::SataddR1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SatsubiImm16R1R2::SatsubiImm16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SatsubR1R2::SatsubR1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SatsubrR1R2::SatsubrR1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Set1Bit3Disp16R1::Set1Bit3Disp16R1(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Set1R2R1::Set1R2R1(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
SetfCondR2::SetfCondR2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
ShlImm5R2::ShlImm5R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
ShlR1R2::ShlR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
ShrImm5R2::ShrImm5R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
ShrR1R2::ShrR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
SldbuDisp4R2::SldbuDisp4R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SldbDisp7R2::SldbDisp7R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SldhuDisp5R2::SldhuDisp5R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SldhDisp8R2::SldhDisp8R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SldwDisp8R2::SldwDisp8R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SstbR2Disp7::SstbR2Disp7(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SsthR2Disp8::SsthR2Disp8(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SstwR2Disp8::SstwR2Disp8(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
StbR2Disp16R1::StbR2Disp16R1(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SthR2Disp26R1::SthR2Disp26R1(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
StsrRidR2::StsrRidR2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
StwR2Disp16R1::StwR2Disp16R1(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
SubR1R2::SubR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
SubrR1R2::SubrR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
SwitchR1::SwitchR1(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
SxbR1::SxbR1(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
SxhR1::SxhR1(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Trap::Trap(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Tst1Bit3Disp16R1::Tst1Bit3Disp16R1(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Tst1R2R1::Tst1R2R1(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
TstR1R2::TstR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
XoriImm16R1R2::XoriImm16R1R2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
XorR1R2::XorR1R2(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
ZxbR1::ZxbR1(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
ZxhR1::ZxhR1(const IsaType &t, const uint8_t len) : Instruction(t, len) {}

/*
 * Instruction decoder method that will parse opcodes and return a
 * pointer to an instruction object
 */
std::optional<std::unique_ptr<Instruction>> DecodeInstruction(
    const IsaType &t, const uint32_t opcode) {
  /* Opcodes always start at bit position 10 -- test to see which op we have. */

  if (opcode & OpcodeFields::OPCODE_BIT_1) {
    return ParsePrefix0b1(t, opcode);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_2) {
    return ParsePrefix0b01(t, opcode);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_3) {
    return ParsePrefix0b001(t, opcode);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_4) {
    return ParsePrefix0b0001(t, opcode);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_5) {
    return ParsePrefix0b00001(t, opcode);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_6) {
    return ParsePrefix0b000001(t, opcode);
  }

  // If none of the above cases were matched, this is a nop/mov
  return ParsePrefix0b0(t, opcode);
}

std::optional<std::unique_ptr<Instruction>> ParsePrefix0b0(
    const IsaType &t, const uint16_t opcode) {
  const auto reg1 = ExtractReg1OpcodeField(opcode);
  const auto reg2 = ExtractReg2OpcodeField(opcode);

  if (reg1 == Registers::R0 && reg2 == Registers::R0) {
    return std::make_unique<Nop>(t, Sizes::LEN16BIT);
  }
  return std::make_unique<MovR1R2>(t, Sizes::LEN16BIT);
}

std::optional<std::unique_ptr<Instruction>> ParsePrefix0b1(
    const IsaType &t, const uint32_t opcode) {
  const auto op4bit = Extract4BitOpcode(opcode);
  const auto op5bit = Extract5BitOpcode(opcode);
  const auto op6bit = Extract6BitOpcode(opcode);
  if (opcode & OpcodeFields::OPCODE_BIT_2) {
    if (op5bit == Opcodes::OP_V_JARL_JR_VII_LDBU_XIII_PREPARE) {
      if (uint8_t subop = opcode >> 16 & OpcodeFields::MASK_V_SUBOP_BIT_16;
          subop == Opcodes::SUBOP_V_JARL_JR) {
        if (const auto reg2 = ExtractReg2OpcodeField(opcode);
            reg2 == Registers::R0) {
          return std::make_unique<JrDisp22>(t, Sizes::LEN32BIT);
        }
        return std::make_unique<JarlDisp22R2>(t, Sizes::LEN32BIT);
      }
      if (uint8_t subop = opcode >> 16 & Opcodes::MASK_XIII_SUBOP_PREPARE;
          subop == Opcodes::SUBOP_XIII_PREPARE_001) {
        return std::make_unique<PrepareList12Imm5>(t, Sizes::LEN32BIT);
      } else if (subop == Opcodes::SUBOP_XIII_PREPARE_011) {
        switch (const auto prepare_format = static_cast<uint8_t>(
                    opcode >> 16 & Opcodes::MASK_XIII_PREPARE_FORMAT);
                prepare_format) {
          case Opcodes::PREPARE_LOAD_SP:
            return std::make_unique<PrepareList12Imm5Sp>(t, Sizes::LEN32BIT);
          case Opcodes::PREPARE_LOAD_SIGN_EXTENDED_IMM16:
            return std::make_unique<PrepareList12Imm5SpImm16SignExt>(
                t, Sizes::LEN48BIT);
          case Opcodes::PREPARE_LOAD_LSL_IMM16:
            return std::make_unique<PrepareList12Imm5SpImm16LogicShift>(
                t, Sizes::LEN48BIT);
          case Opcodes::PREPARE_LOAD_IMM32:
            return std::make_unique<PrepareList12Imm5SpImm32>(t,
                                                              Sizes::LEN64BIT);
          default:
            return std::nullopt;
        }
      }
      return std::make_unique<LdbuDisp16R1R2>(t, Sizes::LEN32BIT);
    }
    if (op6bit == Opcodes::OP_EXT_6BIT) {
      if ((opcode >> 16 & OpcodeFields::MASK_VII_SUBOP) ==
          Opcodes::SUBOP_VII_LD_BU_HU) {
        return std::make_unique<LdhuDisp16R1R2>(t, Sizes::LEN32BIT);
      }
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_2) {
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_3) {
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_5) {
            switch (opcode >> 16 & OpcodeFields::MASK_XII_SUBOP_BSW_BSH_HSW) {
              case Opcodes::SUBOP_XII_BSW:
                return std::make_unique<BswR2R3>(t, Sizes::LEN32BIT);
              case Opcodes::SUBOP_XII_BSH:
                return std::make_unique<BshR2R3>(t, Sizes::LEN32BIT);
              case Opcodes::SUBOP_XII_HSW:
                return std::make_unique<HswR2R3>(t, Sizes::LEN32BIT);
              default:
                return std::nullopt;
            }
          }
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {
            return std::make_unique<CmovCcccR1R2R3>(t, Sizes::LEN32BIT);
          }
          return std::make_unique<CmovCccImm5R2R3>(t, Sizes::LEN32BIT);
        }
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_4) {
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_5) {
            if (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) {
              return std::make_unique<DivuR1R2R3>(t, Sizes::LEN32BIT);
            }
            return std::make_unique<DivR1R2R3>(t, Sizes::LEN32BIT);
          }
          if (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) {
            return std::make_unique<DivhuR1R2R3>(t, Sizes::LEN32BIT);
          }
          return std::make_unique<DivhR1R2R3>(t, Sizes::LEN32BIT);
        }
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_5) {
          if (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) {
            return std::make_unique<MuluImm9R2R3>(t, Sizes::LEN32BIT);
          }
          return std::make_unique<MulImm9R2R3>(t, Sizes::LEN32BIT);
        }
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {
          if (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) {
            return std::make_unique<MuluR1R2R3>(t, Sizes::LEN32BIT);
          }
          return std::make_unique<MulR1R2R3>(t, Sizes::LEN32BIT);
        }
        return std::make_unique<SasfCondR2>(t, Sizes::LEN32BIT);
      }
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_3) {
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_5) {
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {
            if (opcode & OpcodeFields::MASK_X_SUBOP_EI_DI) {
              return std::make_unique<Ei>(t, Sizes::LEN32BIT);
            }
            return std::make_unique<Di>(t, Sizes::LEN32BIT);
          }
          switch (opcode >> 16 & OpcodeFields::MASK_SUBOP_BITS_17_18) {
            case Opcodes::SUBOP_X_RETI:
              return std::make_unique<Reti>(t, Sizes::LEN32BIT);
            case Opcodes::SUBOP_X_CTRET:
              return std::make_unique<Ctret>(t, Sizes::LEN32BIT);
            case Opcodes::SUBOP_X_DBRET:
              return std::make_unique<Dbret>(t, Sizes::LEN32BIT);
            default:
              return std::nullopt;
          }
        }
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {
          return std::make_unique<Halt>(t, Sizes::LEN32BIT);
        }
        return std::make_unique<Trap>(t, Sizes::LEN32BIT);
      }
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_4) {
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_5) {
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {
            switch ((opcode >> 16 & OpcodeFields::MASK_SUBOP_BITS_17_18) >>
                    OpcodeFields::SHIFT_SUBOP_BITS_17_18) {
              case Opcodes::SUBOP_SET1:
                return std::make_unique<Set1R2R1>(t, Sizes::LEN32BIT);
              case Opcodes::SUBOP_NOT1:
                return std::make_unique<Not1R2R1>(t, Sizes::LEN32BIT);
              case Opcodes::SUBOP_CLR1:
                return std::make_unique<Clr1R2R1>(t, Sizes::LEN32BIT);
              case Opcodes::SUBOP_TST1:
                return std::make_unique<Tst1R2R1>(t, Sizes::LEN32BIT);
              default:
                return std::nullopt;
            }
          }
          return std::make_unique<ShlR1R2>(t, Sizes::LEN32BIT);
        }
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {
          return std::make_unique<SarR1R2>(t, Sizes::LEN32BIT);
        }
        return std::make_unique<ShrR1R2>(t, Sizes::LEN32BIT);
      }
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_5) {
        return std::make_unique<StsrRidR2>(t, Sizes::LEN32BIT);
      }
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {
        return std::make_unique<LdsrR1Rid>(t, Sizes::LEN32BIT);
      }
      return std::make_unique<SetfCondR2>(t, Sizes::LEN32BIT);
    }
    if (op6bit == Opcodes::OP_VIII_6BIT_BIT_MANIPULATION) {
      switch (ExtractTypeVIIISubop(opcode)) {
        case Opcodes::SUBOP_SET1:
          return std::make_unique<Set1Bit3Disp16R1>(t, Sizes::LEN32BIT);
        case Opcodes::SUBOP_NOT1:
          return std::make_unique<Not1Bit3Disp16R1>(t, Sizes::LEN32BIT);
        case Opcodes::SUBOP_CLR1:
          return std::make_unique<Clr1Bit3Disp16R1>(t, Sizes::LEN32BIT);
        case Opcodes::SUBOP_TST1:
          return std::make_unique<Tst1Bit3Disp16R1>(t, Sizes::LEN32BIT);
        default:
          return std::nullopt;
      }
    }
    switch (op4bit) {
      case 0b1100:
        if (op5bit == Opcodes::OP_XIII_5BIT_DISPOSE) {
          if (const auto reg2 = ExtractReg2OpcodeField(opcode);
              reg2 == Registers::R0) {
            if ((opcode >> 16 & Opcodes::MASK_XIII_SUBOP_DISPOSE) != 0) {
              return std::make_unique<DisposeImm5List12R1>(t, Sizes::LEN32BIT);
            }
            return std::make_unique<DisposeImm5List12>(t, Sizes::LEN32BIT);
          }
          switch (Extract6BitOpcode(opcode)) {
            case Opcodes::OP_VI_6BIT_MOVHI:
              return std::make_unique<MovhiImm16R1R2>(t, Sizes::LEN32BIT);
            case Opcodes::OP_VI_6BIT_SATSUBI:
              return std::make_unique<SatsubiImm16R1R2>(t, Sizes::LEN32BIT);
            default:
              return std::nullopt;
          }
        }
      case 0b1101:
        switch (const auto opcode2 = ExtractTypeVIOpcode(opcode); opcode2) {
          case Opcodes::OP_VI_6BIT_ADDI:
            return std::make_unique<AddiImm32R1R2>(t, Sizes::LEN32BIT);
          case Opcodes::OP_VI_6BIT_MOVEA_OR_MOV:
            if (const auto reg2 = ExtractReg2OpcodeField(opcode);
                reg2 == Registers::R0) {
              return std::make_unique<MovImm32R1>(t, Sizes::LEN48BIT);
            }
            return std::make_unique<MoveaImm16R1R2>(t, Sizes::LEN32BIT);
          case Opcodes::OP_VI_6BIT_ORI:
            return std::make_unique<OriImm16R1R2>(t, Sizes::LEN32BIT);
          case Opcodes::OP_VI_6BIT_XORI:
            return std::make_unique<XoriImm16R1R2>(t, Sizes::LEN32BIT);
          case Opcodes::OP_VI_6BIT_ANDI:
            return std::make_unique<AndiImm16R1R2>(t, Sizes::LEN32BIT);
          case Opcodes::OP_VI_6BIT_MULHI:
            return std::make_unique<MulhiImm16R1R2>(t, Sizes::LEN32BIT);
          default:
            return std::nullopt;
        }
      case 0b1110: {  // new scope needed because we declare variables
        const auto opcode2 = ExtractTypeVIIOpcode(opcode);
        const auto subop = ExtractTypeVIISubop(opcode);
        switch (opcode2) {
          case Opcodes::OP_VII_6BIT_LD_B:
            return std::make_unique<LdbDisp16R1R2>(t, Sizes::LEN32BIT);

          case Opcodes::OP_VII_6BIT_LD_H_OR_LD_W:
            if (subop == Opcodes::SUBOP_VII_LD_H) {
              return std::make_unique<LdhDisp16R1R2>(t, Sizes::LEN32BIT);
            }
            if (subop == Opcodes::SUBOP_VII_LD_W) {
              return std::make_unique<LdwDisp16R1R2>(t, Sizes::LEN32BIT);
            }
          case Opcodes::OP_VII_6BIT_ST_B:
            return std::make_unique<StbR2Disp16R1>(t, Sizes::LEN32BIT);
          case Opcodes::OP_VII_6BIT_ST_H_OR_ST_W:
            if (subop == Opcodes::SUBOP_VII_ST_H) {
              return std::make_unique<SthR2Disp26R1>(t, Sizes::LEN32BIT);
            }
            if (subop == Opcodes::SUBOP_VII_ST_W) {
              return std::make_unique<StwR2Disp16R1>(t, Sizes::LEN32BIT);
            }
          default:
            return std::nullopt;
        }
      }
      default:
        return std::nullopt;
    }
  }
  switch (op4bit) {
    case Opcodes::OP_III_4BIT_BCOND:
      /* Determine branch instruction based on the condition code */
      switch (const auto condition = ExtractTypeIIIBranchCond(opcode);
              condition) {
        case Conditions::CONDITION_CODE_BGT:
          return std::make_unique<Bgt>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BGE:
          return std::make_unique<Bge>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BLT:
          return std::make_unique<Blt>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BLE:
          return std::make_unique<Ble>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BH:
          return std::make_unique<Bh>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BNH:
          return std::make_unique<Bnh>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BV:
          return std::make_unique<Bv>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BNV:
          return std::make_unique<Bnv>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BN:
          return std::make_unique<Bn>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BP:
          return std::make_unique<Bp>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BC:
          return std::make_unique<Bc>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BNC:
          return std::make_unique<Bnc>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BZ:
          return std::make_unique<Bz>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BNZ:
          return std::make_unique<Bnz>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BR:
          return std::make_unique<Br>(t, Sizes::LEN16BIT);
        case Conditions::CONDITION_CODE_BSA:
          return std::make_unique<Bsa>(t, Sizes::LEN16BIT);
        default:
          return std::nullopt;
      }
    case Opcodes::OP_IV_4BIT_SLD_H:
      return std::make_unique<SldhDisp8R2>(t, Sizes::LEN16BIT);
    case Opcodes::OP_IV_4BIT_SST_H:
      return std::make_unique<SsthR2Disp8>(t, Sizes::LEN16BIT);
    case Opcodes::OP_IV_4BIT_SLD_W_OR_SST_W:
      if (const auto subop = ExtractTypeIVSubop(opcode);
          subop == Opcodes::SUBOP_IV_SLD_W) {
        return std::make_unique<SldwDisp8R2>(t, Sizes::LEN16BIT);
      } else if (subop == Opcodes::SUBOP_IV_SST_W) {
        return std::make_unique<SstwR2Disp8>(t, Sizes::LEN16BIT);
      }
      return std::nullopt;
    default:  // Opcode not matched
      return std::nullopt;
  }
  return std::nullopt;
}

std::optional<std::unique_ptr<Instruction>> ParsePrefix0b01(
    const IsaType &t, const uint16_t opcode) {
  if (opcode & OpcodeFields::OPCODE_BIT_3) {
    if (opcode & OpcodeFields::OPCODE_BIT_4) {
      return std::make_unique<SstbR2Disp7>(t, Sizes::LEN16BIT);
    }
    return std::make_unique<SldbDisp7R2>(t, Sizes::LEN16BIT);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_4) {
    if (opcode & OpcodeFields::OPCODE_BIT_5) {
      if (opcode & OpcodeFields::OPCODE_BIT_6) {
        return std::make_unique<MulhImm5R2>(t, Sizes::LEN16BIT);
      }
      return std::make_unique<ShlImm5R2>(t, Sizes::LEN16BIT);
    }
    if (opcode & OpcodeFields::OPCODE_BIT_6) {
      return std::make_unique<SarImm5R2>(t, Sizes::LEN16BIT);
    }
    return std::make_unique<ShrImm5R2>(t, Sizes::LEN16BIT);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_5) {
    if (opcode & OpcodeFields::OPCODE_BIT_6) {
      return std::make_unique<CmpImm5R2>(t, Sizes::LEN16BIT);
    }
    return std::make_unique<AddImm5R2>(t, Sizes::LEN16BIT);
  }
  if (const auto reg2 = ExtractReg2OpcodeField(opcode); reg2 == Registers::R0) {
    return std::make_unique<CalltImm6>(t, Sizes::LEN16BIT);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_6) {
    return std::make_unique<SataddImm5>(t, Sizes::LEN16BIT);
  }
  return std::make_unique<MovImm5R2>(t, Sizes::LEN16BIT);
}

std::optional<std::unique_ptr<Instruction>> ParsePrefix0b001(
    const IsaType &t, const uint16_t opcode) {
  if (opcode & OpcodeFields::OPCODE_BIT_4) {
    if (opcode & OpcodeFields::OPCODE_BIT_5) {
      if (opcode & OpcodeFields::OPCODE_BIT_6) {
        return std::make_unique<CmpR1R2>(t, Sizes::LEN16BIT);
      }
      return std::make_unique<AddR1R2>(t, Sizes::LEN16BIT);
    }
    if (opcode & OpcodeFields::OPCODE_BIT_6) {
      return std::make_unique<SubR1R2>(t, Sizes::LEN16BIT);
    }
    return std::make_unique<SubrR1R2>(t, Sizes::LEN16BIT);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_5) {
    if (opcode & OpcodeFields::OPCODE_BIT_6) {
      return std::make_unique<TstR1R2>(t, Sizes::LEN16BIT);
    }
    return std::make_unique<AndR1R2>(t, Sizes::LEN16BIT);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_6) {
    return std::make_unique<XorR1R2>(t, Sizes::LEN16BIT);
  }
  return std::make_unique<OrR1R2>(t, Sizes::LEN16BIT);
}

std::optional<std::unique_ptr<Instruction>> ParsePrefix0b0001(
    const IsaType &t, const uint16_t opcode) {
  if (opcode & OpcodeFields::OPCODE_BIT_5) {
    if (opcode & OpcodeFields::OPCODE_BIT_6) {
      if (const auto reg2 = ExtractReg2OpcodeField(opcode);
          reg2 == Registers::R0) {
        return std::make_unique<SxhR1>(t, Sizes::LEN16BIT);
      }
      return std::make_unique<MulhR1R2>(t, Sizes::LEN16BIT);
    }
    if (const auto reg2 = ExtractReg2OpcodeField(opcode);
        reg2 == Registers::R0) {
      return std::make_unique<ZxhR1>(t, Sizes::LEN16BIT);
    }
    return std::make_unique<SataddR1R2>(t, Sizes::LEN16BIT);
  }
  if (opcode & OpcodeFields::OPCODE_BIT_6) {
    if (const auto reg2 = ExtractReg2OpcodeField(opcode);
        reg2 == Registers::R0) {
      return std::make_unique<SxbR1>(t, Sizes::LEN16BIT);
    }
    return std::make_unique<SatsubR1R2>(t, Sizes::LEN16BIT);
  }
  if (const auto reg2 = ExtractReg2OpcodeField(opcode); reg2 == Registers::R0) {
    return std::make_unique<ZxbR1>(t, Sizes::LEN16BIT);
  }
  return std::make_unique<SatsubrR1R2>(t, Sizes::LEN16BIT);
}

std::optional<std::unique_ptr<Instruction>> ParsePrefix0b00001(
    const IsaType &t, const uint16_t opcode) {
  if (opcode & OpcodeFields::OPCODE_BIT_6) {
    // Opcode 0b000011; jmp, sld.hu, sld.bu
    if (const auto reg2 = ExtractReg2OpcodeField(opcode);
        reg2 == Registers::R0) {
      return std::make_unique<JmpR1>(t, Sizes::LEN16BIT);
    }
    if (const auto opcode_7bit = Extract7BitOpcode(opcode);
        opcode_7bit == Opcodes::OP_IV_SLD_BU) {
      return std::make_unique<SldbuDisp4R2>(t, Sizes::LEN16BIT);
    } else if (opcode_7bit == Opcodes::OP_IV_SLD_HU) {
      return std::make_unique<SldhuDisp5R2>(t, Sizes::LEN16BIT);
    }
    return std::nullopt;
  }
  // Opcode 0b000010; switch, dbtrap, divh
  if (const auto reg2 = ExtractReg2OpcodeField(opcode); reg2 == Registers::R0) {
    return std::make_unique<SwitchR1>(t, Sizes::LEN16BIT);
  }
  if (opcode == Opcodes::EXACT_OP_I_DBTRAP) {
    return std::make_unique<Dbtrap>(t, Sizes::LEN16BIT);
  }
  return std::make_unique<DivhR1R2>(t, Sizes::LEN16BIT);
}

std::optional<std::unique_ptr<Instruction>> ParsePrefix0b000001(
    const IsaType &t, const uint16_t opcode) {
  return std::make_unique<NotR1R2>(t, Sizes::LEN16BIT);
}
}  // namespace V850