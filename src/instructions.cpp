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

FpuSingle::FpuSingle(const IsaType &t, const uint8_t len, const FpuOp op)
    : Instruction(t, len), op(op) {}

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
BinsR1PosWidthR2::BinsR1PosWidthR2(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
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
SyncBarrier::SyncBarrier(const IsaType &t, const uint8_t len,
                         const char *mnemonic_, const char *intrinsic_)
    : Instruction(t, len), mnemonic(mnemonic_), intrinsic(intrinsic_) {}
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
MacR1R2R3R4::MacR1R2R3R4(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
MacuR1R2R3R4::MacuR1R2R3R4(const IsaType &t, const uint8_t len)
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
Eiret::Eiret(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Feret::Feret(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
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
PushspRhRt::PushspRhRt(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
PopspRhRt::PopspRhRt(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
CaxiR1R2R3::CaxiR1R2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
JarlR1R3::JarlR1R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Snooze::Snooze(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
RieI::RieI(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
RieX::RieX(const IsaType &t, const uint8_t len) : Instruction(t, len) {}
Sch0lR2R3::Sch0lR2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Sch0rR2R3::Sch0rR2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Sch1lR2R3::Sch1lR2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}
Sch1rR2R3::Sch1rR2R3(const IsaType &t, const uint8_t len)
    : Instruction(t, len) {}

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
  // Format I synchronization family (G3MH Software Manual pp. 287-290).
  // These share the ParsePrefix0b0 path because their top 11 bits are all 0
  // (reg2 field = 0, opcode field = 0) -- only the low 5 bits differ.
  switch (opcode) {
    case Opcodes::EXACT_OP_I_SYNCI:
      return std::make_unique<SyncBarrier>(t, Sizes::LEN16BIT, "synci",
                                           "v850.synci");
    case Opcodes::EXACT_OP_I_SYNCE:
      return std::make_unique<SyncBarrier>(t, Sizes::LEN16BIT, "synce",
                                           "v850.synce");
    case Opcodes::EXACT_OP_I_SYNCM:
      return std::make_unique<SyncBarrier>(t, Sizes::LEN16BIT, "syncm",
                                           "v850.syncm");
    case Opcodes::EXACT_OP_I_SYNCP:
      return std::make_unique<SyncBarrier>(t, Sizes::LEN16BIT, "syncp",
                                           "v850.syncp");
    default:
      break;
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
      /* V850E3 / RH850 G3MH gap-fill decodes.
       * These must be checked before falling into the legacy extended-format
       * arms, because the existing code was written for the V850E1 subset and
       * can otherwise misclassify these encodings. */
      {
        const uint16_t word2_low11 =
            static_cast<uint16_t>((opcode >> 16) & 0x07FF);
        const auto reg2_field = ExtractReg2OpcodeField(opcode);
        const auto reg1_field = ExtractReg1OpcodeField(opcode);

        // PUSHSP / POPSP / JARL [reg1], reg3 share Format XI sub-opcode
        // 00101100000 (low 11 bits of word2). Differentiated by word1 reg2.
        if (word2_low11 == 0b00101100000) {
          if (reg2_field == 0b01000) {
            return std::make_unique<PushspRhRt>(t, Sizes::LEN32BIT);
          }
          if (reg2_field == 0b01100) {
            return std::make_unique<PopspRhRt>(t, Sizes::LEN32BIT);
          }
          if (reg2_field == 0b11000) {
            return std::make_unique<JarlR1R3>(t, Sizes::LEN32BIT);
          }
        }

        // CAXI [reg1], reg2, reg3 : word2 low 11 = 00011101110
        if (word2_low11 == 0b00011101110) {
          return std::make_unique<CaxiR1R2R3>(t, Sizes::LEN32BIT);
        }

        // SCH0R / SCH0L / SCH1R / SCH1L share word2 bits[10:3] = 01101100,
        // with bits[2:0] selecting the variant (see G3MH pp.251-254).
        // reg2 field (source) uses normal word1 reg2; reg1 field must be 0.
        if ((word2_low11 & 0b11111111000) == 0b01101100000 &&
            reg1_field == 0) {
          switch (word2_low11 & 0b111) {
            case 0b000:
              return std::make_unique<Sch0rR2R3>(t, Sizes::LEN32BIT);
            case 0b010:
              return std::make_unique<Sch1rR2R3>(t, Sizes::LEN32BIT);
            case 0b100:
              return std::make_unique<Sch0lR2R3>(t, Sizes::LEN32BIT);
            case 0b110:
              return std::make_unique<Sch1lR2R3>(t, Sizes::LEN32BIT);
            default:
              break;
          }
        }

        // SNOOZE: word1 = 0000111111100000, word2 = 0000000100100000.
        // Word1 reg2 = 00001 distinguishes from HALT (reg2 = 00000).
        if (reg2_field == 0b00001 && reg1_field == 0 &&
            (opcode >> 16 & 0xFFFF) == Opcodes::OP_X_HALT) {
          return std::make_unique<Snooze>(t, Sizes::LEN32BIT);
        }

        // RIE (Format X): word1 = iiiii 1111111 IIII, word2 = 0x0000.
        // Distinguished from SETF by bit 4 of word1 (LSB of reg1 field).
        // SETF has that bit = 0, RIE has it = 1.
        if ((opcode >> 16 & 0xFFFF) == 0 && (reg1_field & 0b10000)) {
          return std::make_unique<RieX>(t, Sizes::LEN32BIT);
        }
      }

      if ((opcode >> 16 & OpcodeFields::MASK_VII_SUBOP) ==
          Opcodes::SUBOP_VII_LD_BU_HU) {
        return std::make_unique<LdhuDisp16R1R2>(t, Sizes::LEN32BIT);
      }
      // Format IX: BINS (bitfield insert). G3MH p.162.
      // Sub-opcode (word2 bits 5..10) is one of 001001 / 001011 / 001101.
      // These collide on sub-opcode bits with Format X specials
      // (HALT=001001, EI/DI=001011) whose regs are all zero. BINS with
      // any non-zero reg/field gets routed here first.
      {
        const auto bins_subop =
            (opcode >> 16 & OpcodeFields::MASK_IX_SUBOP_BINS) >>
            OpcodeFields::SHIFT_IX_SUBOP_BINS;
        const bool bins_subop_match =
            (bins_subop == Opcodes::SUBOP_IX_BINS_HI ||
             bins_subop == Opcodes::SUBOP_IX_BINS_MID ||
             bins_subop == Opcodes::SUBOP_IX_BINS_LO);
        if (bins_subop_match) {
          const auto reg1 = ExtractReg1OpcodeField(opcode);
          const auto reg2 = ExtractReg2OpcodeField(opcode);
          const auto mmmm = (opcode >> 16 & OpcodeFields::MASK_IX_BINS_MMMM) >>
                            OpcodeFields::SHIFT_IX_BINS_MMMM;
          const auto k = (opcode >> 16 & OpcodeFields::MASK_IX_BINS_K) >>
                         OpcodeFields::SHIFT_IX_BINS_K;
          const auto lll = (opcode >> 16 & OpcodeFields::MASK_IX_BINS_LLL) >>
                           OpcodeFields::SHIFT_IX_BINS_LLL;
          if (reg1 != 0 || reg2 != 0 || mmmm != 0 || k != 0 || lll != 0) {
            return std::make_unique<BinsR1PosWidthR2>(t, Sizes::LEN32BIT);
          }
        }
      }

      /* Single-precision FPU (Format F:I): category bit 2 of HW2 == 1
         (OPCODE_BIT_1 in the HW2-shifted projection). Dispatch before the
         existing MUL/DIV/CMOV tree, which only handles category=0b000..0b011. */
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_1) {
        if (auto f = ParseFpuSingle(t, opcode)) {
          return f;
        }
        return std::nullopt;
      }
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_2) {
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_3) {
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_5) {
            // bit23 (OPCODE_BIT_4) distinguishes BSW/BSH/HSW (0) from MAC/MACU (1)
            if (opcode >> 16 & OpcodeFields::OPCODE_BIT_4) {
              // 011110 = MAC, 011111 = MACU (bit21 / OPCODE_BIT_6)
              // G3MH Software Manual p. 215-216
              if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {
                return std::make_unique<MacuR1R2R3R4>(t, Sizes::LEN32BIT);
              }
              return std::make_unique<MacR1R2R3R4>(t, Sizes::LEN32BIT);
            }
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
            case Opcodes::SUBOP_X_RETI: {
              // RH850 G3MH split RETI -> EIRET/FERET. Distinguish by bits
              // that were reserved-zero in original V850:
              //   RETI  = 0x0140 (low byte 0x40)
              //   EIRET = 0x0148 (bit 3 set)
              //   FERET = 0x014A (bit 3 and bit 1 set)
              uint16_t w2 = static_cast<uint16_t>(opcode >> 16);
              if ((w2 & 0x0A) == 0x0A) {
                return std::make_unique<Feret>(t, Sizes::LEN32BIT);
              }
              if (w2 & 0x08) {
                return std::make_unique<Eiret>(t, Sizes::LEN32BIT);
              }
              return std::make_unique<Reti>(t, Sizes::LEN32BIT);
            }
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
  // RIE (Format I): exact 16-bit pattern 0000000001000000 (G3MH p.239).
  // Reuses the NOT r0,r0 encoding slot.
  if (opcode == 0b0000000001000000) {
    return std::make_unique<RieI>(t, Sizes::LEN16BIT);
  }
  return std::make_unique<NotR1R2>(t, Sizes::LEN16BIT);
}

/* -------------------------------------------------------------------------- *
 *  Single-precision FPU decode (Format F:I).
 *  Precondition: HW1[10..5] == 0b111111 && HW2[10] == 1 (category bit 2 set).
 *
 *  HW2 layout: [w:5][category:3][type:2][subop:6]
 *              bits 15..11   10..8        7..6      5..0  (within HW2)
 *              bits 31..27   26..24       23..22    21..16 (full opcode)
 *
 *  For cat=0b100 / type=0b01 (the common path), R (= reg1 field in HW1,
 *  bits 4..0 of the 32-bit opcode) acts as an instruction selector for
 *  the unary ops; for arithmetic three-operand ops R is the source reg.
 * -------------------------------------------------------------------------- */
std::optional<std::unique_ptr<Instruction>> ParseFpuSingle(
    const IsaType &t, const uint32_t opcode) {
  const auto hw2 = static_cast<uint16_t>(opcode >> 16);
  const auto category = static_cast<uint8_t>((hw2 >> 8) & 0b111);
  const auto type = static_cast<uint8_t>((hw2 >> 6) & 0b11);
  const auto subop = static_cast<uint8_t>(hw2 & 0b111111);
  const auto R = static_cast<uint8_t>(opcode & 0b11111);

  if (category != 0b100) {
    return std::nullopt;
  }

  auto mk = [&t](FpuOp op) {
    return std::make_unique<FpuSingle>(t, Sizes::LEN32BIT, op);
  };

  if (type == 0b00) {
    /* CMOVF.S: subop == 0_fff_0 (bit 16 = 0, bits 21..17 = 0_fff),
       disambiguated from other cat=100/type=00 noise by subop bit5=0 and
       bit0=0. Accept any subop matching 0b0???0 with bits 5=0. */
    if ((subop & 0b100001) == 0) {
      return mk(FpuOp::CmovfS);
    }
    return std::nullopt;
  }

  if (type == 0b11) {
    /* FMA family -- reg1 is a source, not a selector. */
    switch (subop) {
      case 0b100000:
        return mk(FpuOp::FmafS);
      case 0b100010:
        return mk(FpuOp::FmsfS);
      case 0b100100:
        return mk(FpuOp::FnmafS);
      case 0b100110:
        return mk(FpuOp::FnmsfS);
      default:
        return std::nullopt;
    }
  }

  if (type != 0b01) {
    return std::nullopt;
  }

  /* type == 0b01: arithmetic, unary, conversion, and CMPF.S/TRFSR live here.
     subop bit5 (= bit 21) distinguishes arithmetic (1) from unary/cvt (0). */
  if (subop & 0b100000) {
    /* Arithmetic three-operand (R is source reg1). */
    switch (subop) {
      case 0b100000:
        return mk(FpuOp::AddfS);
      case 0b100010:
        return mk(FpuOp::SubfS);
      case 0b100100:
        return mk(FpuOp::MulfS);
      case 0b101110:
        return mk(FpuOp::DivfS);
      case 0b101000:
        return mk(FpuOp::MaxfS);
      case 0b101010:
        return mk(FpuOp::MinfS);
      default:
        return std::nullopt;
    }
  }

  /* CMPF.S shares type=01 but uses the reg3 field to carry fcond and has
     subop pattern 0_fff_0 (bit 5=0, bit 0=0). Distinguish from the unary/cvt
     group (which uses subop in {000000, 000010, 010000, 011100}) by checking
     that subop doesn't match any of those templates and bit 0 of subop == 0. */
  if ((subop & 0b100001) == 0 && (subop & 0b011110) != 0b000000 &&
      (subop & 0b011110) != 0b000010 && (subop & 0b011110) != 0b010000 &&
      (subop & 0b011110) != 0b011100) {
    /* TRFSR: reg2 == 0, reg1 == 0 (R == 0). Otherwise CMPF.S. */
    const auto reg2 = static_cast<uint8_t>(hw2 >> 11);
    if (reg2 == 0 && R == 0) {
      return mk(FpuOp::Trfsr);
    }
    return mk(FpuOp::CmpfS);
  }

  /* Unary / conversion ops: keyed on (subop, R). */
  switch (subop) {
    case 0b010000:  // ABSF.S / NEGF.S
      switch (R) {
        case 0:
          return mk(FpuOp::AbsfS);
        case 1:
          return mk(FpuOp::NegfS);
        default:
          return std::nullopt;
      }
    case 0b011100:  // SQRTF.S / RECIPF.S / RSQRTF.S
      switch (R) {
        case 0:
          return mk(FpuOp::SqrtfS);
        case 1:
          return mk(FpuOp::RecipfS);
        case 2:
          return mk(FpuOp::RsqrtfS);
        default:
          return std::nullopt;
      }
    case 0b000000:  // floor/ceil/trunc/round/cvt -> word
      switch (R) {
        case 0:
          return mk(FpuOp::RoundfSw);
        case 1:
          return mk(FpuOp::TrncfSw);
        case 2:
          return mk(FpuOp::CeilfSw);
        case 3:
          return mk(FpuOp::FloorfSw);
        case 4:
          return mk(FpuOp::CvtfSw);
        case 16:
          return mk(FpuOp::RoundfSuw);
        case 17:
          return mk(FpuOp::TrncfSuw);
        case 18:
          return mk(FpuOp::CeilfSuw);
        case 19:
          return mk(FpuOp::FloorfSuw);
        case 20:
          return mk(FpuOp::CvtfSuw);
        default:
          return std::nullopt;
      }
    case 0b000010:  // word/half -> single, single -> half
      switch (R) {
        case 0:
          return mk(FpuOp::CvtfWs);
        case 2:
          return mk(FpuOp::CvtfHs);
        case 3:
          return mk(FpuOp::CvtfSh);
        case 16:
          return mk(FpuOp::CvtfUws);
        default:
          return std::nullopt;
      }
    default:
      return std::nullopt;
  }
}
}  // namespace V850