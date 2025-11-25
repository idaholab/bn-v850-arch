// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#include "architecture.h"

#include <binaryninjaapi.h>

#include <cstring>

#include "flags.h"
#include "instructions.h"
#include "registers.h"
#include "sizes.h"
#include "util.h"

namespace BN = BinaryNinja;
namespace V850 {

V850Architecture::V850Architecture(const std::string &name)
    : Architecture(name), isa_type() {}

BNRegisterInfo V850Architecture::RegisterInfo(const uint32_t fullWidthReg,
                                              const size_t offset,
                                              const size_t size,
                                              const bool zeroExtend) {
  BNRegisterInfo result{};
  result.fullWidthRegister = fullWidthReg;
  result.offset = offset;  // in bytes
  result.size = size;
  result.extend = zeroExtend ? ZeroExtendToFullWidth : NoExtend;
  return result;
}

[[nodiscard]] size_t V850Architecture::GetAddressSize() const {
  return Sizes::LEN32BIT;
}

[[nodiscard]] BNEndianness V850Architecture::GetEndianness() const {
  return LittleEndian;
}

[[nodiscard]] size_t V850Architecture::GetDefaultIntegerSize() const {
  return Sizes::LEN32BIT;
}

[[nodiscard]] size_t V850Architecture::GetInstructionAlignment() const {
  return Sizes::LEN16BIT;
}

[[nodiscard]] size_t V850Architecture::GetMaxInstructionLength() const {
  return Sizes::LEN64BIT;
}

[[nodiscard]] std::string V850Architecture::GetRegisterName(
    const uint32_t rid) {
  /* note: register name mapping function is defined in util.cpp */
  const char *result = RegToStr(rid);
  if (result == nullptr) {
    result = "GetRegisterName: INVALID_REG_ID";
  }
  return result;
}

[[nodiscard]] uint32_t V850Architecture::GetLinkRegister() {
  return Registers::R31;
}

[[nodiscard]] uint32_t V850Architecture::GetStackPointerRegister() {
  return Registers::SP;
}

bool V850Architecture::GetInstructionInfo(const uint8_t *data,
                                          const uint64_t addr,
                                          const size_t maxLen,
                                          BN::InstructionInfo &result) {
  // Instructions are a minimum of 16 bits
  if (maxLen < Sizes::LEN16BIT) {
    return false;
  }

  const auto opcode = reinterpret_cast<const uint32_t *>(data);

  if (const auto i = DecodeInstruction(isa_type, *opcode)) {
    if (i->get()->GetInstrLen() <= Sizes::LEN32BIT) {
      const auto instr_data = reinterpret_cast<const uint32_t *>(data);
      return i->get()->Info(*instr_data, addr, result);
    }
    const auto instr_data = reinterpret_cast<const uint64_t *>(data);
    return i->get()->Info(*instr_data, addr, result);
  }

  return false;
}

bool V850Architecture::GetInstructionText(
    const uint8_t *data, const uint64_t addr, size_t &len,
    std::vector<BN::InstructionTextToken> &result) {
  const auto opcode = reinterpret_cast<const uint32_t *>(data);

  if (const auto i = DecodeInstruction(isa_type, *opcode)) {
    if (i->get()->GetInstrLen() <= Sizes::LEN32BIT) {
      const auto instr_data = reinterpret_cast<const uint32_t *>(data);
      return i->get()->Text(*instr_data, addr, len, result);
    }
    const auto instr_data = reinterpret_cast<const uint64_t *>(data);
    return i->get()->Text(*instr_data, addr, len, result);
  }

  return false;
}

bool V850Architecture::GetInstructionLowLevelIL(const uint8_t *data,
                                                const uint64_t addr,
                                                size_t &len,
                                                BN::LowLevelILFunction &il) {
  const auto opcode = reinterpret_cast<const uint32_t *>(data);

  if (const auto i = DecodeInstruction(isa_type, *opcode)) {
    if (i->get()->GetInstrLen() <= Sizes::LEN32BIT) {
      const auto instr_data = reinterpret_cast<const uint32_t *>(data);
      return i->get()->Lift(*instr_data, addr, len, il, this);
    }
    const auto instr_data = reinterpret_cast<const uint64_t *>(data);
    return i->get()->Lift(*instr_data, addr, len, il, this);
  }

  return false;
}

V850E1Architecture::V850E1Architecture(const std::string &name)
    : V850Architecture(name) {
  this->isa_type = V850_E1_ISA;
}

std::vector<uint32_t> V850E1Architecture::GetAllRegisters() {
  return std::vector<uint32_t>{
      Registers::R0,  Registers::R1,  Registers::R2,
      Registers::SP,  Registers::R4,  Registers::R5,
      Registers::R6,  Registers::R7,  Registers::R8,
      Registers::R9,  Registers::R10, Registers::R11,
      Registers::R12, Registers::R13, Registers::R14,
      Registers::R15, Registers::R16, Registers::R17,
      Registers::R18, Registers::R19, Registers::R20,
      Registers::R21, Registers::R22, Registers::R23,
      Registers::R24, Registers::R25, Registers::R26,
      Registers::R27, Registers::R28, Registers::R29,
      Registers::EP,  Registers::R31
      /* NOTE: system registers are represented using a base address and
         offset */
  };
}

BNRegisterInfo V850E1Architecture::GetRegisterInfo(const uint32_t rid) {
  std::vector<uint32_t> registers = GetAllRegisters();
  if (rid <= Registers::R31) {
    return RegisterInfo(rid, 0,
                        Sizes::LEN32BIT);  // struct contains: full width reg,
    // offset (for sub-registers), size
  }
  // TODO, also add support for float regs
  return RegisterInfo(0, 0, 0);
}

std::vector<uint32_t> V850E1Architecture::GetAllFlags() {
  return std::vector<uint32_t>{Flags::FLAG_Z_ZERO,
                               Flags::FLAG_S_SIGN,
                               Flags::FLAG_OV_OVERFLOW,
                               Flags::FLAG_CY_CARRY,
                               Flags::FLAG_SAT_SATURATED,
                               Flags::FLAG_ID_INTERRUPT_DISABLE,
                               Flags::FLAG_EP_EXCEPTION_PENDING,
                               Flags::FLAG_NP_NMI_PENDING};
}

std::string V850E1Architecture::GetFlagName(const uint32_t flag) {
  /* note: flag name mapping function is defined in util.cpp */
  const char *result = FlagToStr(flag);
  if (result == nullptr) {
    result = "GetFlagName: INVALID_FLAG_ID";
  }
  return result;
}

BNFlagRole V850E1Architecture::GetFlagRole(const uint32_t flag,
                                           uint32_t semClass) {
  switch (flag) {
    case Flags::FLAG_Z_ZERO:
      return ZeroFlagRole;
    case Flags::FLAG_S_SIGN:
      return NegativeSignFlagRole;
    case Flags::FLAG_OV_OVERFLOW:
      return OverflowFlagRole;
    case Flags::FLAG_CY_CARRY:
      return CarryFlagRole;
    // TODO define all the SpecialFlagRole roles
    case Flags::FLAG_SAT_SATURATED:
    case Flags::FLAG_ID_INTERRUPT_DISABLE:
    case Flags::FLAG_EP_EXCEPTION_PENDING:
    case Flags::FLAG_NP_NMI_PENDING:
    default:
      return SpecialFlagRole;
  }
}

std::vector<uint32_t> V850E1Architecture::GetFlagsWrittenByFlagWriteType(
    const uint32_t flags) {
  switch (flags) {
    case Flags::FLAGS_WRITE_Z:
      return std::vector<uint32_t>{Flags::FLAG_Z_ZERO};
    case Flags::FLAGS_WRITE_S_Z:
      return std::vector<uint32_t>{Flags::FLAG_S_SIGN, Flags::FLAG_Z_ZERO};
    case Flags::FLAGS_WRITE_OV_S_Z:
      return std::vector<uint32_t>{Flags::FLAG_OV_OVERFLOW, Flags::FLAG_S_SIGN,
                                   Flags::FLAG_Z_ZERO};
    case Flags::FLAGS_WRITE_CY_OV_S_Z:
      return std::vector<uint32_t>{Flags::FLAG_CY_CARRY,
                                   Flags::FLAG_OV_OVERFLOW, Flags::FLAG_S_SIGN,
                                   Flags::FLAG_Z_ZERO};
    case Flags::FLAGS_WRITE_SAT_CY_OV_S_Z:
      return std::vector<uint32_t>{
          Flags::FLAG_SAT_SATURATED, Flags::FLAG_CY_CARRY,
          Flags::FLAG_OV_OVERFLOW, Flags::FLAG_S_SIGN, Flags::FLAG_Z_ZERO};
    case Flags::FLAGS_WRITE_ID:
      return std::vector<uint32_t>{Flags::FLAG_ID_INTERRUPT_DISABLE};
    default:
      return std::vector<uint32_t>{};
  }
}

std::string V850E1Architecture::GetFlagWriteTypeName(const uint32_t flags) {
  switch (flags) {
    case Flags::FLAGS_WRITE_Z:
      return "z";
    case Flags::FLAGS_WRITE_S_Z:
      return "sz";
    case Flags::FLAGS_WRITE_OV_S_Z:
      return "ovsz";
    case Flags::FLAGS_WRITE_CY_OV_S_Z:
      return "*";
    case Flags::FLAGS_WRITE_SAT_CY_OV_S_Z:
      return "sat*";
    case Flags::FLAGS_WRITE_ID:
      return "id";
    default:
      return "GetFlagWriteTypeName: flag write type undefined";
  }
}

std::vector<uint32_t> V850E1Architecture::GetFlagsRequiredForFlagCondition(
    const BNLowLevelILFlagCondition cond, uint32_t semClass) {
  switch (cond) {
    case LLFC_E:
    case LLFC_NE:
      return std::vector<uint32_t>{Flags::FLAG_Z_ZERO};
    case LLFC_NEG:
    case LLFC_POS:
      return std::vector<uint32_t>{Flags::FLAG_S_SIGN};
    case LLFC_O:
    case LLFC_NO:
      return std::vector<uint32_t>{Flags::FLAG_OV_OVERFLOW};
    case LLFC_ULT:
    case LLFC_UGE:
      return std::vector<uint32_t>{Flags::FLAG_CY_CARRY};
    case LLFC_ULE:
    case LLFC_UGT:
      return std::vector<uint32_t>{Flags::FLAG_Z_ZERO, Flags::FLAG_CY_CARRY};
    case LLFC_SGE:
    case LLFC_SLT:
      return std::vector<uint32_t>{Flags::FLAG_S_SIGN, Flags::FLAG_OV_OVERFLOW};
    case LLFC_SLE:
    case LLFC_SGT:
      return std::vector<uint32_t>{Flags::FLAG_Z_ZERO, Flags::FLAG_S_SIGN,
                                   Flags::FLAG_OV_OVERFLOW};
    // TODO how deal with SAT?
    // TODO float comparisons. Are these flags even needed?
    case LLFC_FE:
    case LLFC_FNE:
    case LLFC_FLT:
    case LLFC_FLE:
    case LLFC_FGE:
    case LLFC_FGT:
    case LLFC_FO:
    case LLFC_FUO:
    default:
      return std::vector<uint32_t>{};
  }
}

class V850E1CallingConvention final : public BN::CallingConvention {
 public:
  explicit V850E1CallingConvention(BN::Architecture *arch)
      : CallingConvention(arch, "V850E1") {}

  uint32_t GetGlobalPointerRegister() override { return Registers::R4; }

  std::vector<uint32_t> GetIntegerArgumentRegisters() override {
    return std::vector<uint32_t>{Registers::R6, Registers::R7, Registers::R9,
                                 Registers::R9};
  }

  uint32_t GetIntegerReturnValueRegister() override { return Registers::R10; }

  uint32_t GetHighIntegerReturnValueRegister() override {
    return Registers::R11;
  }

  std::vector<uint32_t> GetCalleeSavedRegisters() override {
    return std::vector<uint32_t>{
        Registers::R20, Registers::R21, Registers::R22, Registers::R23,
        Registers::R24, Registers::R25, Registers::R26, Registers::R27,
        Registers::R28, Registers::R29, Registers::EP,  Registers::R31};
  }

  std::vector<uint32_t> GetCallerSavedRegisters() override {
    return std::vector<uint32_t>{Registers::R10, Registers::R11, Registers::R12,
                                 Registers::R13, Registers::R14, Registers::R15,
                                 Registers::R16, Registers::R17, Registers::R18,
                                 Registers::R19};
  }
};
}  // namespace V850

extern "C" {
BN_DECLARE_CORE_ABI_VERSION
BINARYNINJAPLUGIN bool CorePluginInit() {
  BN::Architecture *V850E1 = new V850::V850E1Architecture("V850");
  BN::Architecture::Register(V850E1);

  const BN::Ref<BN::CallingConvention> cc =
      new V850::V850E1CallingConvention(V850E1);
  V850E1->RegisterCallingConvention(cc);
  V850E1->SetDefaultCallingConvention(cc);

  return true;
}
}