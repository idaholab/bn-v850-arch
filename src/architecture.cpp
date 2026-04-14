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
  if (rid <= Registers::R31) {
    const char *result = RegToStr(static_cast<uint8_t>(rid));
    if (result != nullptr) return result;
  }
  /* RH850 banked system register handles (see registers.h). */
  if (Registers::IsSysregHandle(rid)) {
    const char *result = SysregHandleToStr(rid);
    if (result != nullptr) return result;
  }
  return "GetRegisterName: INVALID_REG_ID";
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
  std::vector<uint32_t> regs{
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
      Registers::EP,  Registers::R31,
      Registers::FPSR};

  /* Advertise the full banked system register file (selID 0..7 x regID
   * 0..31) so stsr/ldsr lifts can emit il.Register / il.SetRegister on
   * any documented (regID, selID) pair without producing INVALID_REG_ID
   * warnings. Names for undocumented banks fall back to sr<r>_<s> via
   * util::SystemRegToStrBanked. selID > 7 is reserved on G3MH and is
   * omitted to avoid polluting Binary Ninja's register view. */
  for (uint8_t selID = 0; selID <= 7; ++selID) {
    for (uint8_t regID = 0; regID <= 31; ++regID) {
      regs.push_back(Registers::SysregHandle(regID, selID));
    }
  }
  return regs;
}

BNRegisterInfo V850E1Architecture::GetRegisterInfo(const uint32_t rid) {
  if (rid <= Registers::R31) {
    return RegisterInfo(rid, 0,
                        Sizes::LEN32BIT);  // struct contains: full width reg,
    // offset (for sub-registers), size
  }
  if (Registers::IsSysregHandle(rid)) {
    /* Banked system registers are 32-bit, no sub-register aliasing. Each
     * (regID, selID) is its own full-width register. */
    return RegisterInfo(rid, 0, Sizes::LEN32BIT);
  }
  if (rid == Registers::FPSR) {
    return RegisterInfo(Registers::FPSR, 0, Sizes::LEN32BIT);
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

/* ------------------------------------------------------------------------- *
 *  FPU intrinsic scaffolding.
 *  Binary Ninja has no native LLIL primitive for:
 *    - max/min float
 *    - reciprocal / reciprocal-square-root approximation
 *    - fused multiply-add family (fmaf / fmsf / fnmaf / fnmsf)
 *    - half-precision cvt
 *    - unsigned-int <-> float cvt
 *    - floor/ceil/trunc/round with rounding mode other than current
 *    - IEEE754 compare writing to FPSR CC bits
 *    - FPSR -> PSW.Z transfer
 *  We emit these as Architecture intrinsics so BN displays them as named
 *  function calls in the decompiler.
 * ------------------------------------------------------------------------- */
std::vector<uint32_t> V850E1Architecture::GetAllIntrinsics() {
  std::vector<uint32_t> out;
  for (uint32_t i = FpuIntrinsic::MaxfS; i < FpuIntrinsic::_END; ++i) {
    out.push_back(i);
  }
  return out;
}

std::string V850E1Architecture::GetIntrinsicName(const uint32_t intrinsic) {
  switch (intrinsic) {
    case FpuIntrinsic::MaxfS:     return "v850.maxf.s";
    case FpuIntrinsic::MinfS:     return "v850.minf.s";
    case FpuIntrinsic::RecipfS:   return "v850.recipf.s";
    case FpuIntrinsic::RsqrtfS:   return "v850.rsqrtf.s";
    case FpuIntrinsic::RoundfSw:  return "v850.roundf.sw";
    case FpuIntrinsic::RoundfSuw: return "v850.roundf.suw";
    case FpuIntrinsic::TrncfSuw:  return "v850.trncf.suw";
    case FpuIntrinsic::CeilfSuw:  return "v850.ceilf.suw";
    case FpuIntrinsic::FloorfSuw: return "v850.floorf.suw";
    case FpuIntrinsic::CvtfSuw:   return "v850.cvtf.suw";
    case FpuIntrinsic::CvtfUws:   return "v850.cvtf.uws";
    case FpuIntrinsic::CvtfHs:    return "v850.cvtf.hs";
    case FpuIntrinsic::CvtfSh:    return "v850.cvtf.sh";
    case FpuIntrinsic::FmafS:     return "v850.fmaf.s";
    case FpuIntrinsic::FmsfS:     return "v850.fmsf.s";
    case FpuIntrinsic::FnmafS:    return "v850.fnmaf.s";
    case FpuIntrinsic::FnmsfS:    return "v850.fnmsf.s";
    case FpuIntrinsic::CmpfS:     return "v850.cmpf.s";
    case FpuIntrinsic::Trfsr:     return "v850.trfsr";
    default:                      return "";
  }
}

std::vector<BN::NameAndType> V850E1Architecture::GetIntrinsicInputs(
    const uint32_t intrinsic) {
  const auto f32 = BN::Type::FloatType(4);
  const auto u32 = BN::Type::IntegerType(4, false);
  switch (intrinsic) {
    case FpuIntrinsic::MaxfS:
    case FpuIntrinsic::MinfS:
      return {{"a", f32}, {"b", f32}};
    case FpuIntrinsic::RecipfS:
    case FpuIntrinsic::RsqrtfS:
    case FpuIntrinsic::RoundfSw:
    case FpuIntrinsic::RoundfSuw:
    case FpuIntrinsic::TrncfSuw:
    case FpuIntrinsic::CeilfSuw:
    case FpuIntrinsic::FloorfSuw:
    case FpuIntrinsic::CvtfSuw:
    case FpuIntrinsic::CvtfSh:
      return {{"a", f32}};
    case FpuIntrinsic::CvtfUws:
      return {{"a", u32}};
    case FpuIntrinsic::CvtfHs:
      return {{"half", u32}};  // half packed in low 16 bits of a GPR
    case FpuIntrinsic::FmafS:
    case FpuIntrinsic::FmsfS:
    case FpuIntrinsic::FnmafS:
    case FpuIntrinsic::FnmsfS:
      return {{"a", f32}, {"b", f32}, {"c", f32}};
    case FpuIntrinsic::CmpfS:
      return {{"fcond", u32}, {"a", f32}, {"b", f32}, {"fcbit", u32}};
    case FpuIntrinsic::Trfsr:
      return {{"fcbit", u32}, {"fpsr", u32}};
    default:
      return {};
  }
}

std::vector<BN::Confidence<BN::Ref<BN::Type>>>
V850E1Architecture::GetIntrinsicOutputs(const uint32_t intrinsic) {
  const auto f32 = BN::Type::FloatType(4);
  const auto u32 = BN::Type::IntegerType(4, false);
  switch (intrinsic) {
    case FpuIntrinsic::MaxfS:
    case FpuIntrinsic::MinfS:
    case FpuIntrinsic::RecipfS:
    case FpuIntrinsic::RsqrtfS:
    case FpuIntrinsic::FmafS:
    case FpuIntrinsic::FmsfS:
    case FpuIntrinsic::FnmafS:
    case FpuIntrinsic::FnmsfS:
    case FpuIntrinsic::CvtfUws:
    case FpuIntrinsic::CvtfHs:
      return {BN::Confidence<BN::Ref<BN::Type>>(f32)};
    case FpuIntrinsic::RoundfSw:
      return {BN::Confidence<BN::Ref<BN::Type>>(BN::Type::IntegerType(4, true))};
    case FpuIntrinsic::RoundfSuw:
    case FpuIntrinsic::TrncfSuw:
    case FpuIntrinsic::CeilfSuw:
    case FpuIntrinsic::FloorfSuw:
    case FpuIntrinsic::CvtfSuw:
    case FpuIntrinsic::CvtfSh:
      return {BN::Confidence<BN::Ref<BN::Type>>(u32)};
    case FpuIntrinsic::CmpfS:
      return {BN::Confidence<BN::Ref<BN::Type>>(u32)};  // updated FPSR
    case FpuIntrinsic::Trfsr:
      return {};  // writes PSW.Z side-effect; modelled via flag write in lift
    default:
      return {};
  }
}

/*
 * CC-RH (Renesas) calling convention for V850 / RH850.
 *
 * Reference: CC-RH Compiler User's Manual (R20UT3516EJ). Summary:
 *   - Integer argument registers: r6, r7, r8, r9 (first 4 args; rest on stack)
 *   - Integer return value: r10 (low) / r11 (high, for 64-bit)
 *   - Stack pointer: r3 (SP)
 *   - Global pointer: r4 (GP)
 *   - Text pointer / small-data pointer: r5 (TP)
 *   - Element pointer: r30 (EP)
 *   - Link register: r31 (LP) - caller-saved (holds return address)
 *   - Caller-saved (scratch): r1, r5, r10-r17, r31
 *   - Callee-saved (preserved across calls): r20-r29
 *   - r2 is reserved/RTOS-use; not tracked as either.
 *
 * Registering this as the default fixes bogus decompiler signatures
 * (previously BN assumed every register was an input, producing 20+ args).
 */
class V850CCRHCallingConvention final : public BN::CallingConvention {
 public:
  explicit V850CCRHCallingConvention(BN::Architecture *arch)
      : CallingConvention(arch, "cc-rh") {}

  uint32_t GetGlobalPointerRegister() override { return Registers::R4; }

  std::vector<uint32_t> GetIntegerArgumentRegisters() override {
    return std::vector<uint32_t>{Registers::R6, Registers::R7, Registers::R8,
                                 Registers::R9};
  }

  uint32_t GetIntegerReturnValueRegister() override { return Registers::R10; }

  uint32_t GetHighIntegerReturnValueRegister() override {
    return Registers::R11;
  }

  std::vector<uint32_t> GetCallerSavedRegisters() override {
    return std::vector<uint32_t>{
        Registers::R1,  Registers::R5,  Registers::R10, Registers::R11,
        Registers::R12, Registers::R13, Registers::R14, Registers::R15,
        Registers::R16, Registers::R17, Registers::R31};
  }

  std::vector<uint32_t> GetCalleeSavedRegisters() override {
    return std::vector<uint32_t>{
        Registers::R20, Registers::R21, Registers::R22, Registers::R23,
        Registers::R24, Registers::R25, Registers::R26, Registers::R27,
        Registers::R28, Registers::R29};
  }
};
}  // namespace V850

extern "C" {
BN_DECLARE_CORE_ABI_VERSION
BINARYNINJAPLUGIN bool CorePluginInit() {
  BN::Architecture *V850E1 = new V850::V850E1Architecture("V850");
  BN::Architecture::Register(V850E1);

  const BN::Ref<BN::CallingConvention> cc =
      new V850::V850CCRHCallingConvention(V850E1);
  V850E1->RegisterCallingConvention(cc);
  V850E1->SetDefaultCallingConvention(cc);

  return true;
}
}