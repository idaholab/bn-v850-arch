// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#ifndef SRC_ARCHITECTURE_H_
#define SRC_ARCHITECTURE_H_

#include <binaryninjaapi.h>

#include "instructions.h"

namespace BN = BinaryNinja;

namespace V850 {

class V850Architecture : public BN::Architecture {
 protected:
  static BNRegisterInfo RegisterInfo(uint32_t fullWidthReg, size_t offset,
                                     size_t size, bool zeroExtend = false);

 public:
  IsaType isa_type;

  explicit V850Architecture(const std::string& name);

  size_t GetAddressSize() const override;
  BNEndianness GetEndianness() const override;
  size_t GetDefaultIntegerSize() const override;
  size_t GetInstructionAlignment() const override;
  size_t GetMaxInstructionLength() const override;
  std::string GetRegisterName(uint32_t reg) override;
  uint32_t GetLinkRegister() override;
  uint32_t GetStackPointerRegister() override;

  bool GetInstructionInfo(const uint8_t* data, uint64_t addr, size_t maxLen,
                          BN::InstructionInfo& result) override;
  bool GetInstructionText(
      const uint8_t* data, uint64_t addr, size_t& len,
      std::vector<BN::InstructionTextToken>& result) override;
  bool GetInstructionLowLevelIL(const uint8_t* data, uint64_t addr, size_t& len,
                                BN::LowLevelILFunction& il) override;
};

class V850E1Architecture final : public V850Architecture {
 public:
  explicit V850E1Architecture(const std::string& name);

  std::vector<uint32_t> GetAllRegisters() override;
  BNRegisterInfo GetRegisterInfo(uint32_t reg) override;
  std::vector<uint32_t> GetAllFlags() override;
  std::string GetFlagName(uint32_t flag) override;
  BNFlagRole GetFlagRole(uint32_t flag, uint32_t semClass) override;
  std::vector<uint32_t> GetFlagsWrittenByFlagWriteType(uint32_t flags) override;
  std::string GetFlagWriteTypeName(uint32_t flags) override;
  std::vector<uint32_t> GetFlagsRequiredForFlagCondition(
      BNLowLevelILFlagCondition cond, uint32_t semClass) override;
};

}  // namespace V850

#endif  // SRC_ARCHITECTURE_H_
