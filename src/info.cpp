// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#include <binaryninjaapi.h>

#include <cstdint>

#include "conditions.h"
#include "instructions.h"
#include "opcodes.h"
#include "registers.h"
#include "sizes.h"

namespace V850 {
// Default Info method -- can be overridden if needed
bool Instruction::Info(uint64_t opcode, uint64_t addr,
                       BN::InstructionInfo &result) {
  // Don't try to disassemble instructions that aren't aligned
  if (addr & 0b1) {
    result.length = 1;
    return false;
  }

  result.length = GetInstrLen();
  return true;
}

bool Info_III_BCOND(const uint64_t opcode, const uint64_t addr,
                    BN::InstructionInfo &result) {
  /* Calculate branch target */
  int32_t displacement =
      static_cast<int16_t>(((opcode & OpcodeFields::MASK_III_DISP_H) >>
                            OpcodeFields::SHIFT_III_DISP_H) |
                           ((opcode & OpcodeFields::MASK_III_DISP_L) >>
                            OpcodeFields::SHIFT_III_DISP_L));
  // Sign-extend 9-bit displacement
  if (displacement & (1 << 8)) {
    displacement = static_cast<int32_t>(0xFFFFFF00) | displacement;
  }

  /* Branch is based on condition code; target is based on displacement */
  if (const auto condition =
          static_cast<uint8_t>(opcode & OpcodeFields::MASK_III_COND);
      condition == Conditions::CONDITION_CODE_BR) {
    result.AddBranch(UnconditionalBranch, addr + displacement);

  } else {
    result.AddBranch(TrueBranch, addr + displacement);
    result.AddBranch(FalseBranch, addr + Sizes::LEN16BIT);
  }

  result.length = Sizes::LEN16BIT;
  return true;
}

bool Info_V_JARL_JR(const uint64_t opcode, const uint64_t addr,
                    BN::InstructionInfo &result) {
  auto disp22 =
      (opcode & OpcodeFields::MASK_V_DISP_H) << OpcodeFields::SHIFT_V_DISP_H |
      opcode >> 16 &
          OpcodeFields::MASK_V_DISP_L;  // Bit 0 of displacement masked to 0

  if (disp22 & (1 << 21)) {  // Sign extend; MS bit is set
    disp22 = (0b1111111111 << 22) | disp22;
  }
  const uint32_t target = static_cast<uint32_t>(addr) + disp22;

  if (const auto reg2 = static_cast<uint8_t>(
          (opcode & OpcodeFields::MASK_REG2) >> OpcodeFields::SHIFT_REG2);
      reg2 == Registers::R0) {  // jr
    result.AddBranch(UnconditionalBranch, target);

  } else if (target ==
             (addr + Sizes::LEN32BIT)) {  // jarl, jump and register link
    result.AddBranch(UnconditionalBranch,
                     target);  // jarl used ot set up a long range call via jmp
  } else {
    result.AddBranch(CallDestination, target);
  }

  result.length = Sizes::LEN32BIT;
  return true;
}

bool Info_VI_MOVEA_MOV(const uint16_t *instruction_data,
                       BN::InstructionInfo &result) {
  if (const auto reg2 = static_cast<uint8_t>(
          (instruction_data[0] & OpcodeFields::MASK_REG2) >>
          OpcodeFields::SHIFT_REG2);
      reg2 == Registers::R0) {  // mov, 48-bit
    result.length = Sizes::LEN48BIT;

  } else {  // movea
    result.length = Sizes::LEN32BIT;
  }
  return true;
}

bool Format_Ext_Info(const uint64_t opcode, BN::InstructionInfo &result) {
  /* Parse extended-format instructions that affect control flow */
  switch (opcode >> 16 & 0xFFFF) {
    case Opcodes::OP_X_TRAP:
      result.length = Sizes::LEN32BIT;
      result.AddBranch(ExceptionBranch);
      return true;
    case Opcodes::OP_X_HALT:
      result.length = Sizes::LEN32BIT;
      result.AddBranch(ExceptionBranch);
      return true;
    /* Return instructions */
    case Opcodes::OP_X_RETI:
    case Opcodes::OP_X_CTRET:
    case Opcodes::OP_X_DBRET:
      result.AddBranch(FunctionReturn);
      result.length = Sizes::LEN32BIT;
      return true;
    default:
      return false;
  }
}

bool Bc::Info(const uint64_t opcode, const uint64_t addr,
              BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bge::Info(const uint64_t opcode, const uint64_t addr,
               BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bgt::Info(const uint64_t opcode, const uint64_t addr,
               BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bh::Info(const uint64_t opcode, const uint64_t addr,
              BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Ble::Info(const uint64_t opcode, const uint64_t addr,
               BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Blt::Info(const uint64_t opcode, const uint64_t addr,
               BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bn::Info(const uint64_t opcode, const uint64_t addr,
              BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bnc::Info(const uint64_t opcode, const uint64_t addr,
               BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bnh::Info(const uint64_t opcode, const uint64_t addr,
               BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bnv::Info(const uint64_t opcode, const uint64_t addr,
               BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bnz::Info(const uint64_t opcode, const uint64_t addr,
               BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bp::Info(const uint64_t opcode, const uint64_t addr,
              BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Br::Info(const uint64_t opcode, const uint64_t addr,
              BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bsa::Info(const uint64_t opcode, const uint64_t addr,
               BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bv::Info(const uint64_t opcode, const uint64_t addr,
              BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool Bz::Info(const uint64_t opcode, const uint64_t addr,
              BN::InstructionInfo &result) {
  return Info_III_BCOND(opcode, addr, result);
}

bool CalltImm6::Info(const uint64_t opcode, const uint64_t addr,
                     BN::InstructionInfo &result) {
  result.AddBranch(IndirectBranch);
  result.length = Sizes::LEN16BIT;
  return true;
}

bool Ctret::Info(const uint64_t opcode, const uint64_t addr,
                 BN::InstructionInfo &result) {
  return Format_Ext_Info(opcode, result);
}

bool Dbret::Info(const uint64_t opcode, const uint64_t addr,
                 BN::InstructionInfo &result) {
  return Format_Ext_Info(opcode, result);
}

bool Halt::Info(const uint64_t opcode, const uint64_t addr,
                BN::InstructionInfo &result) {
  return Format_Ext_Info(opcode, result);
}

bool JarlDisp22R2::Info(const uint64_t opcode, const uint64_t addr,
                        BN::InstructionInfo &result) {
  return Info_V_JARL_JR(opcode, addr, result);
}

bool JmpR1::Info(const uint64_t opcode, const uint64_t addr,
                 BN::InstructionInfo &result) {
  result.AddBranch(UnresolvedBranch);
  result.length = Sizes::LEN16BIT;
  return true;
}

bool JrDisp22::Info(const uint64_t opcode, const uint64_t addr,
                    BN::InstructionInfo &result) {
  return Info_V_JARL_JR(opcode, addr, result);
}

bool Reti::Info(const uint64_t opcode, const uint64_t addr,
                BN::InstructionInfo &result) {
  return Format_Ext_Info(opcode, result);
}

bool SwitchR1::Info(const uint64_t opcode, const uint64_t addr,
                    BN::InstructionInfo &result) {
  result.AddBranch(UnresolvedBranch);
  result.length = Sizes::LEN16BIT;
  return true;
}

bool Trap::Info(const uint64_t opcode, const uint64_t addr,
                BN::InstructionInfo &result) {
  return Format_Ext_Info(opcode, result);
}

}  // end namespace V850
