// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#include "util.h"

#include <cstdio>

#include "conditions.h"
#include "flags.h"
#include "opcodes.h"
#include "registers.h"
#include "sizes.h"

namespace V850 {

const char *RegToStr(const uint8_t reg_id) {
  switch (reg_id) {
    /* Program Registers */
    case Registers::R0:
      return "r0";
    case Registers::R1:
      return "r1";
    case Registers::R2:
      return "r2";
    case Registers::SP:
      return "sp";
    case Registers::R4:
      return "gp";
    case Registers::R5:
      return "tp";
    case Registers::R6:
      return "r6";
    case Registers::R7:
      return "r7";
    case Registers::R8:
      return "r8";
    case Registers::R9:
      return "r9";
    case Registers::R10:
      return "r10";
    case Registers::R11:
      return "r11";
    case Registers::R12:
      return "r12";
    case Registers::R13:
      return "r13";
    case Registers::R14:
      return "r14";
    case Registers::R15:
      return "r15";
    case Registers::R16:
      return "r16";
    case Registers::R17:
      return "r17";
    case Registers::R18:
      return "r18";
    case Registers::R19:
      return "r19";
    case Registers::R20:
      return "r20";
    case Registers::R21:
      return "r21";
    case Registers::R22:
      return "r22";
    case Registers::R23:
      return "r23";
    case Registers::R24:
      return "r24";
    case Registers::R25:
      return "r25";
    case Registers::R26:
      return "r26";
    case Registers::R27:
      return "r27";
    case Registers::R28:
      return "r28";
    case Registers::R29:
      return "r29";
    case Registers::EP:
      return "ep";
    case Registers::R31:
      return "lp";
    default:
      break;
  }
  if (reg_id == Registers::FPSR) {
    return "fpsr";
  }
  return nullptr;
}

const char *SystemRegToStr(const uint8_t reg_id) {
  /* Legacy V850 (selID = 0) system register name. Kept for compatibility
   * with paths that haven't been migrated to the banked API. */
  return SystemRegToStrBanked(reg_id, 0);
}

const char *SystemRegToStrBanked(const uint8_t regID, const uint8_t selID) {
  /* Names sourced from the RH850G3MH Software Manual
   * (R01US0143EJ0130 Rev 1.30, 2016-12-22). Coverage:
   *   selID 0 : Basic system registers    (Table 3.3,  p.39)
   *   selID 1 : Machine configuration     (Table 3.3,  p.39)
   *   selID 2 : Interrupt + memory-error  (Tables 3.3/3.30, p.39/58)
   *   selID 4 : Cache control             (Table 3.53, p.77)
   *   selID 5 : MPU mode / region control (Table 3.41, p.69)
   *   selID 6 : MPU protection areas 0-7  (Table 3.41, p.69)
   *   selID 7 : MPU protection areas 8-15 (Table 3.41, p.70)
   * FPU system registers (FPSR..FPCFG) live in selID 0 (SR6..SR10) per
   * Table 3.35 on p.62. Other banked selIDs observed on G3MH firmware
   * but not individually tabulated here fall through to "sr<regID>_<selID>"
   * (canonical RH850 assembler spelling) rather than returning nullptr,
   * so the decompiler never sees INVALID_REG_ID. */
  switch (selID) {
    case 0:
      switch (regID) {
        case 0: return "eipc";
        case 1: return "eipsw";
        case 2: return "fepc";
        case 3: return "fepsw";
        case 4: return "ecr";      /* V850 base bank name; G3MH retires ECR */
        case 5: return "psw";
        case 6: return "fpsr";     /* FPU, p.62 */
        case 7: return "fpepc";    /* FPU, p.62 */
        case 8: return "fpst";     /* FPU, p.62 */
        case 9: return "fpcc";     /* FPU, p.62 */
        case 10: return "fpcfg";   /* FPU, p.62 */
        case 13: return "eiic";    /* EI-level exception cause */
        case 14: return "feic";    /* FE-level exception cause */
        case 16: return "ctpc";
        case 17: return "ctpsw";
        case 18: return "dbpc";
        case 19: return "dbpsw";
        case 20: return "ctbp";
        case 28: return "eiwr";
        case 29: return "fewr";
        case 31: return "bsel";
        default: break;
      }
      break;
    case 1:
      switch (regID) {
        case 0: return "mcfg0";
        case 2: return "rbase";
        case 3: return "ebase";
        case 4: return "intbp";
        case 5: return "mctl";
        case 6: return "pid";
        case 11: return "sccfg";
        case 12: return "scbp";
        default: break;
      }
      break;
    case 2:
      switch (regID) {
        case 0: return "htcfg0";
        case 6: return "mea";
        case 7: return "asid";
        case 8: return "mei";
        case 10: return "ispr";    /* interrupt regs, p.58 */
        case 11: return "pmr";
        case 12: return "icsr";
        case 13: return "intcfg";
        default: break;
      }
      break;
    case 4:
      switch (regID) {
        case 16: return "ictagl";
        case 17: return "ictagh";
        case 18: return "icdatl";
        case 19: return "icdath";
        case 24: return "icctrl";
        case 26: return "iccfg";
        case 28: return "icerr";
        default: break;
      }
      break;
    case 5:
      switch (regID) {
        case 0: return "mpm";
        case 1: return "mprc";
        case 4: return "mpbrgn";
        case 5: return "mptrgn";
        case 8: return "mca";
        case 9: return "mcs";
        case 10: return "mcc";
        case 11: return "mcr";
        default: break;
      }
      break;
    case 6:
    case 7: {
      /* MPU protection areas 0..15 — each entry has 3 regs (LA, UA, AT)
       * laid out in a regular pattern. Return nullptr to fall through to
       * the generic sr<r>_<s> naming so we don't mis-label partial areas. */
      break;
    }
    default:
      break;
  }
  /* Thread-local fallback name for banked regs not individually listed. */
  thread_local char fallback[16];
  std::snprintf(fallback, sizeof(fallback), "sr%u_%u",
                static_cast<unsigned>(regID), static_cast<unsigned>(selID));
  return fallback;
}

const char *SysregHandleToStr(const uint32_t handle) {
  if (!Registers::IsSysregHandle(handle)) {
    return nullptr;
  }
  return SystemRegToStrBanked(Registers::SysregHandleRegID(handle),
                              Registers::SysregHandleSelID(handle));
}

const char *FlagToStr(const uint32_t flag_id) {
  switch (flag_id) {
    case Flags::FLAG_Z_ZERO:
      return "z";
    case Flags::FLAG_S_SIGN:
      return "s";
    case Flags::FLAG_OV_OVERFLOW:
      return "ov";
    case Flags::FLAG_CY_CARRY:
      return "cy";
    case Flags::FLAG_SAT_SATURATED:
      return "sat";
    case Flags::FLAG_ID_INTERRUPT_DISABLE:
      return "id";
    case Flags::FLAG_EP_EXCEPTION_PENDING:
      return "ep";
    case Flags::FLAG_NP_NMI_PENDING:
      return "np";
    default:
      return nullptr;
  }
}

const char *ConditionToStr(const uint8_t condition) {
  /* Helper function for generating instruction text based on condition code
   * Used by: setf, sasf (note that bcond condition codes/strings are slightly
   * different) */
  switch (condition) {
    case Conditions::CONDITION_CODE_V:
      return "v";
    case Conditions::CONDITION_CODE_NV:
      return "nv";
    case Conditions::CONDITION_CODE_C_L:
      return "c/l";
    case Conditions::CONDITION_CODE_NC_NL:
      return "nc/nl";
    case Conditions::CONDITION_CODE_Z:
      return "z";
    case Conditions::CONDITION_CODE_NZ:
      return "nz";
    case Conditions::CONDITION_CODE_NH:
      return "nh";
    case Conditions::CONDITION_CODE_H:
      return "h";
    case Conditions::CONDITION_CODE_S_N:
      return "s/n";
    case Conditions::CONDITION_CODE_NS_P:
      return "ns/p";
    case Conditions::CONDITION_CODE_T:
      return "t";
    case Conditions::CONDITION_CODE_SA:
      return "sa";
    case Conditions::CONDITION_CODE_LT:
      return "lt";
    case Conditions::CONDITION_CODE_GE:
      return "ge";
    case Conditions::CONDITION_CODE_LE:
      return "le";
    case Conditions::CONDITION_CODE_GT:
      return "gt";
    default:
      return nullptr;
  }
}

BN::ExprId ConditionToIL(const uint8_t condition, BN::LowLevelILFunction &il) {
  /* Generate IL for flag conditions. Used by setf, sasf */
  switch (condition) {
    case Conditions::CONDITION_CODE_V:
      return il.FlagCondition(LLFC_O);
    case Conditions::CONDITION_CODE_NV:
      return il.FlagCondition(LLFC_NO);
    case Conditions::CONDITION_CODE_C_L:
      return il.FlagCondition(LLFC_ULT);
    case Conditions::CONDITION_CODE_NC_NL:
      return il.FlagCondition(LLFC_UGE);
    case Conditions::CONDITION_CODE_Z:
      return il.FlagCondition(LLFC_E);
    case Conditions::CONDITION_CODE_NZ:
      return il.FlagCondition(LLFC_NE);
    case Conditions::CONDITION_CODE_NH:
      return il.FlagCondition(LLFC_ULE);
    case Conditions::CONDITION_CODE_H:
      return il.FlagCondition(LLFC_UGT);
    case Conditions::CONDITION_CODE_S_N:
      return il.FlagCondition(LLFC_NEG);
    case Conditions::CONDITION_CODE_NS_P:
      return il.FlagCondition(LLFC_POS);
    case Conditions::CONDITION_CODE_T:
      return il.Const(Sizes::LEN64BIT, 1);  // unconditional
    case Conditions::CONDITION_CODE_SA:
      // if SAT flag set - behavior not yet implemented, TODO
      return il.Unimplemented();
    case Conditions::CONDITION_CODE_LT:
      return il.FlagCondition(LLFC_SLT);
    case Conditions::CONDITION_CODE_GE:
      return il.FlagCondition(LLFC_SGE);
    case Conditions::CONDITION_CODE_LE:
      return il.FlagCondition(LLFC_SLE);
    case Conditions::CONDITION_CODE_GT:
      return il.FlagCondition(LLFC_SGT);
    default:
      return il.Unimplemented();
  }
}

void GenerateTextForRegisterList12(
    const uint64_t opcode, std::vector<BN::InstructionTextToken> &result) {
  /* Special helper function for prepare instruction. Generates list of
   * specified general purpose registers (r20-r31) based on which bits are set,
   * as follows:
   *
   *   31    20    29    28    27    26    25    24    23    22    21   20  1 0
   * | r24 | r25 | r26 | r27 | r20 | r21 | r22 | r23 | r28 | r29 | r31 | ... |
   * r30 | */

  result.emplace_back(OperandSeparatorToken, "{");

  // Check which general purpose registers (r20-r31) are specified and generate
  // text accordingly Note: registers are stored in ascending order (e.g., r20,
  // then r21, and so on)
  if (opcode & OpcodeFields::MASK_XIII_BIT_R20 << 16) {
    result.emplace_back(RegisterToken, "r20", Registers::R20);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R21 << 16) {
    result.emplace_back(RegisterToken, "r21", Registers::R21);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R22 << 16) {
    result.emplace_back(RegisterToken, "r22", Registers::R22);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R23 << 16) {
    result.emplace_back(RegisterToken, "r23", Registers::R23);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R24 << 16) {
    result.emplace_back(RegisterToken, "r24", Registers::R24);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R25 << 16) {
    result.emplace_back(RegisterToken, "r25", Registers::R25);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R26 << 16) {
    result.emplace_back(RegisterToken, "r26", Registers::R26);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R27 << 16) {
    result.emplace_back(RegisterToken, "r27", Registers::R27);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R28 << 16) {
    result.emplace_back(RegisterToken, "r28", Registers::R28);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R29 << 16) {
    result.emplace_back(RegisterToken, "r29", Registers::R29);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R30) {
    result.emplace_back(RegisterToken, "r30", Registers::EP);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  if (opcode & (OpcodeFields::MASK_XIII_BIT_R31 << 16)) {
    result.emplace_back(RegisterToken, "r31", Registers::R31);
    result.emplace_back(OperandSeparatorToken, ", ");
  }
  // If at least one register was specified, remove the last ", " in the list
  if ((opcode & OpcodeFields::MASK_XIII_LIST12_WORD1) ||
      (opcode & (OpcodeFields::MASK_XIII_LIST12_WORD2 << 16))) {
    result.pop_back();
  }
  result.emplace_back(OperandSeparatorToken, "}");
}

void GenerateILToSaveRegisters(const uint64_t opcode,
                               BN::LowLevelILFunction &il) {
  /* Special helper function for prepare instruction. Generates il to save
   * specified general purpose registers (r20-r31) to the stack, in preparation
   * for a function call. Which registered are saved is specified based on which
   * instruction bits are set, as follows:
   *
   *   31    20    29    28    27    26    25    24    23    22    21   20  1 0
   * | r24 | r25 | r26 | r27 | r20 | r21 | r22 | r23 | r28 | r29 | r31 | ... |
   * r30 | */

  // Check which general purpose registers (r20-r31) are specified and save
  // contents to the stack Note: registers are stored in ascending order (e.g.,
  // r20, then r21, and so on)
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R20) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R20)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R21) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R21)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R22) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R22)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R23) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R23)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R24) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R24)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R25) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R25)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R26) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R26)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R27) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R27)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R28) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R28)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R29) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R29)));
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R30) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::EP)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R31) {
    il.AddInstruction(
        il.Push(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::R31)));
  }
}

void GenerateILToRestoreRegisters(const uint64_t opcode,
                                  BN::LowLevelILFunction &il) {
  /* Special helper function for dispose instruction. Generates il to restore
   * specified general purpose registers (r20-r31) by popping them off the
   * stack. Registers are specified based on bits in the dispose instruction, as
   * follows:
   *
   *   31    20    29    28    27    26    25    24    23    22    21   20  1 0
   * | r24 | r25 | r26 | r27 | r20 | r21 | r22 | r23 | r28 | r29 | r31 | ... |
   * r30 | */

  // Check which general purpose registers (r20-r31) are specified. Restore
  // values from stack. Note: registers are restored in ascending order (e.g.,
  // r20, then r21, and so on)
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R20) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R20,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R21) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R21,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R22) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R22,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R23) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R23,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R24) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R24,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R25) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R25,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R26) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R26,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R27) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R27,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R28) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R28,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R29) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R29,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode & OpcodeFields::MASK_XIII_BIT_R30) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::EP,
                                     il.Pop(Sizes::LEN32BIT)));
  }
  if (opcode >> 16 & OpcodeFields::MASK_XIII_BIT_R31) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::R31,
                                     il.Pop(Sizes::LEN32BIT)));
  }
}

}  // namespace V850