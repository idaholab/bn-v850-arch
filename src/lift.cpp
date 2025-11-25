// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#include <binaryninjaapi.h>

#include <cstdint>

#include "conditions.h"
#include "flags.h"
#include "instructions.h"
#include "opcodes.h"
#include "registers.h"
#include "sizes.h"
#include "util.h"

namespace BN = BinaryNinja;

#define UNIMPLEMENTED                    \
  il.AddInstruction(il.Unimplemented()); \
  return true;

namespace V850 {

bool Lift_I_JMP_IV_SLDHU_SLDBU(const uint64_t opcode, size_t &len,
                               BN::LowLevelILFunction &il) {
  if (const auto reg2 = ExtractReg2OpcodeField(opcode); reg2 == Registers::R0) {
    // Jmp; jump to address in register
    const auto reg1 = ExtractReg1OpcodeField(opcode);

    il.AddInstruction(
        il.Call(il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                       il.Const(Sizes::LEN32BIT, 0xFFFFFFFE))));

  } else {
    auto opcode_7 =
        static_cast<uint8_t>((opcode & OpcodeFields::OPCODE_7_BITS) >>
                             OpcodeFields::SHIFT_7BIT_OPCODE);
    uint8_t disp;

    if (opcode_7 == Opcodes::OP_IV_SLD_BU) {
      // Short format load byte unsigned
      // Text format: sld.bu disp4[ep], reg2
      disp = static_cast<uint8_t>(opcode & OpcodeFields::MASK_IV_4BIT_DISP);
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
          il.ZeroExtend(
              Sizes::LEN32BIT,
              il.Load(
                  Sizes::LEN8BIT,
                  il.Add(Sizes::LEN32BIT,
                         il.Register(Sizes::LEN32BIT, Registers::EP),
                         il.ZeroExtend(Sizes::LEN32BIT,
                                       il.Const(Sizes::LEN32BIT, disp)))))));

    } else if (opcode_7 == Opcodes::OP_IV_SLD_HU) {
      // Short format load halfword unsigned
      // Text format: sld.hu disp5[ep], reg2
      disp = static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_4BIT_DISP)
                                  << OpcodeFields::SHIFT_IV_DISP);
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
          il.ZeroExtend(
              Sizes::LEN32BIT,
              il.Load(
                  Sizes::LEN32BIT,
                  il.Add(Sizes::LEN32BIT,
                         il.Register(Sizes::LEN32BIT, Registers::EP),
                         il.ZeroExtend(Sizes::LEN32BIT,
                                       il.Const(Sizes::LEN32BIT, disp)))))));
    } else {
      return false;
    }
  }
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_I_MOV_NOP(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  if (reg1 == Registers::R0 && reg2 == Registers::R0) {
    // Nop; no operation
    il.AddInstruction(il.Nop());

  } else {
    // Mov; move register
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg2,
                                     il.Register(Sizes::LEN32BIT, reg1)));
  }

  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_I_NOT(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  // Not
  il.AddInstruction(il.SetRegister(
      Sizes::LEN32BIT, reg2,
      il.Not(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1)),
      Flags::FLAGS_WRITE_S_Z));

  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_I_SWITCH_DBTRAP_DIVH(const uint64_t opcode, uint64_t addr,
                               size_t &len, BN::LowLevelILFunction &il,
                               BinaryNinja::Architecture *arch) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  if (reg2 == Registers::R0) {  // switch
    BN::ExprId lookup_addr = il.Add(
        Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, addr + Sizes::LEN16BIT),
        il.ShiftLeft(Sizes::LEN32BIT,  // reg1 << 1 is index into table
                     il.Register(Sizes::LEN32BIT, reg1),
                     il.Const(Sizes::LEN32BIT, 1)));
    BN::ExprId target = il.Add(
        Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, addr + Sizes::LEN16BIT),
        il.ShiftLeft(Sizes::LEN32BIT,
                     il.SignExtend(Sizes::LEN32BIT,
                                   il.Load(Sizes::LEN16BIT, lookup_addr)),
                     il.Const(Sizes::LEN32BIT, 1)));

    if (BNLowLevelILLabel *jump_target = il.GetLabelForAddress(arch, target)) {
      il.AddInstruction(il.Goto(*jump_target));
    } else {
      il.AddInstruction(il.Jump(target));
    }

    len = Sizes::LEN16BIT;
    return true;

  } else if (opcode == Opcodes::EXACT_OP_I_DBTRAP) {  // dbtrap
    // Debug trap; dbrap
    // Text format: dbtrap
    il.AddInstruction(il.Store(
        Sizes::LEN32BIT,  // dbpc <- pc + 2
        il.ConstPointer(Sizes::LEN32BIT, Registers::SYSTEM_REG_BASE +
                                             Registers::V850_REG_DBPC *
                                                 Registers::REGISTER_SIZE),
        il.Const(Sizes::LEN32BIT, addr + Sizes::LEN16BIT)));
    il.AddInstruction(il.Store(
        Sizes::LEN32BIT,  // dbpsw <- psw
        il.ConstPointer(Sizes::LEN32BIT, Registers::SYSTEM_REG_BASE +
                                             Registers::V850_REG_DBPSW *
                                                 Registers::REGISTER_SIZE),
        il.Load(Sizes::LEN32BIT,
                il.ConstPointer(
                    Sizes::LEN32BIT,
                    Registers::SYSTEM_REG_BASE +
                        Registers::V850_REG_PSW * Registers::REGISTER_SIZE))));
    il.AddInstruction(il.Store(
        Sizes::LEN32BIT,
        il.ConstPointer(Sizes::LEN32BIT,
                        Registers::SYSTEM_REG_BASE +
                            Registers::V850_REG_PSW * Registers::REGISTER_SIZE),
        il.Or(
            Sizes::LEN32BIT,  // set NP (non-maskable interrupt pending) flag in
                              // PSW
            il.Or(
                Sizes::LEN32BIT,  // set EP (exception pending) flag in PSW
                il.Or(
                    Sizes::LEN32BIT,  // set ID (interrupt disable) flagt in PSW
                    il.Load(Sizes::LEN32BIT,
                            il.ConstPointer(Sizes::LEN32BIT,
                                            Registers::SYSTEM_REG_BASE +
                                                Registers::V850_REG_PSW *
                                                    Registers::REGISTER_SIZE)),
                    il.Const(Sizes::LEN32BIT, Flags::MASK_SET_ID_FLAG)),
                il.Const(Sizes::LEN32BIT, Flags::MASK_SET_EP_FLAG)),
            il.Const(Sizes::LEN32BIT, Flags::MASK_SET_NP_FLAG))));
    il.AddInstruction(il.Trap(0x60));  // pc <- 0x60

    len = Sizes::LEN16BIT;
    return true;

  } else {  // divh
    // Divide reg2 by lower half-word of reg1 and store quotient in reg2;
    // remainder not stored
    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, reg2,
        il.DivDoublePrecSigned(
            Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
            il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                   il.Const(Sizes::LEN32BIT, 0xFFFF)),
            Flags::FLAGS_WRITE_OV_S_Z)));
    len = Sizes::LEN16BIT;
    return true;
  }
}

bool Lift_I_SATSUBR_ZXB(const uint64_t opcode, uint64_t addr, size_t &len,
                        BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  if (reg2 == Registers::R0) {
    // Zero extend byte
    // zxb reg1
    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, reg1,
        il.ZeroExtend(Sizes::LEN32BIT,
                      il.And(Sizes::LEN8BIT, il.Register(Sizes::LEN8BIT, reg1),
                             il.Const(Sizes::LEN8BIT, 0xFF)))));
    len = Sizes::LEN16BIT;
    return true;

  } else {
    // Saturated subtract reverse: reg1 - reg2
    // If result exceeds max pos/neg, store saturated value (7FFFFFFF/80000000)
    // in reg2 and set SAT flag NOTE: Once the result of a sat operation is
    // saturated, the SAT flag remains set even if the result of the subsequent
    // ops is not saturated. SAT flag is only reset by loading data to PSW with
    // LDSR.
    BN::LowLevelILLabel sat_pos_true, sat_pos_false, sat_neg_true,
        sat_neg_false, done;

    BN::ExprId result = il.Sub(
        Sizes::LEN64BIT,  // Calculate result of subtract operation
        il.Register(Sizes::LEN32BIT, reg1), il.Register(Sizes::LEN32BIT, reg2),
        Flags::FLAGS_WRITE_SAT_CY_OV_S_Z);  // TODO: implement SAT flag behavior
    il.AddInstruction(  // Check whether maximum negative value is exceeded
        il.If(il.CompareSignedLessThan(Sizes::LEN64BIT, result,
                                       il.Const(Sizes::LEN32BIT, 0x80000000)),
              sat_neg_true, sat_neg_false));

    il.MarkLabel(sat_neg_true);  // Saturated negative result
    il.AddInstruction(  // reg2 set to maximum negative word-sized value,
                        // 0x80000000
        il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                       il.Const(Sizes::LEN32BIT, 0x80000000)));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(sat_neg_false);
    il.AddInstruction(  // Check whether maximum positive value is exceeded
        il.If(
            il.CompareSignedGreaterThan(Sizes::LEN64BIT, result,
                                        il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)),
            sat_pos_true, sat_pos_false));

    il.MarkLabel(sat_pos_true);  // Saturated positive result
    il.AddInstruction(  // reg2 set to maximum positive word-sized value,
                        // 0x7FFFFFFF
        il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                       il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(sat_pos_false);  // Result NOT saturated
    il.AddInstruction(  // Store result just like normal subtract operation
        il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                       result));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(done);

    len = Sizes::LEN16BIT;
    return true;
  }
}

bool Lift_I_SATSUB_SXB(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  if (reg2 == Registers::R0) {
    // Sign extend byte
    // sxb reg1
    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, reg1,
        il.SignExtend(Sizes::LEN32BIT,
                      il.And(Sizes::LEN8BIT, il.Register(Sizes::LEN8BIT, reg1),
                             il.Const(Sizes::LEN8BIT, 0xFF)))));
    len = Sizes::LEN16BIT;
    return true;

  } else {
    // Saturated subtract, satsub
    // reg2 - reg1
    // If result exceeds max pos/neg, store saturated value (7FFFFFFF/80000000)
    // in reg2 and set SAT flag NOTE: Once the result of a sat operation is
    // saturated, the SAT flag remains set even if the result of the subsequent
    // ops is not saturated. SAT flag is only reset by loading data to PSW with
    // LDSR.
    BN::LowLevelILLabel sat_pos_true, sat_pos_false, sat_neg_true,
        sat_neg_false, done;

    BN::ExprId result = il.Sub(
        Sizes::LEN64BIT,  // Calculate result of subtract operation
        il.Register(Sizes::LEN32BIT, reg2), il.Register(Sizes::LEN32BIT, reg1),
        Flags::FLAGS_WRITE_SAT_CY_OV_S_Z);  // TODO: implement SAT flag behavior
    il.AddInstruction(  // Check whether maximum negative value is exceeded
        il.If(il.CompareSignedLessThan(Sizes::LEN64BIT, result,
                                       il.Const(Sizes::LEN32BIT, 0x80000000)),
              sat_neg_true, sat_neg_false));

    il.MarkLabel(sat_neg_true);  // Saturated negative result
    il.AddInstruction(  // reg2 set to maximum negative word-sized value,
                        // 0x80000000
        il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                       il.Const(Sizes::LEN32BIT, 0x80000000)));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(sat_neg_false);
    il.AddInstruction(  // Check whether maximum positive value is exceeded
        il.If(
            il.CompareSignedGreaterThan(Sizes::LEN64BIT, result,
                                        il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)),
            sat_pos_true, sat_pos_false));

    il.MarkLabel(sat_pos_true);  // Saturated positive result
    il.AddInstruction(  // reg2 set to maximum positive word-sized value,
                        // 0x7FFFFFFF
        il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                       il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(sat_pos_false);  // Result NOT saturated
    il.AddInstruction(  // Store result just like normal subtract operation
        il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                       result));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(done);

    len = Sizes::LEN16BIT;
    return true;
  }
}

bool Lift_I_SATADD_ZXH(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  if (reg2 == Registers::R0) {
    // Zero extend halfword
    // zxh reg1
    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, reg1,
        il.ZeroExtend(
            Sizes::LEN32BIT,
            il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                   il.Const(Sizes::LEN32BIT, 0xFFFF)))));
    return true;

  } else {
    // Saturated add register, satadd
    // If result exceeds max pos/neg, store saturated value (7FFFFFFF/80000000)
    // in reg2 and set SAT flag NOTE: Once the result of a sat operation is
    // saturated, the SAT flag remains set even if the result of the subsequent
    // ops is not saturated. SAT flag is only reset by loading data to PSW with
    // LDSR.
    BN::LowLevelILLabel sat_pos_true, sat_pos_false, sat_neg_true,
        sat_neg_false, done;

    BN::ExprId result = il.Add(
        Sizes::LEN64BIT,  // Calculate result of subtract operation
        il.Register(Sizes::LEN32BIT, reg2), il.Register(Sizes::LEN32BIT, reg1),
        Flags::FLAGS_WRITE_SAT_CY_OV_S_Z);  // TODO: implement SAT flag behavior

    il.AddInstruction(  // Check whether maximum negative value is exceeded
        il.If(il.CompareSignedLessThan(Sizes::LEN64BIT, result,
                                       il.Const(Sizes::LEN32BIT, 0x80000000)),
              sat_neg_true, sat_neg_false));

    il.MarkLabel(sat_neg_true);  // Saturated negative result
    il.AddInstruction(  // reg2 set to maximum negative word-sized value,
                        // 0x80000000
        il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                       il.Const(Sizes::LEN32BIT, 0x80000000)));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(sat_neg_false);
    il.AddInstruction(  // Check whether maximum positive value is exceeded
        il.If(
            il.CompareSignedGreaterThan(Sizes::LEN64BIT, result,
                                        il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)),
            sat_pos_true, sat_pos_false));

    il.MarkLabel(sat_pos_true);  // Saturated positive result
    il.AddInstruction(  // reg2 set to maximum positive word-sized value,
                        // 0x7FFFFFFF
        il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                       il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(sat_pos_false);  // Result NOT saturated
    il.AddInstruction(  // Store result just like normal subtract operation
        il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                       result));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(done);

    len = Sizes::LEN16BIT;
    return true;
  }
}

bool Lift_I_MULH_SXH(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  if (reg2 == Registers::R0) {
    // Sign extend halfword
    // sxh reg1
    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, reg1,
        il.SignExtend(
            Sizes::LEN32BIT,
            il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                   il.Const(Sizes::LEN32BIT, 0xFFFF)))));
    len = Sizes::LEN16BIT;
    return true;

  } else {
    // Mulh
    // Multiplies lower half-word of reg2 by half-word of reg2 and store in reg2
    // as word
    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, reg2,
        il.Mult(Sizes::LEN32BIT,  // TODO: should double precision be used
                                  // instead? idk...
                il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                       il.Const(Sizes::LEN32BIT, 0xFFFF)),
                il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                       il.Const(Sizes::LEN32BIT, 0xFFFF)))));
    len = Sizes::LEN16BIT;
    return true;
  }
}

bool Lift_I_OR(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  il.AddInstruction(il.SetRegister(
      Sizes::LEN32BIT, reg2,
      il.Or(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
            il.Register(Sizes::LEN32BIT, reg2), Flags::FLAGS_WRITE_OV_S_Z)));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_I_XOR(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  il.AddInstruction(il.SetRegister(
      Sizes::LEN32BIT, reg2,
      il.Xor(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
             il.Register(Sizes::LEN32BIT, reg2), Flags::FLAGS_WRITE_OV_S_Z)));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_I_AND(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  il.AddInstruction(il.SetRegister(
      Sizes::LEN32BIT, reg2,
      il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
             il.Register(Sizes::LEN32BIT, reg2), Flags::FLAGS_WRITE_OV_S_Z)));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_I_TST(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  // Result is not stored, only the flags are changed
  il.AddInstruction(il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                           il.Register(Sizes::LEN32BIT, reg2),
                           Flags::FLAGS_WRITE_OV_S_Z));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_I_SUBR(const uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  // Subtract reverse; instead of reg2 - reg1, does reg1 - reg2
  il.AddInstruction(
      il.SetRegister(Sizes::LEN32BIT, reg2,
                     il.Sub(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                            il.Register(Sizes::LEN32BIT, reg2),
                            Flags::FLAGS_WRITE_CY_OV_S_Z)));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_I_CMP(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  // Compare, reg2 - reg1; data in registers not affected, only flags are
  // changed
  il.AddInstruction(il.Sub(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                           il.Register(Sizes::LEN32BIT, reg1),
                           Flags::FLAGS_WRITE_CY_OV_S_Z));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_I_SUB(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  il.AddInstruction(
      il.SetRegister(Sizes::LEN32BIT, reg2,
                     il.Sub(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                            il.Register(Sizes::LEN32BIT, reg1),
                            Flags::FLAGS_WRITE_CY_OV_S_Z)));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_I_ADD(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg1 = ExtractReg1OpcodeField(opcode);

  il.AddInstruction(
      il.SetRegister(Sizes::LEN32BIT, reg2,
                     il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                            il.Register(Sizes::LEN32BIT, reg1),
                            Flags::FLAGS_WRITE_CY_OV_S_Z)));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_II(const uint64_t opcode, uint64_t addr, size_t &len,
             BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  uint8_t imm5 = static_cast<int8_t>(
      opcode & OpcodeFields::MASK_II_IMM5);  // no shift needed

  if (opcode & OpcodeFields::OPCODE_BIT_4) {    // Opcodes starting with 0b0101
    if (opcode & OpcodeFields::OPCODE_BIT_5) {  // Opcodes starting with 0b01011
      if (opcode &
          OpcodeFields::OPCODE_BIT_6) {  // Opcode 0b010111; format II mulh
        // Multiplies lower half-word of reg2 by 5-bit immediate
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.MultDoublePrecSigned(
                Sizes::LEN32BIT,
                il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                       il.Const(Sizes::LEN32BIT, 0xFFFF)),
                il.SignExtend(Sizes::LEN32BIT,
                              il.Const(Sizes::LEN8BIT, imm5)))));
        len = Sizes::LEN16BIT;
        return true;

      } else {  // Opcode 0b010110; format II shl
        // Logical shift reg2 left by imm5
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.ShiftLeft(
                Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN16BIT, imm5)),
                Flags::FLAGS_WRITE_CY_OV_S_Z)));
        len = Sizes::LEN16BIT;
        return true;
      }
    } else {  // Opcodes starting with 0b01010
      if (opcode &
          OpcodeFields::OPCODE_BIT_6) {  // Opcode 0b010101; format II sar
        // Arithmetic shift reg2 right by imm5
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.ArithShiftRight(
                Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN8BIT, imm5)),
                Flags::FLAGS_WRITE_CY_OV_S_Z)));
        len = Sizes::LEN16BIT;
        return true;

      } else {  // Opcode 0b010100; format II shr
        // Logical shift reg2 right by imm5
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.LogicalShiftRight(
                Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN8BIT, imm5)),
                Flags::FLAGS_WRITE_CY_OV_S_Z)));
        len = Sizes::LEN16BIT;
        return true;
      }
    }
  } else {                                      // Opcodes starting with 0b0100
    if (opcode & OpcodeFields::OPCODE_BIT_5) {  // Opcodes starting with 0b01001
      if (opcode &
          OpcodeFields::OPCODE_BIT_6) {  // Opcode 0b010011; format II cmp
        // Compare, reg2 - imm5; data in register is not affected, only flags
        // are changed
        il.AddInstruction(il.Sub(
            Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
            il.SignExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN8BIT, imm5)),
            Flags::FLAGS_WRITE_CY_OV_S_Z));
        len = Sizes::LEN16BIT;
        return true;

      } else {  // Opcode 0b010010; format II add
        // reg2 = reg2 + imm5
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.Add(
                Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                il.SignExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN8BIT, imm5)),
                Flags::FLAGS_WRITE_CY_OV_S_Z)));
        len = Sizes::LEN16BIT;
        return true;
      }
    } else {  // Opcodes starting with 0b01000; includes callt (5-bit opcode),
              // satadd, and mov
      // Opcode 0b01000; format II callt, with reg2 == r0
      // Opcode 0b010001; format II satadd
      // Opcode 0b010000; format II mov
      if (reg2 == Registers::R0) {  // Callt
        // Call with table lookup
        // Text format: callt imm6
        auto imm6 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_II_IMM6);

        uint32_t return_pc = static_cast<uint32_t>(addr) + Sizes::LEN16BIT;
        uint32_t ctpc =
            Registers::SYSTEM_REG_BASE +
            Registers::V850_REG_CTPC *
                Registers::REGISTER_SIZE;  // TODO maybe change the defines to
                                           // just straight up have all the
                                           // addresses not do this maths
        uint32_t ctpsw = Registers::SYSTEM_REG_BASE +
                         Registers::V850_REG_CTPSW * Registers::REGISTER_SIZE;
        uint32_t psw = Registers::SYSTEM_REG_BASE +
                       Registers::V850_REG_PSW * Registers::REGISTER_SIZE;
        uint32_t ctbp = Registers::SYSTEM_REG_BASE +
                        Registers::V850_REG_CTBP * Registers::REGISTER_SIZE;

        il.AddInstruction(  // ctpc <- pc + 2
            il.Store(Sizes::LEN32BIT, il.ConstPointer(Sizes::LEN32BIT, ctpc),
                     il.Const(Sizes::LEN32BIT, return_pc)));

        il.AddInstruction(  // ctpsw <- psw
            il.Store(Sizes::LEN32BIT, il.ConstPointer(Sizes::LEN32BIT, ctpsw),
                     il.Load(Sizes::LEN32BIT,
                             il.ConstPointer(Sizes::LEN32BIT, psw))));
        // adr <- ctbp + ZeroExtend(imm6 << 1)
        // pc <- ctbp + ZeroExtend(LoadMemory(addr, Halfword))
        il.AddInstruction(il.Jump(il.Add(
            Sizes::LEN32BIT,
            il.Load(Sizes::LEN32BIT, il.ConstPointer(Sizes::LEN32BIT, ctbp)),
            // il.Load(Sizes::LEN32BIT, addr))));
            il.Add(Sizes::LEN32BIT,
                   il.Load(Sizes::LEN32BIT,
                           il.ConstPointer(Sizes::LEN32BIT, ctbp)),
                   il.Const(Sizes::LEN32BIT, imm6 << 1)))));

        len = Sizes::LEN16BIT;
        return true;

      } else {
        if (opcode & OpcodeFields::OPCODE_BIT_6) {
          // Saturated add 5-bit immediate, format II
          // Text format: satadd imm5, reg2

          // If result exceeds max pos/neg, store saturated value
          // (7FFFFFFF/80000000) in reg2 and set SAT flag NOTE: Once the result
          // of a sat operation is saturated, the SAT flag remains set even if
          // the result of the subsequent ops is not saturated. SAT flag is only
          // reset by loading data to PSW with LDSR.
          BN::LowLevelILLabel sat_pos_true, sat_pos_false, sat_neg_true,
              sat_neg_false, done;

          BN::ExprId result = il.Add(
              Sizes::LEN64BIT,  // Calculate result of subtract operation
              il.Register(Sizes::LEN32BIT, reg2),
              il.SignExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN8BIT, imm5)),
              Flags::FLAGS_WRITE_SAT_CY_OV_S_Z);  // TODO: implement SAT flag
                                                  // behavior

          il.AddInstruction(  // Check whether maximum negative value is
                              // exceeded
              il.If(il.CompareSignedLessThan(
                        Sizes::LEN64BIT, result,
                        il.Const(Sizes::LEN32BIT, 0x80000000)),
                    sat_neg_true, sat_neg_false));

          il.MarkLabel(sat_neg_true);  // Saturated negative result
          il.AddInstruction(  // reg2 set to maximum negative word-sized value,
                              // 0x80000000
              il.SetRegister(Sizes::LEN32BIT,
                             il.Register(Sizes::LEN32BIT, reg2),
                             il.Const(Sizes::LEN32BIT, 0x80000000)));
          il.AddInstruction(il.Goto(done));

          il.MarkLabel(sat_neg_false);
          il.AddInstruction(  // Check whether maximum positive value is
                              // exceeded
              il.If(il.CompareSignedGreaterThan(
                        Sizes::LEN64BIT, result,
                        il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)),
                    sat_pos_true, sat_pos_false));

          il.MarkLabel(sat_pos_true);  // Saturated positive result
          il.AddInstruction(  // reg2 set to maximum positive word-sized value,
                              // 0x7FFFFFFF
              il.SetRegister(Sizes::LEN32BIT,
                             il.Register(Sizes::LEN32BIT, reg2),
                             il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)));
          il.AddInstruction(il.Goto(done));

          il.MarkLabel(sat_pos_false);  // Result NOT saturated
          il.AddInstruction(  // Store result just like normal subtract
                              // operation
              il.SetRegister(Sizes::LEN32BIT,
                             il.Register(Sizes::LEN32BIT, reg2), result));
          il.AddInstruction(il.Goto(done));

          il.MarkLabel(done);

          len = Sizes::LEN16BIT;
          return true;

        } else {
          // Move 5-bit immediate; format II mov
          // Text format: mov imm5, reg2
          il.AddInstruction(il.SetRegister(
              Sizes::LEN32BIT, reg2,
              il.SignExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN8BIT, imm5))));
          len = Sizes::LEN16BIT;
          return true;
        }
      }
    }
  }
  return false;
}

bool Lift_III(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  auto subop = static_cast<uint8_t>((opcode & OpcodeFields::MASK_III_OPCODE) >>
                                    OpcodeFields::SHIFT_III_OPCODE);

  /* The only format III instruction is bcond, but there are a lot of variants
   * depending on the condition code */
  if (subop != Opcodes::OP_III_4BIT_BCOND) {
    return false;
  }
  auto condition = static_cast<uint8_t>(opcode & OpcodeFields::MASK_III_COND);
  // Displacement is: SignExtend( DISP_H | DISP_L | 0 )
  int32_t displacement =
      static_cast<int16_t>(((opcode & OpcodeFields::MASK_III_DISP_H) >>
                            OpcodeFields::SHIFT_III_DISP_H) |
                           ((opcode & OpcodeFields::MASK_III_DISP_L) >>
                            OpcodeFields::SHIFT_III_DISP_L));
  // Sign-extend 9-bit displacement
  if (displacement & (1 << 8)) {
    displacement = static_cast<int32_t>(0xFFFFFF00) | displacement;
  }

  /* Get label for true branch, if one exists */
  BN::ExprId dest_if_true = il.Const(Sizes::LEN32BIT, addr + displacement);
  BNLowLevelILLabel *t = il.GetLabelForAddress(arch, dest_if_true);

  /* Determine branch instruction based on the condition code */
  BN::ExprId conditionIL;
  switch (condition) {
    // Note: where condition codes have multiple meanings, the most general one
    // is used for the mnemonic e.g., between "bz" (zero) and "be" (equal), "bz"
    // is used because it describes flag status rather than ascribing meaning to
    // the flag status
    case Conditions::CONDITION_CODE_BGT:
      conditionIL = il.FlagCondition(LLFC_SGT);
      break;
    case Conditions::CONDITION_CODE_BGE:
      conditionIL = il.FlagCondition(LLFC_SGE);
      break;
    case Conditions::CONDITION_CODE_BLT:
      conditionIL = il.FlagCondition(LLFC_SLT);
      break;
    case Conditions::CONDITION_CODE_BLE:
      conditionIL = il.FlagCondition(LLFC_SLE);
      break;
    case Conditions::CONDITION_CODE_BH:
      conditionIL = il.FlagCondition(LLFC_UGT);
      break;
    case Conditions::CONDITION_CODE_BNH:
      conditionIL = il.FlagCondition(LLFC_ULE);
      break;
    case Conditions::CONDITION_CODE_BC:  // Note: same condition code as
                                         // CONDITION_CODE_BL
      conditionIL = il.FlagCondition(LLFC_ULT);
      break;
    case Conditions::CONDITION_CODE_BNC:  // Note: same condition code as
                                          // CONDITION_CODE_BNL
      conditionIL = il.FlagCondition(LLFC_UGE);
      break;
    case Conditions::CONDITION_CODE_BV:
      conditionIL = il.FlagCondition(LLFC_O);
      break;
    case Conditions::CONDITION_CODE_BNV:
      conditionIL = il.FlagCondition(LLFC_NO);
      break;
    case Conditions::CONDITION_CODE_BN:
      conditionIL = il.FlagCondition(LLFC_NEG);
      break;
    case Conditions::CONDITION_CODE_BP:
      conditionIL = il.FlagCondition(LLFC_POS);
      break;
    case Conditions::CONDITION_CODE_BZ:  // Note: same condition code as
                                         // CONDITION_CODE_BE
      conditionIL = il.FlagCondition(LLFC_E);
      break;
    case Conditions::CONDITION_CODE_BNZ:  // Note: same condition code as
                                          // CONDITION_CODE_BNZ
      conditionIL = il.FlagCondition(LLFC_NE);
      break;

    case Conditions::CONDITION_CODE_BR:
      // Unconditional branch
      if (t) {
        il.AddInstruction(il.Goto(*t));
      } else {
        il.AddInstruction(il.Jump(dest_if_true));
      }
      len = Sizes::LEN16BIT;
      return true;

    case Conditions::CONDITION_CODE_BSA:
      // SAT == 1; Saturated
      // conditionIL = il.Flag(FLAG_SAT_SATURATED); // TODO
      UNIMPLEMENTED

    default:
      return false;
  }

  bool indirect;
  BNLowLevelILLabel true_label, false_label;
  if (t) {
    indirect = false;
    true_label = *t;

  } else {
    indirect = true;
    true_label = BN::LowLevelILLabel();
  }

  BN::ExprId dest_if_false = il.Const(Sizes::LEN32BIT, addr + Sizes::LEN16BIT);
  BNLowLevelILLabel *f = il.GetLabelForAddress(arch, dest_if_false);
  bool found_false_label;
  if (f) {
    found_false_label = true;
    false_label = *f;
  } else {
    found_false_label = false;
    false_label = BN::LowLevelILLabel();
  }

  il.AddInstruction(il.If(conditionIL, true_label, false_label));

  if (indirect) {
    il.MarkLabel(true_label);
    il.AddInstruction(il.Jump(dest_if_true));
  }

  if (!found_false_label) {
    il.MarkLabel(false_label);
  }

  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_IV_SLDB(const uint64_t opcode, uint64_t addr, size_t &len,
                  BN::LowLevelILFunction &il) {
  // Short format load byte; format IV
  // Text format: sld.b disp7[ep], reg2
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  (reg2 == Registers::R0)
      ? il.Const(Sizes::LEN32BIT, 0)
      : il.Register(Sizes::LEN32BIT,
                    reg2);  // r0 is always 0; TODO do this elsewhere too?
  auto disp7 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_IV_7BIT_DISP);

  // TODO should this be SignExtend not ZeroExtend for disp?
  il.AddInstruction(il.SetRegister(
      Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
      il.SignExtend(
          Sizes::LEN32BIT,
          il.Load(Sizes::LEN8BIT,
                  il.Add(Sizes::LEN32BIT,
                         il.Register(Sizes::LEN32BIT, Registers::EP),
                         il.ZeroExtend(Sizes::LEN32BIT,
                                       il.Const(Sizes::LEN32BIT, disp7)))))));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_IV_SSTB(const uint64_t opcode, uint64_t addr, size_t &len,
                  BN::LowLevelILFunction &il) {
  // Short format store byte; format IV
  // Text format: sst.b reg2, disp7[ep]
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  BN::ExprId reg2_il =
      (reg2 == Registers::R0)
          ? il.Const(Sizes::LEN32BIT, 0)
          : il.Register(Sizes::LEN32BIT, reg2);  // r0 is always 0
  auto disp7 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_IV_7BIT_DISP);

  il.AddInstruction(il.Store(
      Sizes::LEN8BIT,
      il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::EP),
             il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, disp7))),
      reg2_il));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_IV(const uint64_t opcode, uint64_t addr, size_t &len,
             BN::LowLevelILFunction &il) {
  auto op_iv = static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_OPCODE) >>
                                    OpcodeFields::SHIFT_IV_OPCODE);
  static_cast<uint8_t>((opcode & OpcodeFields::OPCODE_7_BITS) >>
                       OpcodeFields::SHIFT_7BIT_OPCODE);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  auto subop = static_cast<uint8_t>(opcode & OpcodeFields::MASK_IV_SUBOP);
  uint8_t disp;  // Displacement field varies depending on opcode

  // Value of r0 is always 0
  BN::ExprId reg2_il;

  switch (op_iv) {
    case Opcodes::OP_IV_4BIT_SLD_H:
      disp = static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_7BIT_DISP)
                                  << OpcodeFields::SHIFT_IV_DISP);

      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
          il.SignExtend(
              Sizes::LEN32BIT,
              il.Load(
                  Sizes::LEN16BIT,
                  il.Add(Sizes::LEN32BIT,
                         il.Register(Sizes::LEN32BIT, Registers::EP),
                         il.ZeroExtend(Sizes::LEN32BIT,
                                       il.Const(Sizes::LEN32BIT, disp)))))));
      len = Sizes::LEN16BIT;
      return true;

    case Opcodes::OP_IV_4BIT_SST_H:
      disp = static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_7BIT_DISP)
                                  << OpcodeFields::SHIFT_IV_DISP);

      reg2_il = (reg2 == Registers::R0) ? il.Const(Sizes::LEN16BIT, 0)
                                        : il.Register(Sizes::LEN16BIT, reg2);
      il.AddInstruction(il.Store(
          Sizes::LEN16BIT,
          il.Add(
              Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::EP),
              il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, disp))),
          reg2_il));

      len = Sizes::LEN16BIT;
      return true;

    case Opcodes::OP_IV_4BIT_SLD_W_OR_SST_W:
      if (subop == Opcodes::SUBOP_IV_SLD_W) {
        disp = static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_6BIT_DISP)
                                    << OpcodeFields::SHIFT_IV_DISP);

        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
            il.SignExtend(
                Sizes::LEN32BIT,
                il.Load(
                    Sizes::LEN32BIT,
                    il.Add(Sizes::LEN32BIT,
                           il.Register(Sizes::LEN32BIT, Registers::EP),
                           il.ZeroExtend(Sizes::LEN32BIT,
                                         il.Const(Sizes::LEN32BIT, disp)))))));
        len = Sizes::LEN16BIT;
        return true;

      } else if (subop == Opcodes::SUBOP_IV_SST_W) {
        disp = static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_6BIT_DISP)
                                    << OpcodeFields::SHIFT_IV_DISP);

        reg2_il = (reg2 == Registers::R0) ? il.Const(Sizes::LEN32BIT, 0)
                                          : il.Register(Sizes::LEN32BIT, reg2);
        il.AddInstruction(il.Store(
            Sizes::LEN32BIT,
            il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::EP),
                   il.ZeroExtend(Sizes::LEN32BIT,
                                 il.Const(Sizes::LEN32BIT, disp))),
            reg2_il));
        len = Sizes::LEN16BIT;
        return true;
      }

    default:
      return false;
  }
}

bool Lift_V_JARL_JR_VII_LDBU_XIII_PREPARE(const uint64_t opcode,
                                          const uint64_t addr, size_t &len,
                                          BN::LowLevelILFunction &il,
                                          BinaryNinja::Architecture *arch) {
  uint8_t subop = opcode >> 16 & OpcodeFields::MASK_V_SUBOP_BIT_16;

  if (subop == Opcodes::SUBOP_V_JARL_JR) {  // jarl, jr
    auto reg2 = static_cast<uint8_t>((opcode & OpcodeFields::MASK_REG2) >>
                                     OpcodeFields::SHIFT_REG2);
    auto disp22 =
        (int32_t)(((opcode & OpcodeFields::MASK_V_DISP_H)
                   << OpcodeFields::SHIFT_V_DISP_H) |
                  (opcode >> 16 &
                   OpcodeFields::MASK_V_DISP_L));  // Bit 0 of displacement
                                                   // masked to 0

    if (disp22 & (1 << 21)) {  // Sign extend; MS bit is set
      disp22 = (0b1111111111 << 22) | disp22;
    }

    const uint32_t target = addr + disp22;
    BN::ExprId dest =
        il.ConstPointer(Sizes::LEN32BIT,  // Calculate jump target
                        il.Add(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, addr),
                               il.Const(Sizes::LEN32BIT, disp22)));

    if (reg2 == Registers::R0) {  // if reg2 is r0, is jr
      // Jump relative
      // Text format: jr disp22
      il.AddInstruction(il.Jump(dest));

      len = Sizes::LEN32BIT;
      return true;

    } else {
      // Jump and register link
      // Text format: jarl disp22, reg2
      il.AddInstruction(  // Save PC in reg2
          il.SetRegister(
              Sizes::LEN32BIT, reg2,
              il.Const(Sizes::LEN32BIT,
                       il.Add(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, addr),
                              il.Const(Sizes::LEN32BIT, Sizes::LEN32BIT)))));

      if (target == (addr + Sizes::LEN32BIT)) {
        /* JARL used to set up a long range call via jump */
        BNLowLevelILLabel *label = il.GetLabelForAddress(arch, dest);
        if (!label) {
          il.AddInstruction(il.Jump(dest));
        } else {
          il.AddInstruction(il.Goto(*label));
        }
        len = Sizes::LEN32BIT;
        return true;

      } else {
        il.AddInstruction(il.Call(dest));

        len = Sizes::LEN32BIT;
        return true;
      }
    }
  } else {
    subop = opcode >> 16 & Opcodes::MASK_XIII_SUBOP_PREPARE;
    if (subop == Opcodes::SUBOP_XIII_PREPARE_001) {
      // Function prepare; generate stack frame
      // Text format: prepare list12, imm5

      // Store specified general purpose registers (r20-r31) on the stack in
      // ascending order, e.g., push r20, push r21, etc.
      GenerateILToSaveRegisters(opcode, il);

      // sp = sp - ZeroExtend(imm5 << 2)
      auto imm5 =
          static_cast<uint8_t>((opcode >> 16 & OpcodeFields::MASK_XIII_IMM5)
                               << OpcodeFields::SHIFT_XIII_IMM5);
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, Registers::SP,
          il.Sub(Sizes::LEN64BIT, il.Register(Sizes::LEN32BIT, Registers::SP),
                 il.Const(Sizes::LEN32BIT, imm5))));

      len = Sizes::LEN32BIT;
      return true;

    } else if (subop == Opcodes::SUBOP_XIII_PREPARE_011) {
      // Function prepare
      // Text format: prepare list12, imm5, sp/imm

      // Store specified general purpose registers (r20-r31) on the stack in
      // ascending order, e.g., push r20, push r21, etc.
      GenerateILToSaveRegisters(opcode, il);

      int32_t imm = 0;

      // sp = sp - ZeroExtend(imm5 << 2)
      const auto imm5 =
          static_cast<uint8_t>((opcode >> 16 & OpcodeFields::MASK_XIII_IMM5)
                               << OpcodeFields::SHIFT_XIII_IMM5);
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, Registers::SP,
          il.Sub(Sizes::LEN64BIT, il.Register(Sizes::LEN32BIT, Registers::SP),
                 il.Const(Sizes::LEN32BIT, imm5))));

      auto prepare_format = static_cast<uint8_t>(
          opcode >> 16 & Opcodes::MASK_XIII_PREPARE_FORMAT);
      switch (prepare_format) {
        case Opcodes::PREPARE_LOAD_SP:

          // EP = SP
          il.AddInstruction(
              il.SetRegister(Sizes::LEN32BIT, Registers::EP,
                             il.Register(Sizes::LEN32BIT, Registers::SP)));

          len = Sizes::LEN32BIT;
          return true;

        case Opcodes::PREPARE_LOAD_SIGN_EXTENDED_IMM16:
          // Sign-extend 16-bit immediate to 32-bit // TODO make functions and
          // use everywhere
          if (imm & (1 << 15)) {  // MSB is set
            imm = static_cast<int32_t>(0xFFFF0000) | opcode >> 48;
          } else {  // MSB is not set
            imm = 0x00000000 | opcode >> 48;
          }

          len = Sizes::LEN48BIT;
          break;

        case Opcodes::PREPARE_LOAD_LSL_IMM16:
          // Logically shift 16-bit immediate left by 16
          imm = (int32_t)(opcode >> 48 << 16);

          len = Sizes::LEN48BIT;
          break;

        case Opcodes::PREPARE_LOAD_IMM32:
          // 32-bit immediate, formed from bits 32-63 of the instruction
          imm = (int32_t)((opcode >> 48 << 16) | opcode >> 48);

          len = Sizes::LEN64BIT;
          break;
        default:
          return false;
      }
      // EP = imm
      il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, Registers::EP,
                                       il.Const(Sizes::LEN32BIT, imm)));
      return true;

    } else {  // ld.bu
      // Load byte unsigned
      // Text format: ld.bu disp16[reg1], reg2
      auto reg2 = static_cast<uint8_t>((opcode & OpcodeFields::MASK_REG2) >>
                                       OpcodeFields::SHIFT_REG2);
      auto reg1 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_REG1);
      uint16_t disp16 = ((opcode >> 16 & OpcodeFields::MASK_VII_DISP) |
                         ((opcode & OpcodeFields::MASK_VII_DISP0) >>
                          OpcodeFields::SHIFT_VII_DISP0));

      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
          il.ZeroExtend(
              Sizes::LEN32BIT,
              il.Load(
                  Sizes::LEN8BIT,
                  il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                         il.SignExtend(Sizes::LEN32BIT,
                                       il.Const(Sizes::LEN32BIT, disp16)))))));
      len = Sizes::LEN32BIT;
      return true;
    }
  }
}

bool Lift_VI_MOVHI_SATSUBI_XIII_DISPOSE(const uint64_t opcode, uint64_t addr,
                                        size_t &len,
                                        BN::LowLevelILFunction &il) {
  const auto reg2 = ExtractReg2OpcodeField(opcode);

  if (reg2 == Registers::R0) {
    // Function dispose; has several forms
    auto imm5 =
        static_cast<uint8_t>((opcode >> 16 & OpcodeFields::MASK_XIII_IMM5) >>
                             OpcodeFields::SHIFT_XIII_IMM5);
    auto reg1 = static_cast<uint8_t>(opcode >> 16 & OpcodeFields::MASK_REG1);

    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, Registers::SP,
        il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::SP),
               il.Const(Sizes::LEN32BIT, imm5 << 2))));

    GenerateILToRestoreRegisters(opcode, il);

    if (reg1 != 0) {
      // Text format: dispose imm5, list12, [reg1]
      il.AddInstruction(il.Return(il.Register(Sizes::LEN32BIT, reg1)));

    }  // Else, text format: dispose imm5, list12

    len = Sizes::LEN32BIT;
    return true;

  } else {
    auto reg1 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_REG1);
    auto imm16 = opcode >> 16 & OpcodeFields::MASK_VI_IMM;
    auto imm_hi = imm16 << 16;

    BN::ExprId reg1_il = (reg1 == Registers::R0)
                             ? il.Const(Sizes::LEN32BIT, 0)
                             : il.Register(Sizes::LEN32BIT, reg1);

    auto opcode_6 =
        static_cast<uint8_t>((opcode & OpcodeFields::OPCODE_6_BITS) >>
                             OpcodeFields::SHIFT_6BIT_OPCODE);
    if (opcode_6 == Opcodes::OP_VI_6BIT_MOVHI) {
      // Move high halfword
      // Text format: movhi imm16, reg1, reg2
      // reg2 = reg1 + (imm << 16)
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.Add(Sizes::LEN32BIT, reg1_il,
                 il.Const(Sizes::LEN32BIT,
                          imm_hi))));  // TODO what if reg2 == r0?
      len = Sizes::LEN32BIT;
      return true;

    } else if (opcode_6 == Opcodes::OP_VI_6BIT_SATSUBI) {
      // Saturated subtract 16-bit immediate
      // Text format: satsubi imm16, reg1, reg2
      // Satsubi: reg2 = saturated(reg1 - sign-extend(imm))

      // If result exceeds max pos/neg, store saturated value
      // (7FFFFFFF/80000000) in reg2 and set SAT flag NOTE: Once the result of a
      // sat operation is saturated, the SAT flag remains set even if the result
      // of the subsequent ops is not saturated. SAT flag is only reset by
      // loading data to PSW with LDSR.
      BN::LowLevelILLabel sat_pos_true, sat_pos_false, sat_neg_true,
          sat_neg_false, done;

      BN::ExprId result = il.Sub(
          Sizes::LEN32BIT, reg1_il,
          il.SignExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, imm16)),
          Flags::FLAGS_WRITE_SAT_CY_OV_S_Z);  // TODO: implement SAT flag
                                              // behavior

      il.AddInstruction(  // Check whether maximum negative value is exceeded
          il.If(il.CompareSignedLessThan(Sizes::LEN64BIT, result,
                                         il.Const(Sizes::LEN32BIT, 0x80000000)),
                sat_neg_true, sat_neg_false));

      il.MarkLabel(sat_neg_true);  // Saturated negative result
      il.AddInstruction(  // reg2 set to maximum negative word-sized value,
                          // 0x80000000
          il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                         il.Const(Sizes::LEN32BIT, 0x80000000)));
      il.AddInstruction(il.Goto(done));

      il.MarkLabel(sat_neg_false);
      il.AddInstruction(  // Check whether maximum positive value is exceeded
          il.If(il.CompareSignedGreaterThan(
                    Sizes::LEN64BIT, result,
                    il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)),
                sat_pos_true, sat_pos_false));

      il.MarkLabel(sat_pos_true);  // Saturated positive result
      il.AddInstruction(  // reg2 set to maximum positive word-sized value,
                          // 0x7FFFFFFF
          il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                         il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)));
      il.AddInstruction(il.Goto(done));

      il.MarkLabel(sat_pos_false);  // Result NOT saturated
      il.AddInstruction(  // Store result just like normal subtract operation
          il.SetRegister(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                         result));
      il.AddInstruction(il.Goto(done));

      il.MarkLabel(done);

      len = Sizes::LEN32BIT;
      return true;
    }
    return false;
  }
}

bool Lift_VI(const uint64_t opcode, uint64_t addr, size_t &len,
             BN::LowLevelILFunction &il) {
  auto subop = static_cast<uint8_t>((opcode & OpcodeFields::MASK_VI_OPCODE) >>
                                    OpcodeFields::SHIFT_VI_OPCODE);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  auto reg1 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_REG1);
  auto imm = opcode >> 16 & OpcodeFields::MASK_VI_IMM;
  auto imm32 = static_cast<uint32_t>((opcode >> 32 << 16) | opcode >> 16);

  BN::ExprId reg1_il = (reg1 == Registers::R0)
                           ? il.Const(Sizes::LEN32BIT, 0)
                           : il.Register(Sizes::LEN32BIT, reg1);

  switch (subop) {
    case Opcodes::OP_VI_6BIT_ADDI:
      // Add immediate: reg2 = reg1 + imm
      if (reg2 == Registers::R0) {
        il.AddInstruction(il.Add(
            Sizes::LEN32BIT, reg1_il,
            il.SignExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, imm)),
            Flags::FLAGS_WRITE_CY_OV_S_Z));

      } else {
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.Add(
                Sizes::LEN32BIT, reg1_il,
                il.SignExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, imm)),
                Flags::FLAGS_WRITE_CY_OV_S_Z)));
      }
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VI_6BIT_MOVEA_OR_MOV:
      if (reg2 == Registers::R0) {
        // Move 32-bit immediate
        // Format: mov imm32, reg1
        il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg1,
                                         il.Const(Sizes::LEN32BIT, imm32)));
        len = Sizes::LEN48BIT;  // Format VI mov is 48-bit; it is the
                                // only 48-bit instruction
        return true;

      } else {
        // Move effective address (basically the same as addi but doesn't set
        // flags): reg2 = reg1 + imm Format: movea imm16, reg1, reg2
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.Add(Sizes::LEN32BIT, reg1_il,
                   il.SignExtend(Sizes::LEN32BIT,
                                 il.Const(Sizes::LEN32BIT, imm)))));
        len = Sizes::LEN32BIT;
        return true;
      }

    case Opcodes::OP_VI_6BIT_ORI:
      // reg2 = reg1 | zero-extend(imm)
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.Or(Sizes::LEN32BIT, reg1_il,
                il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, imm),
                              Flags::FLAGS_WRITE_OV_S_Z))));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VI_6BIT_XORI:
      // reg2 = reg1 ^ zero-extend(imm)
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.Xor(Sizes::LEN32BIT, reg1_il,
                 il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, imm),
                               Flags::FLAGS_WRITE_OV_S_Z))));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VI_6BIT_ANDI:
      // reg2 = reg1 & zero-extend(imm)
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.And(Sizes::LEN32BIT, reg1_il,
                 il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, imm),
                               Flags::FLAGS_WRITE_OV_S_Z))));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VI_6BIT_MULHI:
      // reg2 = lower-half-of(reg1) * imm
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.MultDoublePrecSigned(
              Sizes::LEN32BIT,
              il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                     il.Const(Sizes::LEN32BIT, 0xFFFF)),
              il.Const(Sizes::LEN32BIT, imm))));
      len = Sizes::LEN32BIT;
      return true;

    default:
      return false;
  }
}

bool Lift_VII(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il) {
  auto op_vii = static_cast<uint8_t>((opcode & OpcodeFields::MASK_VII_OPCODE) >>
                                     OpcodeFields::SHIFT_VII_OPCODE);
  auto subop =
      static_cast<uint8_t>(opcode >> 16 & OpcodeFields::MASK_VII_SUBOP);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  auto reg1 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_REG1);
  int16_t disp;

  switch (op_vii) {
    case Opcodes::OP_VII_6BIT_LD_B:
      disp = static_cast<int16_t>(opcode >> 16);
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
          il.SignExtend(
              Sizes::LEN32BIT,
              il.Load(
                  Sizes::LEN8BIT,
                  il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                         il.SignExtend(Sizes::LEN32BIT,
                                       il.Const(Sizes::LEN32BIT, disp)))))));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VII_6BIT_LD_H_OR_LD_W:
      if (subop == Opcodes::SUBOP_VII_LD_H) {
        disp =
            static_cast<int16_t>(opcode >> 16 & OpcodeFields::MASK_VII_DISP_H);
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
            il.SignExtend(
                Sizes::LEN32BIT,
                il.Load(
                    Sizes::LEN16BIT,
                    il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                           il.SignExtend(Sizes::LEN32BIT,
                                         il.Const(Sizes::LEN32BIT, disp)))))));
        len = Sizes::LEN32BIT;
        return true;

      } else if (subop == Opcodes::SUBOP_VII_LD_W) {
        disp =
            static_cast<int16_t>(opcode >> 16 & OpcodeFields::MASK_VII_DISP_W);
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
            il.SignExtend(
                Sizes::LEN32BIT,
                il.Load(
                    Sizes::LEN32BIT,
                    il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                           il.SignExtend(Sizes::LEN32BIT,
                                         il.Const(Sizes::LEN32BIT, disp)))))));
        len = Sizes::LEN32BIT;
        return true;
      }

    case Opcodes::OP_VII_6BIT_ST_B:
      disp = static_cast<int16_t>(opcode >> 16);
      il.AddInstruction(il.Store(
          Sizes::LEN8BIT,
          il.Add(
              Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
              il.SignExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, disp))),
          il.Register(Sizes::LEN32BIT, reg2)));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VII_6BIT_ST_H_OR_ST_W:
      if (subop == Opcodes::SUBOP_VII_ST_H) {
        disp =
            static_cast<int16_t>(opcode >> 16 & OpcodeFields::MASK_VII_DISP_H);
        il.AddInstruction(
            il.Store(Sizes::LEN16BIT,
                     il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                            il.SignExtend(Sizes::LEN32BIT,
                                          il.Const(Sizes::LEN32BIT, disp))),
                     il.Register(Sizes::LEN16BIT, reg2)));
        len = Sizes::LEN32BIT;
        return true;

      } else if (subop == Opcodes::SUBOP_VII_ST_W) {
        disp =
            static_cast<int16_t>(opcode >> 16 & OpcodeFields::MASK_VII_DISP_W);
        il.AddInstruction(
            il.Store(Sizes::LEN32BIT,
                     il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                            il.SignExtend(Sizes::LEN32BIT,
                                          il.Const(Sizes::LEN32BIT, disp))),
                     il.Register(Sizes::LEN32BIT, reg2)));
        len = Sizes::LEN32BIT;
        return true;
      }
    default:
      return false;
  }
}

bool Lift_VIII(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il) {
  auto op_viii =
      static_cast<uint8_t>((opcode & OpcodeFields::MASK_VIII_OPCODE) >>
                           OpcodeFields::SHIFT_VIII_OPCODE);
  auto subop = static_cast<uint8_t>((opcode & OpcodeFields::MASK_VIII_SUBOP) >>
                                    OpcodeFields::SHIFT_VIII_SUBOP);
  auto reg1 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_REG1);
  auto bitNum =
      static_cast<uint8_t>((opcode & OpcodeFields::MASK_VIII_BITNUM) >>
                           OpcodeFields::SHIFT_VIII_BITNUM);
  BN::ExprId bitmask;
  auto disp = static_cast<int16_t>(opcode >> 16);

  if (op_viii != Opcodes::OP_VIII_6BIT_BIT_MANIPULATION) {
    len = Sizes::LEN16BIT;
    return false;
  }

  len = Sizes::LEN32BIT;
  BN::ExprId addrIL =
      il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
             il.Const(Sizes::LEN32BIT, disp));

  switch (subop) {
    case Opcodes::SUBOP_SET1:
      il.AddInstruction(il.Store(
          Sizes::LEN8BIT, addrIL,
          il.Or(Sizes::LEN32BIT, il.Load(Sizes::LEN8BIT, addrIL),
                il.ShiftLeft(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, 1),
                             il.Const(Sizes::LEN8BIT, bitNum))),
          Flags::FLAGS_WRITE_Z));  // TODO need to implement special behavior
                                   // for Z flag here or maybe set manually,
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::SUBOP_CLR1:
      il.AddInstruction(il.Store(
          Sizes::LEN8BIT, addrIL,
          il.And(
              Sizes::LEN32BIT, il.Load(Sizes::LEN8BIT, addrIL),
              il.Not(Sizes::LEN32BIT,
                     il.ShiftLeft(Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, 1),
                                  il.Const(Sizes::LEN8BIT, bitNum))),
              Flags::FLAGS_WRITE_Z)));  // TODO need to implement special
                                        // behavior for Z flag here or maybe set
      // TODO ???il.AddInstruction(il.FlagBit(1, FLAG_Z_ZERO, ))

      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::SUBOP_NOT1:
      // Bitwise not operation
      // not1 bit#3, disp16[reg1]
      bitmask = il.ShiftLeft(
          Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, 1),
          il.Const(Sizes::LEN8BIT, bitNum));  // Mask for selected bit only
      il.AddInstruction(il.Store(
          Sizes::LEN8BIT, addrIL,
          il.Or(
              Sizes::LEN32BIT,  // Combine rest of byte with the modified bit
              il.And(Sizes::LEN32BIT,  // Get all the bits that are NOT selected
                     il.Load(Sizes::LEN8BIT, addrIL),
                     il.Not(Sizes::LEN32BIT, bitmask)),
              il.And(
                  Sizes::LEN32BIT,  // Get the modified bit that IS selected
                  il.Not(
                      Sizes::LEN32BIT,  // Not operation on that single bit
                      il.And(Sizes::LEN32BIT,  // Mask to get selected bit only
                             il.Load(Sizes::LEN8BIT, addrIL), bitmask),
                      Flags::FLAGS_WRITE_Z),  // TODO sanity check, is this the
                                              // right place for Z flag?
                  bitmask))));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::SUBOP_TST1:
      // Bitwise test operation
      // tst1 bit#3, disp16[reg1]
      bitmask = il.ShiftLeft(
          Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, 1),
          il.Const(Sizes::LEN8BIT, bitNum));  // Mask for selected bit only
      il.AddInstruction(
          il.Not(Sizes::LEN32BIT,         // Not operation on that single bit
                 il.And(Sizes::LEN32BIT,  // Mask to get selected bit only
                        il.Load(Sizes::LEN8BIT, addrIL), bitmask),
                 Flags::FLAGS_WRITE_Z));  // TODO sanity check, is this
                                          // the right place for Z flag?
      len = Sizes::LEN32BIT;
      return true;

    default:
      return false;
  }
}

bool Format_Ext_Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il) {
  if ((opcode >> 16 & OpcodeFields::MASK_VII_SUBOP) ==
      Opcodes::SUBOP_VII_LD_BU_HU) {
    // ld.hu: load halfword unsigned, actually a format VII
    auto reg2 = static_cast<uint8_t>((opcode & OpcodeFields::MASK_REG2) >>
                                     OpcodeFields::SHIFT_REG2);
    auto reg1 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_REG1);
    uint16_t disp = opcode >> 16 & OpcodeFields::MASK_VII_DISP_H;

    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
        il.ZeroExtend(
            Sizes::LEN32BIT,
            il.Load(Sizes::LEN16BIT,
                    il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                           il.SignExtend(Sizes::LEN32BIT,
                                         il.Const(Sizes::LEN32BIT, disp)))))));
    len = Sizes::LEN32BIT;
    return true;
  }

  // Probably TODO move these to places as appropriate
  const auto reg1 = ExtractReg1OpcodeField(opcode);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg3 = ExtractReg3OpcodeField(opcode);

  auto imm5 = (opcode & OpcodeFields::MASK_XII_IMM9_LOW_OR_IMM5);
  auto imm9 =
      static_cast<int16_t>(((opcode >> 16 & OpcodeFields::MASK_XII_IMM9_HI)
                            << OpcodeFields::SHIFT_LEFT_XII_IMM9_HI) |
                           imm5);

  // separate functions ughhhh

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
            // reg3 <- reg2[7:0] | reg2[15:8] | reg2[26:16] | reg2[31:24]
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg3,
                il.Or(
                    Sizes::LEN32BIT,
                    il.Or(
                        Sizes::LEN32BIT,
                        il.ShiftLeft(Sizes::LEN32BIT,
                                     il.And(Sizes::LEN32BIT,  // reg2[7:0]
                                            il.Register(Sizes::LEN32BIT, reg2),
                                            il.Const(Sizes::LEN32BIT, 0xFF)),
                                     il.Const(Sizes::LEN64BIT, 24)),
                        il.ShiftLeft(Sizes::LEN32BIT,
                                     il.And(Sizes::LEN32BIT,  // reg2[15:8]
                                            il.Register(Sizes::LEN32BIT, reg2),
                                            il.Const(Sizes::LEN32BIT, 0xFF00)),
                                     il.Const(Sizes::LEN64BIT, 8))),
                    il.Or(Sizes::LEN32BIT,
                          il.LogicalShiftRight(
                              Sizes::LEN32BIT,
                              il.And(Sizes::LEN32BIT,  // reg2[23:16]
                                     il.Register(Sizes::LEN32BIT, reg2),
                                     il.Const(Sizes::LEN32BIT, 0xFF0000)),
                              il.Const(Sizes::LEN64BIT, 8)),
                          il.LogicalShiftRight(
                              Sizes::LEN32BIT,
                              il.And(Sizes::LEN32BIT,  // reg2[31:24]
                                     il.Register(Sizes::LEN32BIT, reg2),
                                     il.Const(Sizes::LEN32BIT, 0xFF000000)),
                              il.Const(Sizes::LEN64BIT, 24))))));
            // TODO note: the flag behavior for this instruction is real weird
            len = Sizes::LEN32BIT;
            return true;

          case Opcodes::SUBOP_XII_BSH:
            // Byte swap halfword; for endian translation
            // Text format: bsh reg2, reg3

            // reg3 <- reg2[26:16] | reg2[31:24] | reg2[7:0] | reg2[15:8]
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg3,
                il.Or(
                    Sizes::LEN32BIT,
                    il.Or(Sizes::LEN32BIT,
                          il.LogicalShiftRight(
                              Sizes::LEN32BIT,
                              il.And(Sizes::LEN32BIT,  // reg2[23:16]
                                     il.Register(Sizes::LEN32BIT, reg2),
                                     il.Const(Sizes::LEN32BIT, 0xFF0000)),
                              il.Const(Sizes::LEN64BIT, 8)),
                          il.LogicalShiftRight(
                              Sizes::LEN32BIT,
                              il.And(Sizes::LEN32BIT,  // reg2[31:24]
                                     il.Register(Sizes::LEN32BIT, reg2),
                                     il.Const(Sizes::LEN32BIT, 0xFF000000)),
                              il.Const(Sizes::LEN64BIT, 24))),
                    il.Or(
                        Sizes::LEN32BIT,
                        il.ShiftLeft(Sizes::LEN32BIT,
                                     il.And(Sizes::LEN32BIT,  // reg2[7:0]
                                            il.Register(Sizes::LEN32BIT, reg2),
                                            il.Const(Sizes::LEN32BIT, 0xFF)),
                                     il.Const(Sizes::LEN64BIT, 24)),
                        il.ShiftLeft(Sizes::LEN32BIT,
                                     il.And(Sizes::LEN32BIT,  // reg2[15:8]
                                            il.Register(Sizes::LEN32BIT, reg2),
                                            il.Const(Sizes::LEN32BIT, 0xFF00)),
                                     il.Const(Sizes::LEN64BIT, 8))))));
            // TODO note: the flag behavior for this instruction is real weird
            len = Sizes::LEN32BIT;
            return true;

          case Opcodes::SUBOP_XII_HSW:
            // Halfword swap word; for endian translation
            // Text format: hsw reg2, reg3

            // reg3 <- reg2[15:0] | reg2[31:16]
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg3,
                il.Or(Sizes::LEN32BIT,
                      il.ShiftLeft(Sizes::LEN32BIT,
                                   il.And(Sizes::LEN32BIT,  // reg2[15:0]
                                          il.Register(Sizes::LEN32BIT, reg2),
                                          il.Const(Sizes::LEN32BIT, 0xFFFF)),
                                   il.Const(Sizes::LEN64BIT, 16)),
                      il.LogicalShiftRight(
                          Sizes::LEN32BIT,
                          il.And(Sizes::LEN32BIT,  // reg2[31:16]
                                 il.Register(Sizes::LEN32BIT, reg2),
                                 il.Const(Sizes::LEN32BIT, 0xFFFF0000)),
                          il.Const(Sizes::LEN64BIT, 16)))));
            // TODO note: the flag behavior for this instruction is real weird
            len = Sizes::LEN32BIT;
            return true;
          default:
            return false;
        }
      } else {  // 01100; conditional move (two forms)
        auto condition_CMOV =
            static_cast<uint8_t>((opcode >> 16 & OpcodeFields::MASK_XI_COND) >>
                                 OpcodeFields::SHIFT_XI_COND);

        if (opcode >> 16 &
            OpcodeFields::OPCODE_BIT_6) {  // 011001, format XI cmov
          // Conditional move
          // Text format: cmov cccc, reg1, reg2, reg3
          if (il.GetExprValue(ConditionToIL(condition_CMOV, il)).value !=
              0) {  // Condition satisfied, reg3 <- reg1
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg3, il.Register(Sizes::LEN32BIT, reg1)));
          } else {  // Condition not satisfied, reg3 <- reg2
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg3, il.Register(Sizes::LEN32BIT, reg2)));
          }
          len = Sizes::LEN32BIT;
          return true;

        } else {  // 011000, format XII cmov
          // Conditional move
          // Text format: cmov ccc, imm5, reg2, reg3
          if (il.GetExprValue(ConditionToIL(condition_CMOV, il)).value != 0) {
            // Condition satisfied, reg3 <- sign_extend(imm5)
            il.AddInstruction(
                il.SetRegister(Sizes::LEN32BIT, reg3,
                               il.SignExtend(Sizes::LEN32BIT,
                                             il.Const(Sizes::LEN8BIT, imm5))));
          } else {  // Condition not satisfied, reg3 <- reg2
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg3, il.Register(Sizes::LEN32BIT, reg2)));
          }
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
            il.AddInstruction(  // Get quotient and store in reg2
                il.SetRegister(
                    Sizes::LEN32BIT, reg2,
                    il.DivDoublePrecUnsigned(Sizes::LEN32BIT,
                                             il.Register(Sizes::LEN32BIT, reg2),
                                             il.Register(Sizes::LEN32BIT, reg1),
                                             Flags::FLAGS_WRITE_OV_S_Z)));
            // Special case: if reg2 is same as reg3, remainder will be stored
            // in reg2/reg3 and quotient will be overwritten
            if (reg3 !=
                Registers::R0) {  // If reg3 is r0, remainder is discarded
              il.AddInstruction(il.SetRegister(
                  Sizes::LEN32BIT, reg3,
                  il.ModDoublePrecUnsigned(
                      Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                      il.Register(Sizes::LEN32BIT,
                                  reg1))));  // TODO is this a correct place to
                                             // set flags?
            }
            len = Sizes::LEN32BIT;
            return true;

          } else {  // div
            // Divide word; divide reg2 by reg1, quotient in reg2 and remainder
            // in reg3 Text format: div reg1, reg2, reg3
            il.AddInstruction(  // Get quotient and store in reg2
                il.SetRegister(
                    Sizes::LEN32BIT, reg2,
                    il.DivDoublePrecSigned(Sizes::LEN32BIT,
                                           il.Register(Sizes::LEN32BIT, reg2),
                                           il.Register(Sizes::LEN32BIT, reg1),
                                           Flags::FLAGS_WRITE_OV_S_Z)));
            // Special case: if reg2 is same as reg3, remainder will be stored
            // in reg2/reg3 and quotient will be overwritten
            if (reg3 !=
                Registers::R0) {  // If reg3 is r0, remainder is discarded
              il.AddInstruction(il.SetRegister(
                  Sizes::LEN32BIT, reg3,
                  il.ModDoublePrecSigned(
                      Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                      il.Register(Sizes::LEN32BIT,
                                  reg1))));  // TODO is this a correct place to
                                             // set flags?
            }
            len = Sizes::LEN32BIT;
            return true;
          }
        } else {  // 01010; format XI divh, divhu
          if (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) {  // divhu
            // Divide halfword unsigned; divide reg2 by lower half of reg1;
            // quotient in reg2 ad remainder in reg3 Text format: divhu reg1,
            // reg2, reg3
            il.AddInstruction(  // Get quotient and store in reg2
                il.SetRegister(
                    Sizes::LEN32BIT, reg2,
                    il.DivDoublePrecUnsigned(
                        Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                        il.And(Sizes::LEN16BIT,
                               il.Register(Sizes::LEN32BIT, reg1),
                               il.Const(
                                   Sizes::LEN16BIT,
                                   0xFFFF)),  // Mask out upper half of register
                        Flags::FLAGS_WRITE_OV_S_Z)));
            // Special case: if reg2 is same as reg3, remainder will be stored
            // in reg2/reg3 and quotient will be overwritten
            if (reg3 !=
                Registers::R0) {  // If reg3 is r0, remainder is discarded
              il.AddInstruction(il.SetRegister(
                  Sizes::LEN32BIT, reg3,
                  il.ModDoublePrecUnsigned(
                      Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                      il.And(Sizes::LEN16BIT,
                             il.Register(Sizes::LEN32BIT, reg1),
                             il.Const(Sizes::LEN16BIT, 0xFFFF)),
                      Flags::FLAGS_WRITE_OV_S_Z)));  // TODO is this a correct
                                                     // place to set flags?
            }
            // AND in the prior instruction? Should different flags be set for
            // the div vs the mod instruction to get the right result?
            // Or maybe JUST in the div part of the instruction.
            len = Sizes::LEN32BIT;
            return true;

          } else {  // divh
            // Divide halfword; divide reg2 by lower half of reg1, quotient in
            // reg2 and remainder in reg3 Text format: divh reg1, reg2, reg3
            il.AddInstruction(  // Get quotient and store in reg2
                il.SetRegister(
                    Sizes::LEN32BIT, reg2,
                    il.DivDoublePrecSigned(
                        Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                        il.And(Sizes::LEN16BIT,
                               il.Register(Sizes::LEN32BIT, reg1),
                               il.Const(
                                   Sizes::LEN16BIT,
                                   0xFFFF)),  // Mask out upper half of register
                        Flags::FLAGS_WRITE_OV_S_Z)));
            // Special case: if reg2 is same as reg3, remainder will be stored
            // in reg2/reg3 and quotient will be overwritten
            if (reg3 !=
                Registers::R0) {  // If reg3 is r0, remainder is discarded
              il.AddInstruction(il.SetRegister(
                  Sizes::LEN32BIT, reg3,
                  il.ModDoublePrecSigned(
                      Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                      il.And(Sizes::LEN16BIT,
                             il.Register(Sizes::LEN32BIT, reg1),
                             il.Const(Sizes::LEN16BIT, 0xFFFF)),
                      Flags::FLAGS_WRITE_OV_S_Z)));  // TODO is this a correct
                                                     // place to set flags?
              // AND in the prior instruction? Should different flags be set for
              // the div vs the mod instruction to get the right result?
              // Or maybe JUST in the div part of the instruction.
            }
            len = Sizes::LEN32BIT;
            return true;
          }
        }
      } else {
        BN::ExprId result;
        // 0100
        if (opcode >> 16 &
            OpcodeFields::OPCODE_BIT_5) {  // 01001; format XII mul, mulu
          if (opcode >> 16 &
              OpcodeFields::MASK_SUBOP_BIT_17) {  // Format XII mulu
            // Multiply word unsigned by 9-bit immediate
            // Text format: mulu imm9, reg2, reg3
            result = il.MultDoublePrecUnsigned(
                Sizes::LEN64BIT, il.Register(Sizes::LEN32BIT, reg2),
                il.SignExtend(Sizes::LEN32BIT,
                              il.Const(Sizes::LEN16BIT, imm9)));

          } else {  // Format XII mul
            // Multiply word by 9-bit immediate
            // Text format: mul imm9, reg2, reg3
            result = il.MultDoublePrecSigned(
                Sizes::LEN64BIT, il.Register(Sizes::LEN32BIT, reg2),
                il.SignExtend(Sizes::LEN32BIT,
                              il.Const(Sizes::LEN16BIT, imm9)));
          }

          // These instructions apply for both mulu and mul
          if (reg3 != Registers::R0) {  // If reg3 is r0, upper half of result
                                        // is discarded
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg3,
                il.LogicalShiftRight(
                    Sizes::LEN32BIT,  // Upper 32 bits of 64 bit result
                    result, il.Const(Sizes::LEN32BIT, 32))));
          }
          // Special case: if reg2 is same as reg3, higher 32 bits are stored in
          // reg2/reg3 (already done)
          if (reg2 != reg3) {  // If reg2 is NOT same as reg3, store lower 32
                               // bits in reg2
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2,
                il.And(Sizes::LEN32BIT, result,
                       il.Const(Sizes::LEN32BIT,
                                0xFFFFFFFF))));  // Mask lower 32 bits
          }
          len = Sizes::LEN32BIT;
          return true;

        } else {  // 01000
          if (opcode >> 16 &
              OpcodeFields::OPCODE_BIT_6) {  // 010001; format XI mul, mulu
            if (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) {  // mulu
              // Multiply word unsigned by register; stores higher 32bits in
              // reg3 and lower 32 bits in reg2 Text format: mulu reg1, reg2,
              // reg3
              result = il.MultDoublePrecUnsigned(
                  Sizes::LEN64BIT, il.Register(Sizes::LEN32BIT, reg2),
                  il.Register(Sizes::LEN32BIT, reg1));
              if (reg3 != Registers::R0) {  // If reg3 is r0, upper half of
                                            // result is discarded
                il.AddInstruction(il.SetRegister(
                    Sizes::LEN32BIT, reg3,
                    il.LogicalShiftRight(
                        Sizes::LEN32BIT,  // Upper 32 bits of 64 bit result
                        result, il.Const(Sizes::LEN32BIT, 32))));
              }
              // Special case: if reg2 is same as reg3, higher 32 bits are
              // stored in reg2/reg3 (already done)
              if (reg2 != reg3) {  // If reg2 is NOT same as reg3, store lower
                                   // 32 bits in reg2
                il.AddInstruction(il.SetRegister(
                    Sizes::LEN32BIT, reg2,
                    il.And(Sizes::LEN32BIT, result,
                           il.Const(Sizes::LEN32BIT,
                                    0xFFFFFFFF))));  // Mask lower 32 bits
              }
              len = Sizes::LEN32BIT;
              return true;

            } else {  // mul
              // Multiply word by register;  stores higher 32bits in reg3 and
              // lower 32 bits in reg2 Text format: mul reg1, reg2, reg3
              // Multiply word by register;  stores higher 32bits in reg3 and
              // lower 32 bits in reg2 Format: mul reg1, reg2, reg3
              result = il.MultDoublePrecSigned(
                  Sizes::LEN64BIT, il.Register(Sizes::LEN32BIT, reg2),
                  il.Register(Sizes::LEN32BIT, reg1));
              if (reg3 != Registers::R0) {  // If reg3 is r0, upper half of
                                            // result is discarded
                il.AddInstruction(il.SetRegister(
                    Sizes::LEN32BIT, reg3,
                    il.LogicalShiftRight(
                        Sizes::LEN32BIT,  // Upper 32 bits of 64 bit result
                        result, il.Const(Sizes::LEN32BIT, 32))));
              }
              // Special case: if reg2 is same as reg3, higher 32 bits are
              // stored in reg2/reg3 (already done)
              if (reg2 != reg3) {  // If reg2 is NOT same as reg3, store lower
                                   // 32 bits in reg2
                il.AddInstruction(il.SetRegister(
                    Sizes::LEN32BIT, reg2,
                    il.And(Sizes::LEN32BIT, result,
                           il.Const(Sizes::LEN32BIT,
                                    0xFFFFFFFF))));  // Mask lower 32 bits
              }
              len = Sizes::LEN32BIT;
              return true;
            }
          } else {  // 010000; sasf
            // Shift and set flag condition; reg2 shifted left by 1, and if
            // condition is satisfied, LSB set to 1, else if condition not
            // satisfied, LSB set to 0 Text format: sasf cond, reg2
            auto condition_sasf =
                static_cast<uint8_t>(opcode & OpcodeFields::MASK_IX_COND);
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2,
                il.Or(Sizes::LEN32BIT,
                      il.ShiftLeft(Sizes::LEN32BIT,
                                   il.Register(Sizes::LEN32BIT, reg2),
                                   il.Const(Sizes::LEN32BIT, 1)),
                      ConditionToIL(condition_sasf,
                                    il))));  // condition satisfied ? 1 : 0
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
            // Enable interrupt; clear ID (interrupt disable) flag
            il.AddInstruction(il.Store(
                Sizes::LEN32BIT,
                il.ConstPointer(
                    Sizes::LEN32BIT,
                    Registers::SYSTEM_REG_BASE +
                        Registers::V850_REG_PSW * Registers::REGISTER_SIZE),
                il.And(
                    Sizes::LEN32BIT,
                    il.Load(Sizes::LEN32BIT,
                            il.ConstPointer(Sizes::LEN32BIT,
                                            Registers::SYSTEM_REG_BASE +
                                                Registers::V850_REG_PSW *
                                                    Registers::REGISTER_SIZE)),
                    il.Const(Sizes::LEN32BIT, Flags::MASK_CLEAR_ID_FLAG))));
            len = Sizes::LEN32BIT;
            return true;

          } else {  // di
            // Disable interrupt; set ID (interrupt disable) flag
            il.AddInstruction(il.Store(
                Sizes::LEN32BIT,
                il.ConstPointer(
                    Sizes::LEN32BIT,
                    Registers::SYSTEM_REG_BASE +
                        Registers::V850_REG_PSW * Registers::REGISTER_SIZE),
                il.Or(
                    Sizes::LEN32BIT,
                    il.Load(Sizes::LEN32BIT,
                            il.ConstPointer(Sizes::LEN32BIT,
                                            Registers::SYSTEM_REG_BASE +
                                                Registers::V850_REG_PSW *
                                                    Registers::REGISTER_SIZE)),
                    il.Const(Sizes::LEN32BIT, Flags::MASK_SET_ID_FLAG))));
            len = Sizes::LEN32BIT;
            return true;
          }
        } else {
          BN::ExprId psw_np_set;
          BN::ExprId psw_ep_set;
          // 001010; format X reti, ctret, dbret
          switch (opcode >> 16 & OpcodeFields::MASK_SUBOP_BITS_17_18) {
            case Opcodes::SUBOP_X_RETI:
              // Return from trap or interrupt
              psw_ep_set = il.And(
                  Sizes::LEN32BIT,  // condition to check whether ep flag is set
                  il.LogicalShiftRight(
                      Sizes::LEN32BIT,
                      il.Load(Sizes::LEN32BIT,
                              Registers::SYSTEM_REG_BASE +
                                  Registers::V850_REG_PSW *
                                      Registers::REGISTER_SIZE),
                      Flags::FLAG_EP_EXCEPTION_PENDING),
                  il.Const(Sizes::LEN32BIT, 1));
              psw_np_set = il.And(
                  Sizes::LEN32BIT,  // condition to check whether np flag is set
                  il.LogicalShiftRight(
                      Sizes::LEN32BIT,
                      il.Load(Sizes::LEN32BIT,
                              Registers::SYSTEM_REG_BASE +
                                  Registers::V850_REG_PSW *
                                      Registers::REGISTER_SIZE),
                      Flags::FLAG_NP_NMI_PENDING),
                  il.Const(Sizes::LEN32BIT, 1));
              if (il.GetExprValue(psw_np_set).value == 1 &&
                  il.GetExprValue(psw_ep_set).value ==
                      0) {  // NP flag && !EP flag
                il.AddInstruction(il.Return(il.Load(
                    Sizes::LEN32BIT,  // pc <- fepc
                    il.ConstPointer(Sizes::LEN32BIT,
                                    Registers::SYSTEM_REG_BASE +
                                        Registers::V850_REG_FEPC *
                                            Registers::REGISTER_SIZE))));
                il.AddInstruction(il.Store(
                    Sizes::LEN32BIT,  // psw <- fepsw
                    il.ConstPointer(
                        Sizes::LEN32BIT,
                        Registers::SYSTEM_REG_BASE +
                            Registers::V850_REG_PSW * Registers::REGISTER_SIZE),
                    il.Load(
                        Sizes::LEN32BIT,
                        il.ConstPointer(Sizes::LEN32BIT,
                                        Registers::SYSTEM_REG_BASE +
                                            Registers::V850_REG_FEPSW *
                                                Registers::REGISTER_SIZE))));
              } else {
                il.AddInstruction(il.Return(il.Load(
                    Sizes::LEN32BIT,  // pc <- eipc
                    il.ConstPointer(Sizes::LEN32BIT,
                                    Registers::SYSTEM_REG_BASE +
                                        Registers::V850_REG_EIPC *
                                            Registers::REGISTER_SIZE))));
                il.AddInstruction(il.Store(
                    Sizes::LEN32BIT,  // psw <- eipsw
                    il.ConstPointer(
                        Sizes::LEN32BIT,
                        Registers::SYSTEM_REG_BASE +
                            Registers::V850_REG_PSW * Registers::REGISTER_SIZE),
                    il.Load(
                        Sizes::LEN32BIT,
                        il.ConstPointer(Sizes::LEN32BIT,
                                        Registers::SYSTEM_REG_BASE +
                                            Registers::V850_REG_FEPSW *
                                                Registers::REGISTER_SIZE))));
              }
              len = Sizes::LEN32BIT;
              return true;

            case Opcodes::SUBOP_X_CTRET:
              // Return from callt
              il.AddInstruction(  // pc <- ctpc
                  il.Return(il.Load(
                      Sizes::LEN32BIT,
                      il.ConstPointer(Sizes::LEN32BIT,
                                      Registers::SYSTEM_REG_BASE +
                                          Registers::V850_REG_CTPC *
                                              Registers::REGISTER_SIZE))));
              il.AddInstruction(  // psw <- ctpsw
                  il.Store(Sizes::LEN32BIT,
                           il.ConstPointer(Sizes::LEN32BIT,
                                           Registers::SYSTEM_REG_BASE +
                                               Registers::V850_REG_PSW *
                                                   Registers::REGISTER_SIZE),
                           il.Load(Sizes::LEN32BIT,
                                   il.ConstPointer(
                                       Sizes::LEN32BIT,
                                       Registers::SYSTEM_REG_BASE +
                                           Registers::V850_REG_CTPSW *
                                               Registers::REGISTER_SIZE))));
              // TODO if using flags the Binja way I probably need to set them
              // here but
              //  idk if it's just easier to manually set/read them from the psw
              len = Sizes::LEN32BIT;
              return true;

            case Opcodes::SUBOP_X_DBRET:
              // Return from debug trap
              il.AddInstruction(  // pc <- dbpc
                  il.Return(il.Load(
                      Sizes::LEN32BIT,
                      il.ConstPointer(Sizes::LEN32BIT,
                                      Registers::SYSTEM_REG_BASE +
                                          Registers::V850_REG_DBPC *
                                              Registers::REGISTER_SIZE))));
              il.AddInstruction(  // psw <- dbpsw
                  il.Store(Sizes::LEN32BIT,
                           il.ConstPointer(Sizes::LEN32BIT,
                                           Registers::SYSTEM_REG_BASE +
                                               Registers::V850_REG_PSW *
                                                   Registers::REGISTER_SIZE),
                           il.Load(Sizes::LEN32BIT,
                                   il.ConstPointer(
                                       Sizes::LEN32BIT,
                                       Registers::SYSTEM_REG_BASE +
                                           Registers::V850_REG_DBPSW *
                                               Registers::REGISTER_SIZE))));
              // TODO if using flags the Binja way I probably need to set them
              // here but
              //  idk if it's just easier to manually set/read them from the psw
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
          il.AddInstruction(il.NoReturn());

          len = Sizes::LEN32BIT;
          return true;

        } else {  // 001000; trap
          // Trap
          // Text format: trap vector
          auto vector =
              static_cast<uint8_t>(opcode & OpcodeFields::MASK_X_IMM_VECTOR);

          il.AddInstruction(il.Store(
              Sizes::LEN32BIT,  // eipc <- pc + 4
              il.ConstPointer(
                  Sizes::LEN32BIT,
                  Registers::SYSTEM_REG_BASE +
                      Registers::V850_REG_EIPC * Registers::REGISTER_SIZE),
              il.Const(Sizes::LEN32BIT, addr + Sizes::LEN32BIT)));
          il.AddInstruction(il.Store(
              Sizes::LEN32BIT,  // eipsw <- psw
              il.ConstPointer(
                  Sizes::LEN32BIT,
                  Registers::SYSTEM_REG_BASE +
                      Registers::V850_REG_EIPSW * Registers::REGISTER_SIZE),
              il.Load(Sizes::LEN32BIT,
                      il.ConstPointer(Sizes::LEN32BIT,
                                      Registers::SYSTEM_REG_BASE +
                                          Registers::V850_REG_PSW *
                                              Registers::REGISTER_SIZE))));
          il.AddInstruction(il.Store(
              Sizes::LEN16BIT,  // ecr.eicc <- exception code
              il.ConstPointer(
                  Sizes::LEN16BIT,
                  Registers::SYSTEM_REG_BASE +
                      Registers::V850_REG_ECR * Registers::REGISTER_SIZE),
              il.Const(Sizes::LEN16BIT, 0x40 + vector)));
          il.AddInstruction(il.Store(
              Sizes::LEN32BIT,
              il.ConstPointer(
                  Sizes::LEN32BIT,
                  Registers::SYSTEM_REG_BASE +
                      Registers::V850_REG_PSW * Registers::REGISTER_SIZE),
              il.Or(Sizes::LEN32BIT,  // set EP (exception pending) flag in PSW
                    il.Or(Sizes::LEN32BIT,  // set ID (interrupt disable) flagt
                                            // in PSW
                          il.Load(Sizes::LEN32BIT,
                                  il.ConstPointer(
                                      Sizes::LEN32BIT,
                                      Registers::SYSTEM_REG_BASE +
                                          Registers::V850_REG_PSW *
                                              Registers::REGISTER_SIZE)),
                          il.Const(Sizes::LEN32BIT, Flags::MASK_SET_ID_FLAG)),
                    il.Const(Sizes::LEN32BIT, Flags::MASK_SET_EP_FLAG))));
          // pc <- 0x40 (if vector is 0x00 to 0x0F) or 0x50 (if vector is 0x10
          // to 0x1F)
          if (vector < 0x10) {
            il.AddInstruction(il.Trap(0x40));
          } else {
            il.AddInstruction(il.Trap(0x50));
          }

          len = Sizes::LEN32BIT;
          return true;
        }
      }
    } else {                                              // 000
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_4) {    // 0001
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_5) {  // 00011
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {
            BN::ExprId bitmask;
            // 000111
            // Determine opcode by last 3 bits of halfword (bits 16-18 of
            // instruction)
            switch ((opcode >> 16 & OpcodeFields::MASK_SUBOP_BITS_17_18) >>
                    OpcodeFields::SHIFT_SUBOP_BITS_17_18) {
              case Opcodes::SUBOP_SET1:
                // Set single bit; reg2 holds bit num and reg1 is addr
                // Text format: set1 reg2, [reg1]
                il.AddInstruction(il.Store(
                    Sizes::LEN8BIT, il.Register(Sizes::LEN32BIT, reg1),
                    il.Or(
                        Sizes::LEN32BIT,
                        il.Load(Sizes::LEN8BIT,
                                il.Register(Sizes::LEN32BIT, reg1)),
                        il.ShiftLeft(Sizes::LEN32BIT,  // Lowest 3 bits of reg2
                                                       // used to select bit
                                     il.Const(Sizes::LEN32BIT, 1),
                                     il.And(Sizes::LEN8BIT,
                                            il.Register(Sizes::LEN8BIT, reg2),
                                            il.Const(Sizes::LEN8BIT, 0b111))),
                        Flags::FLAGS_WRITE_Z)));
                // TODO need to implement special behavior for Z flag here
                len = Sizes::LEN32BIT;
                return true;

              case Opcodes::SUBOP_NOT1:
                // Not operation on single bit
                // Text format: not1 reg2, [reg1]
                bitmask = il.ShiftLeft(
                    Sizes::LEN32BIT,  // Mask to select bit
                    il.Const(Sizes::LEN32BIT, 1),
                    il.And(Sizes::LEN32BIT,  // Bitnum is lower 3 bits of reg2
                           reg2, il.Const(Sizes::LEN8BIT, 0b111)));
                il.AddInstruction(il.Store(
                    Sizes::LEN8BIT, il.Register(Sizes::LEN32BIT, reg1),
                    il.Or(
                        Sizes::LEN32BIT,  // Combine rest of byte with the
                                          // modified bit
                        il.And(Sizes::LEN32BIT,  // Get all the bits that are
                                                 // NOT selected
                               il.Load(Sizes::LEN8BIT,
                                       il.Register(Sizes::LEN32BIT, reg1)),
                               il.Not(Sizes::LEN32BIT, bitmask)),
                        il.And(
                            Sizes::LEN32BIT,  // Get the modified bit that IS
                                              // selected
                            il.Not(Sizes::LEN32BIT,  // Not operation on that
                                                     // single bit
                                   il.And(Sizes::LEN32BIT,  // Mask to get
                                                            // selected bit only
                                          il.Load(Sizes::LEN8BIT,
                                                  il.Register(Sizes::LEN32BIT,
                                                              reg1)),
                                          bitmask),
                                   Flags::FLAGS_WRITE_Z),  // TODO sanity check,
                                                           // is this the right
                                                           // place for Z flag?
                            bitmask))));
                len = Sizes::LEN32BIT;
                return true;

              case Opcodes::SUBOP_CLR1:
                // Clear single bit
                // Text format: clr1 reg2, [reg1]
                il.AddInstruction(il.Store(
                    Sizes::LEN8BIT, il.Register(Sizes::LEN32BIT, reg1),
                    il.And(
                        Sizes::LEN32BIT,
                        il.Load(Sizes::LEN8BIT,
                                il.Register(Sizes::LEN32BIT, reg1)),
                        il.Not(
                            Sizes::LEN32BIT,
                            il.ShiftLeft(
                                Sizes::LEN32BIT, il.Const(Sizes::LEN32BIT, 1),
                                il.And(Sizes::LEN8BIT,
                                       il.Register(Sizes::LEN8BIT, reg2),
                                       il.Const(
                                           Sizes::LEN8BIT,
                                           0b111)))),  // Lowest 3 bits of reg2
                        Flags::
                            // used to select bit
                        FLAGS_WRITE_Z)));  // TODO need to implement special
                                           // behavior for Z flag here
                len = Sizes::LEN32BIT;
                return true;

              case Opcodes::SUBOP_TST1:
                // Test single bit
                // Text format: tst1 reg2, [reg1]
                bitmask = il.ShiftLeft(
                    Sizes::LEN32BIT,  // Mask to select bit
                    il.Const(Sizes::LEN32BIT, 1),
                    il.And(Sizes::LEN32BIT,  // Bitnum is lower 3 bits of reg2
                           reg2, il.Const(Sizes::LEN8BIT, 0b111)));
                il.AddInstruction(  // tst1 is basically not1 except without
                                    // storing the result
                    il.Not(
                        Sizes::LEN32BIT,  // Not operation on that single bit
                        il.And(
                            Sizes::LEN32BIT,  // Mask to get selected bit only
                            il.Load(Sizes::LEN8BIT,
                                    il.Register(Sizes::LEN32BIT, reg1)),
                            bitmask),
                        Flags::FLAGS_WRITE_Z));  // TODO sanity check, is this
                                                 // the right place for Z flag?
                len = Sizes::LEN32BIT;
                return true;
              default:
                return false;
            }
          } else {  // 000110; shl
            // Logical shift left
            // Text format: shl reg1, reg2
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2,
                il.ShiftLeft(
                    Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                    il.And(Sizes::LEN8BIT, il.Register(Sizes::LEN8BIT, reg1),
                           il.Const(Sizes::LEN8BIT, 0b11111)),
                    Flags::FLAGS_WRITE_CY_OV_S_Z)));
            len = Sizes::LEN32BIT;
            return true;
          }
        } else {                                            // 00010
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {  // 000101; sar
            // Arithmetic shift right
            // Text format: sar reg1, reg2
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2,
                il.ArithShiftRight(
                    Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                    il.And(Sizes::LEN8BIT, il.Register(Sizes::LEN8BIT, reg1),
                           il.Const(Sizes::LEN8BIT, 0b11111)),
                    Flags::FLAGS_WRITE_CY_OV_S_Z)));
            len = Sizes::LEN32BIT;
            return true;

          } else {  // 000100; shr
            // Logical shift right
            // Text format: shr reg1, reg2
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2,
                il.LogicalShiftRight(
                    Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                    il.And(Sizes::LEN8BIT, il.Register(Sizes::LEN8BIT, reg1),
                           il.Const(Sizes::LEN8BIT, 0b11111)),
                    Flags::FLAGS_WRITE_CY_OV_S_Z)));
            len = Sizes::LEN32BIT;
            return true;
          }
        }
      } else {  // 0000
        if (opcode >> 16 &
            OpcodeFields::OPCODE_BIT_5) {  // 00001; Only option is 000010; stsr
          // Store contents of system register
          // Text format: stsr regID, reg2
          // Note: regID is same field as reg1
          il.AddInstruction(il.SetRegister(
              Sizes::LEN32BIT, reg2,
              il.Load(Sizes::LEN32BIT,
                      il.ConstPointer(Sizes::LEN32BIT,
                                      Registers::SYSTEM_REG_BASE +
                                          reg1 * Registers::REGISTER_SIZE))));
          len = Sizes::LEN32BIT;
          return true;

        } else {                                            // 00000
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {  // 000001; ldsr
            // Load to system register
            // Text format: ldsr reg2, regID
            // Note: regID is same field as reg1
            // TODO special handling of flags! if the system register is the
            // PSW, the values of the
            //  corresponding bits of PSW (aka the flags) are set according to
            //  contents of reg2
            il.AddInstruction(
                il.Store(Sizes::LEN32BIT,
                         il.ConstPointer(Sizes::LEN32BIT,
                                         Registers::SYSTEM_REG_BASE +
                                             reg1 * Registers::REGISTER_SIZE),
                         il.Register(Sizes::LEN32BIT, reg2)));

            len = Sizes::LEN32BIT;
            return true;

          } else {  // 000000; setf
            // Set flag condition; if condition is met, sets reg2 to 1; if not,
            // sets to 0 Text format: setf cond, reg2
            auto condition_setf =
                static_cast<uint8_t>(opcode & OpcodeFields::MASK_IX_COND);

            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2, ConditionToIL(condition_setf, il)));

            len = Sizes::LEN32BIT;
            return true;
          }
        }
      }
    }
  }
  return false;
}

/*
 * Instruction lift methods
 */
bool AddImm5R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Lift_II(opcode, addr, len, il);
}

bool AddR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Lift_I_ADD(opcode, addr, len, il);
}

bool AndiImm16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_VI(opcode, addr, len, il);
}

bool AddiImm32R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_VI(opcode, addr, len, il);
}

bool AndR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Lift_I_AND(opcode, addr, len, il);
}

bool Bc::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bge::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bgt::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bh::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Ble::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Blt::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bn::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bnc::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bnh::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bnv::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bnz::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bp::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Br::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bsa::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool BshR2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool BswR2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool Bv::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool Bz::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_III(opcode, addr, len, il, arch);
}

bool CalltImm6::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Lift_II(opcode, addr, len, il);
}

bool Clr1Bit3Disp16R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                            BN::LowLevelILFunction &il,
                            BinaryNinja::Architecture *arch) {
  return Lift_VIII(opcode, addr, len, il);
}

bool Clr1R2R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool CmovCcccR1R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                          BN::LowLevelILFunction &il,
                          BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool CmovCccImm5R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                           BN::LowLevelILFunction &il,
                           BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool CmpImm5R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Lift_II(opcode, addr, len, il);
}

bool CmpR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Lift_I_CMP(opcode, addr, len, il);
}

bool Ctret::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool Dbret::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool Dbtrap::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                  BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_I_SWITCH_DBTRAP_DIVH(opcode, addr, len, il, arch);
}

bool Di::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool DisposeImm5List12::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                             BN::LowLevelILFunction &il,
                             BinaryNinja::Architecture *arch) {
  return Lift_VI_MOVHI_SATSUBI_XIII_DISPOSE(opcode, addr, len, il);
}

bool DisposeImm5List12R1::Lift(const uint64_t opcode, uint64_t addr,
                               size_t &len, BN::LowLevelILFunction &il,
                               BinaryNinja::Architecture *arch) {
  return Lift_VI_MOVHI_SATSUBI_XIII_DISPOSE(opcode, addr, len, il);
}

bool DivR1R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool DivhR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) {
  return Lift_I_SWITCH_DBTRAP_DIVH(opcode, addr, len, il, arch);
}

bool DivhR1R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool DivhuR1R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool DivuR1R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool Ei::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool Halt::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool HswR2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool JarlDisp22R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture *arch) {
  return Lift_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, addr, len, il, arch);
}

bool JmpR1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_I_JMP_IV_SLDHU_SLDBU(opcode, len, il);
}

bool JrDisp22::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) {
  return Lift_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, addr, len, il, arch);
}

bool LdbDisp16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_VII(opcode, addr, len, il);
}

bool LdbuDisp16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                          BN::LowLevelILFunction &il,
                          BinaryNinja::Architecture *arch) {
  return Lift_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, addr, len, il, arch);
}

bool LdhDisp16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_VII(opcode, addr, len, il);
}

bool LdhuDisp16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                          BN::LowLevelILFunction &il,
                          BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool LdsrR1Rid::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool LdwDisp16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_VII(opcode, addr, len, il);
}

bool MovhiImm16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                          BN::LowLevelILFunction &il,
                          BinaryNinja::Architecture *arch) {
  return Lift_VI_MOVHI_SATSUBI_XIII_DISPOSE(opcode, addr, len, il);
}

bool MoveaImm16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                          BN::LowLevelILFunction &il,
                          BinaryNinja::Architecture *arch) {
  return Lift_VI(opcode, addr, len, il);
}

bool MovImm32R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  return Lift_VI(opcode, addr, len, il);
}

bool MovImm5R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Lift_II(opcode, addr, len, il);
}

bool MovR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Lift_I_MOV_NOP(opcode, addr, len, il);
}

bool MulhiImm16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                          BN::LowLevelILFunction &il,
                          BinaryNinja::Architecture *arch) {
  return Lift_VI(opcode, addr, len, il);
}

bool MulhImm5R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  return Lift_II(opcode, addr, len, il);
}

bool MulImm9R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool MulhR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) {
  return Lift_I_MULH_SXH(opcode, addr, len, il);
}

bool MulR1R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool MuluImm9R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool MuluR1R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool Nop::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_I_MOV_NOP(opcode, addr, len, il);
}

bool Not1Bit3Disp16R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                            BN::LowLevelILFunction &il,
                            BinaryNinja::Architecture *arch) {
  return Lift_VIII(opcode, addr, len, il);
}

bool Not1R2R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool NotR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Lift_I_NOT(opcode, addr, len, il);
}

bool OriImm16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture *arch) {
  return Lift_VI(opcode, addr, len, il);
}

bool OrR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                  BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_I_OR(opcode, addr, len, il);
}

bool PrepareList12Imm5::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                             BN::LowLevelILFunction &il,
                             BinaryNinja::Architecture *arch) {
  return Lift_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, addr, len, il, arch);
}

bool PrepareList12Imm5Sp::Lift(const uint64_t opcode, uint64_t addr,
                               size_t &len, BN::LowLevelILFunction &il,
                               BinaryNinja::Architecture *arch) {
  return Lift_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, addr, len, il, arch);
}

bool PrepareList12Imm5SpImm16SignExt::Lift(const uint64_t opcode, uint64_t addr,
                                           size_t &len,
                                           BN::LowLevelILFunction &il,
                                           BinaryNinja::Architecture *arch) {
  return Lift_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, addr, len, il, arch);
}

bool PrepareList12Imm5SpImm16LogicShift::Lift(const uint64_t opcode,
                                              uint64_t addr, size_t &len,
                                              BN::LowLevelILFunction &il,
                                              BinaryNinja::Architecture *arch) {
  return Lift_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, addr, len, il, arch);
}

bool PrepareList12Imm5SpImm32::Lift(const uint64_t opcode, uint64_t addr,
                                    size_t &len, BN::LowLevelILFunction &il,
                                    BinaryNinja::Architecture *arch) {
  return Lift_V_JARL_JR_VII_LDBU_XIII_PREPARE(opcode, addr, len, il, arch);
}

bool Reti::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool SarImm5R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Lift_II(opcode, addr, len, il);
}

bool SarR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool SasfCondR2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool SataddImm5::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  return Lift_II(opcode, addr, len, il);
}

bool SataddR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  return Lift_I_SATADD_ZXH(opcode, addr, len, il);
}

bool SatsubiImm16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                            BN::LowLevelILFunction &il,
                            BinaryNinja::Architecture *arch) {
  return Lift_VI_MOVHI_SATSUBI_XIII_DISPOSE(opcode, addr, len, il);
}

bool SatsubR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  return Lift_I_SATSUB_SXB(opcode, addr, len, il);
}

bool SatsubrR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  return Lift_I_SATSUBR_ZXB(opcode, addr, len, il);
}

bool Set1Bit3Disp16R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                            BN::LowLevelILFunction &il,
                            BinaryNinja::Architecture *arch) {
  return Lift_VIII(opcode, addr, len, il);
}

bool Set1R2R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool SetfCondR2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool ShlImm5R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Lift_II(opcode, addr, len, il);
}

bool ShlR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool ShrImm5R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Lift_II(opcode, addr, len, il);
}

bool ShrR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool SldbuDisp4R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture *arch) {
  return Lift_I_JMP_IV_SLDHU_SLDBU(opcode, len, il);
}

bool SldbDisp7R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  return Lift_IV_SLDB(opcode, addr, len, il);
}

bool SldhuDisp5R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture *arch) {
  return Lift_I_JMP_IV_SLDHU_SLDBU(opcode, len, il);
}

bool SldhDisp8R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  return Lift_IV(opcode, addr, len, il);
}

bool SldwDisp8R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  return Lift_IV(opcode, addr, len, il);
}

bool SstbR2Disp7::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  return Lift_IV_SSTB(opcode, addr, len, il);
}

bool SsthR2Disp8::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  return Lift_IV(opcode, addr, len, il);
}

bool SstwR2Disp8::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  return Lift_IV(opcode, addr, len, il);
}

bool StbR2Disp16R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_VII(opcode, addr, len, il);
}

bool SthR2Disp26R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_VII(opcode, addr, len, il);
}

bool StsrRidR2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool StwR2Disp16R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_VII(opcode, addr, len, il);
}

bool SubR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Lift_I_SUB(opcode, addr, len, il);
}

bool SubrR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) {
  return Lift_I_SUBR(opcode, addr, len, il);
}

bool SwitchR1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) {
  return Lift_I_SWITCH_DBTRAP_DIVH(opcode, addr, len, il, arch);
}

bool SxbR1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_I_SATSUB_SXB(opcode, addr, len, il);
}

bool SxhR1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_I_MULH_SXH(opcode, addr, len, il);
}

bool Trap::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool Tst1Bit3Disp16R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                            BN::LowLevelILFunction &il,
                            BinaryNinja::Architecture *arch) {
  return Lift_VIII(opcode, addr, len, il);
}

bool Tst1R2R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool TstR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Lift_I_TST(opcode, addr, len, il);
}

bool XoriImm16R1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_VI(opcode, addr, len, il);
}

bool XorR1R2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture *arch) {
  return Lift_I_XOR(opcode, addr, len, il);
}

bool ZxbR1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_I_SATSUBR_ZXB(opcode, addr, len, il);
}

bool ZxhR1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  return Lift_I_SATADD_ZXH(opcode, addr, len, il);
}
}  // namespace V850
