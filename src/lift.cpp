// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#include <binaryninjaapi.h>
#include <lowlevelilinstruction.h>

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

/* Reconstruct the signed 23-bit displacement for Format XIV instructions.
 * Matches the Ghidra SLEIGH definition:
 *   byte-granular variants: disp23 = (s3247 << 7) | op2026
 *   aligned variants:       disp23 = (s3247 << 7) | (op2126 << 1)
 * Returned value is sign-extended to 32 bits. */
static int32_t LiftExtractXIVDisp23(const uint64_t opcode, bool aligned) {
  const auto hw2 = static_cast<uint16_t>(opcode >> 16);
  const auto hw3 = static_cast<uint16_t>(opcode >> 32);
  uint32_t low;
  if (aligned) {
    low = (static_cast<uint32_t>(hw2 & OpcodeFields::MASK_XIV_OP2126) >>
           OpcodeFields::SHIFT_XIV_OP2126)
          << 1;
  } else {
    low = static_cast<uint32_t>(hw2 & OpcodeFields::MASK_XIV_OP2026) >>
          OpcodeFields::SHIFT_XIV_OP2026;
  }
  const int32_t upper = static_cast<int32_t>(static_cast<int16_t>(hw3));
  return (upper << 7) | static_cast<int32_t>(low);
}

static uint8_t LiftExtractXIVReg3(const uint64_t opcode) {
  const auto hw2 = static_cast<uint16_t>(opcode >> 16);
  return static_cast<uint8_t>((hw2 & OpcodeFields::MASK_XIV_R2731) >>
                              OpcodeFields::SHIFT_XIV_R2731);
}

/* Shared load lift for Format XIV: reg3 = extend(M[reg1 + se(disp23)]).
 *   access_size : byte count of the memory load (1, 2, or 4)
 *   sign_extend : true -> SignExtend to 32 bits, false -> ZeroExtend
 *   aligned     : true -> use op2126<<1 disp, false -> use op2026 disp */
static bool Lift_XIV_Load(const uint64_t opcode, size_t &len,
                          BN::LowLevelILFunction &il, size_t access_size,
                          bool sign_extend, bool aligned) {
  const auto reg1 = ExtractReg1OpcodeField(static_cast<uint16_t>(opcode));
  const auto reg3 = LiftExtractXIVReg3(opcode);
  const int32_t disp = LiftExtractXIVDisp23(opcode, aligned);

  BN::ExprId addr_il =
      il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
             il.Const(Sizes::LEN32BIT, static_cast<uint32_t>(disp)));
  BN::ExprId load_il = il.Load(access_size, addr_il);
  BN::ExprId extended = sign_extend ? il.SignExtend(Sizes::LEN32BIT, load_il)
                                    : il.ZeroExtend(Sizes::LEN32BIT, load_il);
  il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3, extended));
  len = Sizes::LEN48BIT;
  return true;
}

static bool Lift_XIV_Store(const uint64_t opcode, size_t &len,
                           BN::LowLevelILFunction &il, size_t access_size,
                           bool aligned) {
  const auto reg1 = ExtractReg1OpcodeField(static_cast<uint16_t>(opcode));
  const auto reg3 = LiftExtractXIVReg3(opcode);
  const int32_t disp = LiftExtractXIVDisp23(opcode, aligned);

  BN::ExprId addr_il =
      il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
             il.Const(Sizes::LEN32BIT, static_cast<uint32_t>(disp)));
  il.AddInstruction(
      il.Store(access_size, addr_il, il.Register(access_size, reg3)));
  len = Sizes::LEN48BIT;
  return true;
}

bool Lift_I_JMP_IV_SLDHU_SLDBU(const uint64_t opcode, size_t &len,
                               BN::LowLevelILFunction &il) {
  if (const auto reg2 = ExtractReg2OpcodeField(opcode); reg2 == Registers::R0) {
    // Jmp; jump to address in register
    const auto reg1 = ExtractReg1OpcodeField(opcode);

    // V850 `jmp [reg1]` is an unconditional register-indirect jump, NOT a
    // call — no link register is saved. Using il.Call here makes every tail
    // call / jumptable dispatch look like a function call in Binja HLIL.
    il.AddInstruction(
        il.Jump(il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                       il.Const(Sizes::LEN32BIT, 0xFFFFFFFE))));

  } else {
    auto opcode_7 =
        static_cast<uint8_t>((opcode & OpcodeFields::OPCODE_7_BITS) >>
                             OpcodeFields::SHIFT_7BIT_OPCODE);
    uint8_t disp;

    if (opcode_7 == Opcodes::OP_IV_SLD_BU) {
      // Short format load byte unsigned
      // Text format: sld.bu disp4[ep], reg2
      // G3MH software manual p. 5-50 (sld.bu)
      disp = static_cast<uint8_t>(opcode & OpcodeFields::MASK_IV_4BIT_DISP);
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.ZeroExtend(
              Sizes::LEN32BIT,
              il.Load(Sizes::LEN8BIT,
                      il.Add(Sizes::LEN32BIT,
                             il.Register(Sizes::LEN32BIT, Registers::EP),
                             il.Const(Sizes::LEN32BIT, disp))))));

    } else if (opcode_7 == Opcodes::OP_IV_SLD_HU) {
      // Short format load halfword unsigned
      // Text format: sld.hu disp5[ep], reg2
      // G3MH Software Manual, "SLD.HU" (see extracted reference).
      // Decoder already scales the 4-bit field left by 1 to produce a
      // byte-granular disp5.
      disp = static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_4BIT_DISP)
                                  << OpcodeFields::SHIFT_IV_DISP);
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.ZeroExtend(
              Sizes::LEN32BIT,
              il.Load(Sizes::LEN16BIT,
                      il.Add(Sizes::LEN32BIT,
                             il.Register(Sizes::LEN32BIT, Registers::EP),
                             il.Const(Sizes::LEN32BIT, disp))))));
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
    // G3MH p. 180 (DIVH format 1 — 16-bit encoding):
    //   reg2 <- reg2 / sign_extend(reg1[15:0])
    //   remainder is discarded
    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, reg2,
        il.DivSigned(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                     il.SignExtend(Sizes::LEN32BIT,
                                   il.LowPart(Sizes::LEN16BIT,
                                              il.Register(Sizes::LEN32BIT,
                                                          reg1)))),
        Flags::FLAGS_WRITE_OV_S_Z));
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
        Flags::FLAGS_WRITE_CY_OV_S_Z);  // SAT is sticky; set explicitly on sat branches below
    il.AddInstruction(  // Check whether maximum negative value is exceeded
        il.If(il.CompareSignedLessThan(Sizes::LEN64BIT, result,
                                       il.Const(Sizes::LEN32BIT, 0x80000000)),
              sat_neg_true, sat_neg_false));

    il.MarkLabel(sat_neg_true);  // Saturated negative result
    il.AddInstruction(  // reg2 set to maximum negative word-sized value,
                        // 0x80000000
        il.SetRegister(Sizes::LEN32BIT, reg1,
                       il.Const(Sizes::LEN32BIT, 0x80000000)));
    // SAT flag is sticky per G3MH spec — set on saturation, cleared only
    // by LDSR of PSW. Enables correct BSa lowering via il.Flag("sat").
    il.AddInstruction(il.SetFlag(Flags::FLAG_SAT_SATURATED,
                                 il.Const(Sizes::LEN8BIT, 1)));
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
        il.SetRegister(Sizes::LEN32BIT, reg1,
                       il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)));
    il.AddInstruction(il.SetFlag(Flags::FLAG_SAT_SATURATED,
                                 il.Const(Sizes::LEN8BIT, 1)));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(sat_pos_false);  // Result NOT saturated
    il.AddInstruction(  // Store result just like normal subtract operation
        il.SetRegister(Sizes::LEN32BIT, reg1,
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
        Flags::FLAGS_WRITE_CY_OV_S_Z);  // SAT is sticky; set explicitly on sat branches below
    il.AddInstruction(  // Check whether maximum negative value is exceeded
        il.If(il.CompareSignedLessThan(Sizes::LEN64BIT, result,
                                       il.Const(Sizes::LEN32BIT, 0x80000000)),
              sat_neg_true, sat_neg_false));

    il.MarkLabel(sat_neg_true);  // Saturated negative result
    il.AddInstruction(  // reg2 set to maximum negative word-sized value,
                        // 0x80000000
        il.SetRegister(Sizes::LEN32BIT, reg2,
                       il.Const(Sizes::LEN32BIT, 0x80000000)));
    il.AddInstruction(il.SetFlag(Flags::FLAG_SAT_SATURATED,
                                 il.Const(Sizes::LEN8BIT, 1)));
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
        il.SetRegister(Sizes::LEN32BIT, reg2,
                       il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)));
    il.AddInstruction(il.SetFlag(Flags::FLAG_SAT_SATURATED,
                                 il.Const(Sizes::LEN8BIT, 1)));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(sat_pos_false);  // Result NOT saturated
    il.AddInstruction(  // Store result just like normal subtract operation
        il.SetRegister(Sizes::LEN32BIT, reg2,
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
    len = Sizes::LEN16BIT;
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
        Flags::FLAGS_WRITE_CY_OV_S_Z);  // SAT is sticky; set explicitly on sat branches below

    il.AddInstruction(  // Check whether maximum negative value is exceeded
        il.If(il.CompareSignedLessThan(Sizes::LEN64BIT, result,
                                       il.Const(Sizes::LEN32BIT, 0x80000000)),
              sat_neg_true, sat_neg_false));

    il.MarkLabel(sat_neg_true);  // Saturated negative result
    il.AddInstruction(
        il.SetRegister(Sizes::LEN32BIT, reg2,
                       il.Const(Sizes::LEN32BIT, 0x80000000)));
    il.AddInstruction(il.SetFlag(Flags::FLAG_SAT_SATURATED,
                                 il.Const(Sizes::LEN8BIT, 1)));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(sat_neg_false);
    il.AddInstruction(  // Check whether maximum positive value is exceeded
        il.If(
            il.CompareSignedGreaterThan(Sizes::LEN64BIT, result,
                                        il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)),
            sat_pos_true, sat_pos_false));

    il.MarkLabel(sat_pos_true);  // Saturated positive result
    il.AddInstruction(
        il.SetRegister(Sizes::LEN32BIT, reg2,
                       il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)));
    il.AddInstruction(il.SetFlag(Flags::FLAG_SAT_SATURATED,
                                 il.Const(Sizes::LEN8BIT, 1)));
    il.AddInstruction(il.Goto(done));

    il.MarkLabel(sat_pos_false);  // Result NOT saturated
    il.AddInstruction(  // Store result just like normal subtract operation
        il.SetRegister(Sizes::LEN32BIT, reg2,
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
    // Multiplies lower half-word of reg2 (signed) by lower half-word of reg1
    // (signed) and stores the 32-bit signed result in reg2.
    // Per G3MH p.223: GR[reg2] <- GR[reg2](15:0) * GR[reg1](15:0) (signed).
    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, reg2,
        il.Mult(
            Sizes::LEN32BIT,
            il.SignExtend(Sizes::LEN32BIT,
                          il.LowPart(Sizes::LEN16BIT,
                                     il.Register(Sizes::LEN32BIT, reg2))),
            il.SignExtend(Sizes::LEN32BIT,
                          il.LowPart(Sizes::LEN16BIT,
                                     il.Register(Sizes::LEN32BIT, reg1))))));
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
            il.Register(Sizes::LEN32BIT, reg2), Flags::FLAGS_WRITE_S_Z)));
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
             il.Register(Sizes::LEN32BIT, reg2), Flags::FLAGS_WRITE_S_Z)));
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
             il.Register(Sizes::LEN32BIT, reg2), Flags::FLAGS_WRITE_S_Z)));
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
                           Flags::FLAGS_WRITE_S_Z));
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
        // Logical shift reg2 left by imm5 (G3MH manual: shl imm5, reg2)
        // Flags: CY = last bit shifted out, S = sign of result, Z = zero, OV = 0
        len = Sizes::LEN16BIT;
        if (reg2 == Registers::R0) {
          // r0 is hardwired zero; writes discarded. Skip to avoid malformed LLIL.
          return true;
        }
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.ShiftLeft(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                         il.Const(Sizes::LEN32BIT, imm5 & 0x1F),
                         Flags::FLAGS_WRITE_CY_OV_S_Z)));
        return true;
      }
    } else {  // Opcodes starting with 0b01010
      if (opcode &
          OpcodeFields::OPCODE_BIT_6) {  // Opcode 0b010101; format II sar
        // Arithmetic shift reg2 right by imm5 (G3MH manual: sar imm5, reg2)
        // Flags: CY = last bit shifted out, S = sign of result, Z = zero, OV = 0
        len = Sizes::LEN16BIT;
        if (reg2 == Registers::R0) {
          return true;
        }
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.ArithShiftRight(Sizes::LEN32BIT,
                               il.Register(Sizes::LEN32BIT, reg2),
                               il.Const(Sizes::LEN32BIT, imm5 & 0x1F),
                               Flags::FLAGS_WRITE_CY_OV_S_Z)));
        return true;

      } else {  // Opcode 0b010100; format II shr
        // Logical shift reg2 right by imm5 (G3MH manual: shr imm5, reg2)
        // Flags: CY = last bit shifted out, S = sign of result, Z = zero, OV = 0
        len = Sizes::LEN16BIT;
        if (reg2 == Registers::R0) {
          return true;
        }
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.LogicalShiftRight(Sizes::LEN32BIT,
                                 il.Register(Sizes::LEN32BIT, reg2),
                                 il.Const(Sizes::LEN32BIT, imm5 & 0x1F),
                                 Flags::FLAGS_WRITE_CY_OV_S_Z)));
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
        // pc  <- ctbp + ZeroExtend(LoadMemory(adr, Halfword))
        // CTBP is modeled as a system register living at ConstPointer(ctbp).
        BN::ExprId ctbp_val =
            il.Load(Sizes::LEN32BIT, il.ConstPointer(Sizes::LEN32BIT, ctbp));
        BN::ExprId entry_addr =
            il.Add(Sizes::LEN32BIT, ctbp_val,
                   il.Const(Sizes::LEN32BIT, imm6 << 1));
        BN::ExprId target_pc = il.Add(
            Sizes::LEN32BIT, ctbp_val,
            il.ZeroExtend(Sizes::LEN32BIT,
                          il.Load(Sizes::LEN16BIT, entry_addr)));
        il.AddInstruction(il.Call(target_pc));

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
              Flags::FLAGS_WRITE_CY_OV_S_Z);  // SAT is sticky; set explicitly on sat branches
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
      // SAT == 1; Saturated. Now that satadd/satsub/satsubi/satsubr emit
      // an explicit SetFlag(FLAG_SAT_SATURATED) on saturation, the
      // condition is a simple flag test.
      conditionIL = il.Flag(Flags::FLAG_SAT_SATURATED);
      break;

    default:
      return false;
  }

  // For each branch direction: if the target address already has a known
  // BN label, use that label by reference (it's marked at the target).
  // Otherwise use a local label that we MarkLabel + Jump from below.
  BN::ExprId dest_if_false = il.Const(Sizes::LEN32BIT, addr + Sizes::LEN16BIT);
  BNLowLevelILLabel *f = il.GetLabelForAddress(arch, dest_if_false);
  BN::LowLevelILLabel local_true_label, local_false_label;
  const bool indirect_true = (t == nullptr);
  const bool indirect_false = (f == nullptr);

  // il.If() takes references; pass the pointed-to label or the local one.
  il.AddInstruction(il.If(conditionIL,
                          t ? *t : local_true_label,
                          f ? *f : local_false_label));

  if (indirect_true) {
    il.MarkLabel(local_true_label);
    il.AddInstruction(il.Jump(dest_if_true));
  }

  if (indirect_false) {
    il.MarkLabel(local_false_label);
  }

  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_IV_SLDB(const uint64_t opcode, uint64_t addr, size_t &len,
                  BN::LowLevelILFunction &il) {
  // Short format load byte; format IV
  // Text format: sld.b disp7[ep], reg2
  // G3MH software manual p. 5-50 (sld.b)
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  auto disp7 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_IV_7BIT_DISP);

  il.AddInstruction(il.SetRegister(
      Sizes::LEN32BIT, reg2,
      il.SignExtend(
          Sizes::LEN32BIT,
          il.Load(Sizes::LEN8BIT,
                  il.Add(Sizes::LEN32BIT,
                         il.Register(Sizes::LEN32BIT, Registers::EP),
                         il.Const(Sizes::LEN32BIT, disp7))))));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_IV_SSTB(const uint64_t opcode, uint64_t addr, size_t &len,
                  BN::LowLevelILFunction &il) {
  // Short format store byte; format IV
  // Text format: sst.b reg2, disp7[ep]
  // G3MH Software Manual p.270: Store byte, address = EP + zero_extend(disp7),
  // no displacement scaling.  il.Store truncates the source to 1 byte.
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  BN::ExprId reg2_il =
      (reg2 == Registers::R0)
          ? il.Const(Sizes::LEN32BIT, 0)
          : il.Register(Sizes::LEN32BIT, reg2);  // r0 is always 0
  const auto disp7 = ExtractDisp7OpcodeField(opcode);

  BN::ExprId addr_expr =
      il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::EP),
             il.Const(Sizes::LEN32BIT, disp7));
  il.AddInstruction(il.Store(Sizes::LEN8BIT, addr_expr, reg2_il));
  len = Sizes::LEN16BIT;
  return true;
}

bool Lift_IV(const uint64_t opcode, uint64_t addr, size_t &len,
             BN::LowLevelILFunction &il) {
  auto op_iv = static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_OPCODE) >>
                                    OpcodeFields::SHIFT_IV_OPCODE);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  auto subop = static_cast<uint8_t>(opcode & OpcodeFields::MASK_IV_SUBOP);
  uint8_t disp;  // Displacement field varies depending on opcode

  // Value of r0 is always 0
  BN::ExprId reg2_il;

  switch (op_iv) {
    case Opcodes::OP_IV_4BIT_SLD_H:
      // Short format load halfword (sign-extending); format IV
      // Text format: sld.h disp8[ep], reg2
      // G3MH Software Manual, "SLD.H" (see extracted reference).
      // Decoder scales the 7-bit field left by 1 to produce byte-granular
      // disp8.
      disp = static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_7BIT_DISP)
                                  << OpcodeFields::SHIFT_IV_DISP);

      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.SignExtend(
              Sizes::LEN32BIT,
              il.Load(Sizes::LEN16BIT,
                      il.Add(Sizes::LEN32BIT,
                             il.Register(Sizes::LEN32BIT, Registers::EP),
                             il.Const(Sizes::LEN32BIT, disp))))));
      len = Sizes::LEN16BIT;
      return true;

    case Opcodes::OP_IV_4BIT_SST_H:
      // G3MH Software Manual p.271: Store halfword, address = EP +
      // zero_extend(disp8), where disp8 = raw7 << 1 (x2 scaling, already
      // applied by Extract7BitDisp8OpcodeField via MASK_IV_7BIT_DISP +
      // SHIFT_IV_DISP).  il.Store truncates the 32-bit source to 2 bytes.
      disp = Extract7BitDisp8OpcodeField(opcode);

      reg2_il = (reg2 == Registers::R0)
                    ? il.Const(Sizes::LEN32BIT, 0)
                    : il.Register(Sizes::LEN32BIT, reg2);
      il.AddInstruction(il.Store(
          Sizes::LEN16BIT,
          il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::EP),
                 il.Const(Sizes::LEN32BIT, disp)),
          reg2_il));

      len = Sizes::LEN16BIT;
      return true;

    case Opcodes::OP_IV_4BIT_SLD_W_OR_SST_W:
      if (subop == Opcodes::SUBOP_IV_SLD_W) {
        // sld.w disp8[ep], reg2 — word load, EP-relative
        // Encoding: `1010 0 rrr rr ddd ddd0` (G3MH software manual p. 5-57)
        // Disp is 6-bit field scaled ×4 for word addressing. The helper mask
        // `MASK_IV_6BIT_DISP << SHIFT_IV_DISP` already yields the byte offset
        // (raw d-value × 4), so no additional scaling is needed here.
        disp = static_cast<uint8_t>((opcode & OpcodeFields::MASK_IV_6BIT_DISP)
                                    << OpcodeFields::SHIFT_IV_DISP);

        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.Load(Sizes::LEN32BIT,
                    il.Add(Sizes::LEN32BIT,
                           il.Register(Sizes::LEN32BIT, Registers::EP),
                           il.Const(Sizes::LEN32BIT, disp)))));
        len = Sizes::LEN16BIT;
        return true;

      } else if (subop == Opcodes::SUBOP_IV_SST_W) {
        // G3MH Software Manual p.272: Store word, address = EP +
        // zero_extend(disp8), where disp8 = raw6 << 2 (x4 scaling, already
        // applied by Extract6BitDisp8OpcodeField: MASK_IV_6BIT_DISP isolates
        // bits [6:1] and SHIFT_IV_DISP (<<1) puts them at [7:2] with LSB=0).
        disp = Extract6BitDisp8OpcodeField(opcode);

        reg2_il = (reg2 == Registers::R0)
                      ? il.Const(Sizes::LEN32BIT, 0)
                      : il.Register(Sizes::LEN32BIT, reg2);
        il.AddInstruction(il.Store(
            Sizes::LEN32BIT,
            il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::EP),
                   il.Const(Sizes::LEN32BIT, disp)),
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

    const uint32_t target = static_cast<uint32_t>(addr) + disp22;
    BN::ExprId dest = il.ConstPointer(Sizes::LEN32BIT, target);

    if (reg2 == Registers::R0) {  // if reg2 is r0, is jr
      // Jump relative
      // Text format: jr disp22
      il.AddInstruction(il.Jump(dest));

      len = Sizes::LEN32BIT;
      return true;

    } else {
      // Jump and register link
      // Text format: jarl disp22, reg2
      const uint32_t return_pc =
          static_cast<uint32_t>(addr) + Sizes::LEN32BIT;
      il.AddInstruction(  // Save return PC in reg2
          il.SetRegister(Sizes::LEN32BIT, reg2,
                         il.Const(Sizes::LEN32BIT, return_pc)));

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
      // G3MH manual p.234-236: saves list12 registers (r20..r31) to stack in
      // ascending order (r20 at highest addr, r31 at lowest), then
      //   sp = sp - zero_extend(imm5 << 2)

      // Store specified general purpose registers (r20-r31) on the stack
      GenerateILToSaveRegisters(opcode, il);

      // sp = sp - ZeroExtend(imm5 << 2)
      const auto imm5 =
          static_cast<uint8_t>((opcode >> 16 & OpcodeFields::MASK_XIII_IMM5) >>
                               OpcodeFields::SHIFT_XIII_IMM5);
      const uint32_t frame_adjust = static_cast<uint32_t>(imm5) << 2;
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, Registers::SP,
          il.Sub(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::SP),
                 il.Const(Sizes::LEN32BIT, frame_adjust))));

      len = Sizes::LEN32BIT;
      return true;

    } else if (subop == Opcodes::SUBOP_XIII_PREPARE_011) {
      // Function prepare with EP update
      // Text format: prepare list12, imm5, sp/imm/imm16/imm32
      // G3MH manual p.234-236: same as form (1), then sets EP per ff field:
      //   ff=00: ep = sp
      //   ff=01: ep = sign_extend(imm16)
      //   ff=10: ep = imm16 << 16
      //   ff=11: ep = imm32

      // Store specified general purpose registers (r20-r31) on the stack
      GenerateILToSaveRegisters(opcode, il);

      // sp = sp - ZeroExtend(imm5 << 2)
      const auto imm5 =
          static_cast<uint8_t>((opcode >> 16 & OpcodeFields::MASK_XIII_IMM5) >>
                               OpcodeFields::SHIFT_XIII_IMM5);
      const uint32_t frame_adjust = static_cast<uint32_t>(imm5) << 2;
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, Registers::SP,
          il.Sub(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::SP),
                 il.Const(Sizes::LEN32BIT, frame_adjust))));

      int32_t imm = 0;
      const auto prepare_format = static_cast<uint8_t>(
          opcode >> 16 & Opcodes::MASK_XIII_PREPARE_FORMAT);
      switch (prepare_format) {
        case Opcodes::PREPARE_LOAD_SP:
          // EP = SP
          il.AddInstruction(
              il.SetRegister(Sizes::LEN32BIT, Registers::EP,
                             il.Register(Sizes::LEN32BIT, Registers::SP)));
          len = Sizes::LEN32BIT;
          return true;

        case Opcodes::PREPARE_LOAD_SIGN_EXTENDED_IMM16: {
          // Bits 47..32 hold imm16; sign-extend to 32 bits
          const uint16_t imm16 =
              static_cast<uint16_t>((opcode >> 32) & 0xFFFF);
          imm = static_cast<int32_t>(static_cast<int16_t>(imm16));
          len = Sizes::LEN48BIT;
          break;
        }

        case Opcodes::PREPARE_LOAD_LSL_IMM16: {
          // Bits 47..32 hold imm16; logically shift left by 16
          const uint16_t imm16 =
              static_cast<uint16_t>((opcode >> 32) & 0xFFFF);
          imm = static_cast<int32_t>(static_cast<uint32_t>(imm16) << 16);
          len = Sizes::LEN48BIT;
          break;
        }

        case Opcodes::PREPARE_LOAD_IMM32: {
          // Bits 47..32 = low 16; bits 63..48 = high 16
          const uint32_t lo = static_cast<uint32_t>((opcode >> 32) & 0xFFFF);
          const uint32_t hi = static_cast<uint32_t>((opcode >> 48) & 0xFFFF);
          imm = static_cast<int32_t>((hi << 16) | lo);
          len = Sizes::LEN64BIT;
          break;
        }
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
      // Per G3MH: disp16 is sign-extended to 32 bits before being added to reg1.
      auto reg2 = static_cast<uint8_t>((opcode & OpcodeFields::MASK_REG2) >>
                                       OpcodeFields::SHIFT_REG2);
      auto reg1 = static_cast<uint8_t>(opcode & OpcodeFields::MASK_REG1);
      int16_t disp16 = static_cast<int16_t>(
          (opcode >> 16 & OpcodeFields::MASK_VII_DISP) |
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
                                       il.Const(Sizes::LEN16BIT, disp16)))))));
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
    // Function dispose; G3MH manual p.177-178
    //   tmp = sp + zero_extend(imm5 << 2)
    //   foreach reg in list12: reg = mem[tmp]; tmp += 4
    //   sp = tmp
    //   (form 2) PC = reg1
    // Text format: dispose imm5, list12[, [reg1]]
    const auto imm5 =
        static_cast<uint8_t>((opcode >> 16 & OpcodeFields::MASK_XIII_IMM5) >>
                             OpcodeFields::SHIFT_XIII_IMM5);
    const auto reg1 =
        static_cast<uint8_t>(opcode >> 16 & OpcodeFields::MASK_REG1);
    const uint32_t frame_adjust = static_cast<uint32_t>(imm5) << 2;

    // Collapse local frame first so Pop reads from the saved-register area.
    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, Registers::SP,
        il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, Registers::SP),
               il.Const(Sizes::LEN32BIT, frame_adjust))));

    GenerateILToRestoreRegisters(opcode, il);

    if (reg1 != 0) {
      // dispose imm5, list12, [reg1] — tail return to reg1
      il.AddInstruction(il.Return(il.Register(Sizes::LEN32BIT, reg1)));
    }
    // else form (1): plain stack teardown; caller will follow with jmp [lp]

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
          Flags::FLAGS_WRITE_CY_OV_S_Z);  // SAT is sticky; set explicitly on sat branches
                                              // behavior

      il.AddInstruction(  // Check whether maximum negative value is exceeded
          il.If(il.CompareSignedLessThan(Sizes::LEN64BIT, result,
                                         il.Const(Sizes::LEN32BIT, 0x80000000)),
                sat_neg_true, sat_neg_false));

      il.MarkLabel(sat_neg_true);  // Saturated negative result
      il.AddInstruction(
          il.SetRegister(Sizes::LEN32BIT, reg2,
                         il.Const(Sizes::LEN32BIT, 0x80000000)));
      il.AddInstruction(il.SetFlag(Flags::FLAG_SAT_SATURATED,
                                   il.Const(Sizes::LEN8BIT, 1)));
      il.AddInstruction(il.Goto(done));

      il.MarkLabel(sat_neg_false);
      il.AddInstruction(  // Check whether maximum positive value is exceeded
          il.If(il.CompareSignedGreaterThan(
                    Sizes::LEN64BIT, result,
                    il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)),
                sat_pos_true, sat_pos_false));

      il.MarkLabel(sat_pos_true);  // Saturated positive result
      il.AddInstruction(
          il.SetRegister(Sizes::LEN32BIT, reg2,
                         il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)));
      il.AddInstruction(il.SetFlag(Flags::FLAG_SAT_SATURATED,
                                   il.Const(Sizes::LEN8BIT, 1)));
      il.AddInstruction(il.Goto(done));

      il.MarkLabel(sat_pos_false);  // Result NOT saturated
      il.AddInstruction(  // Store result just like normal subtract operation
          il.SetRegister(Sizes::LEN32BIT, reg2,
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
      // reg2 = reg1 | zero-extend(imm) ; writes S/Z, clears OV (per G3MH).
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.Or(Sizes::LEN32BIT, reg1_il,
                il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN16BIT, imm)),
                Flags::FLAGS_WRITE_S_Z)));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VI_6BIT_XORI:
      // reg2 = reg1 ^ zero-extend(imm) ; writes S/Z, clears OV (per G3MH).
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.Xor(Sizes::LEN32BIT, reg1_il,
                 il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN16BIT, imm)),
                 Flags::FLAGS_WRITE_S_Z)));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VI_6BIT_ANDI:
      // reg2 = reg1 & zero-extend(imm) ; writes S/Z, clears OV (per G3MH).
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.And(Sizes::LEN32BIT, reg1_il,
                 il.ZeroExtend(Sizes::LEN32BIT, il.Const(Sizes::LEN16BIT, imm)),
                 Flags::FLAGS_WRITE_S_Z)));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::OP_VI_6BIT_MULHI:
      // Multiply halfword by imm16 (signed).
      // Per G3MH p.224: GR[reg2] <- GR[reg1](15:0) * imm16, both signed.
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, reg2,
          il.Mult(Sizes::LEN32BIT,
                  il.SignExtend(Sizes::LEN32BIT,
                                il.LowPart(Sizes::LEN16BIT, reg1_il)),
                  il.SignExtend(Sizes::LEN32BIT,
                                il.Const(Sizes::LEN16BIT, imm)))));
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
      return false;  // Unknown subop; defensive (prevents switch fallthrough
                     // into ST_B below, which would misinterpret the opcode).

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
      return false;  // Unknown subop; defensive.
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

  // Per G3MH: Z <- NOT(bit_before). Emit an explicit SetFlag on the Z flag
  // by computing (loaded_byte AND mask) with FLAGS_WRITE_Z — the result is
  // discarded but the Z annotation is captured. We do the flag write BEFORE
  // the read-modify-write so the Z value reflects the bit's pre-state.
  bitmask = il.ShiftLeft(Sizes::LEN8BIT, il.Const(Sizes::LEN8BIT, 1),
                         il.Const(Sizes::LEN8BIT, bitNum));

  switch (subop) {
    case Opcodes::SUBOP_SET1:
      // Z flag: Z = !bit_before
      il.AddInstruction(il.And(Sizes::LEN8BIT,
                               il.Load(Sizes::LEN8BIT, addrIL), bitmask,
                               Flags::FLAGS_WRITE_Z));
      il.AddInstruction(il.Store(
          Sizes::LEN8BIT, addrIL,
          il.Or(Sizes::LEN8BIT, il.Load(Sizes::LEN8BIT, addrIL), bitmask)));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::SUBOP_CLR1:
      il.AddInstruction(il.And(Sizes::LEN8BIT,
                               il.Load(Sizes::LEN8BIT, addrIL), bitmask,
                               Flags::FLAGS_WRITE_Z));
      il.AddInstruction(il.Store(
          Sizes::LEN8BIT, addrIL,
          il.And(Sizes::LEN8BIT, il.Load(Sizes::LEN8BIT, addrIL),
                 il.Not(Sizes::LEN8BIT, bitmask))));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::SUBOP_NOT1:
      // Bitwise not operation: not1 bit#3, disp16[reg1]
      il.AddInstruction(il.And(Sizes::LEN8BIT,
                               il.Load(Sizes::LEN8BIT, addrIL), bitmask,
                               Flags::FLAGS_WRITE_Z));
      il.AddInstruction(il.Store(
          Sizes::LEN8BIT, addrIL,
          il.Xor(Sizes::LEN8BIT, il.Load(Sizes::LEN8BIT, addrIL), bitmask)));
      len = Sizes::LEN32BIT;
      return true;

    case Opcodes::SUBOP_TST1:
      // Bitwise test: tst1 bit#3, disp16[reg1]
      // Per G3MH p.263: Z <- Not(extract-bit(M[reg1+se(disp16)], bit#3)).
      // And of the loaded byte with a single-bit mask: Z=1 iff bit was 0.
      // Result discarded (tst1 doesn't write memory or a register).
      il.AddInstruction(il.And(Sizes::LEN8BIT,
                               il.Load(Sizes::LEN8BIT, addrIL), bitmask,
                               Flags::FLAGS_WRITE_Z));
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

  // Format IX: BINS (bitfield insert). G3MH p.162.
  //   GR[reg2] <- GR[reg2](31:pos+width) || GR[reg1](width-1:0) || GR[reg2](pos-1:0)
  // Sub-opcode bits (5..10) are 001001 / 001011 / 001101; collides with
  // HALT/EI/DI which have all reg/field bits zero. Discriminator matches
  // decoder and text handlers.
  {
    const auto bins_subop =
        (opcode >> 16 & OpcodeFields::MASK_IX_SUBOP_BINS) >>
        OpcodeFields::SHIFT_IX_SUBOP_BINS;
    const bool bins_subop_match =
        (bins_subop == Opcodes::SUBOP_IX_BINS_HI ||
         bins_subop == Opcodes::SUBOP_IX_BINS_MID ||
         bins_subop == Opcodes::SUBOP_IX_BINS_LO);
    const auto mmmm = (opcode >> 16 & OpcodeFields::MASK_IX_BINS_MMMM) >>
                      OpcodeFields::SHIFT_IX_BINS_MMMM;
    const auto k = (opcode >> 16 & OpcodeFields::MASK_IX_BINS_K) >>
                   OpcodeFields::SHIFT_IX_BINS_K;
    const auto lll = (opcode >> 16 & OpcodeFields::MASK_IX_BINS_LLL) >>
                     OpcodeFields::SHIFT_IX_BINS_LLL;
    if (bins_subop_match &&
        (reg1 != 0 || reg2 != 0 || mmmm != 0 || k != 0 || lll != 0)) {
      const uint8_t lsb_low = static_cast<uint8_t>((k << 3) | lll);
      uint8_t msb = 0, lsb = 0;
      switch (bins_subop) {
        case Opcodes::SUBOP_IX_BINS_HI:
          msb = static_cast<uint8_t>(16 | mmmm);
          lsb = static_cast<uint8_t>(16 | lsb_low);
          break;
        case Opcodes::SUBOP_IX_BINS_MID:
          msb = static_cast<uint8_t>(16 | mmmm);
          lsb = lsb_low;
          break;
        case Opcodes::SUBOP_IX_BINS_LO:
          msb = static_cast<uint8_t>(mmmm);
          lsb = lsb_low;
          break;
        default:
          break;
      }
      if (msb >= lsb) {
        const uint8_t pos = lsb;
        const uint8_t width = static_cast<uint8_t>(msb - lsb + 1);
        const uint32_t field_mask =
            (width >= 32)
                ? 0xFFFFFFFFu
                : ((static_cast<uint32_t>(1) << width) - 1u);
        const uint32_t insert_mask = field_mask << pos;
        const uint32_t keep_mask = ~insert_mask;

        // new_val = (reg2 & keep_mask) | ((reg1 & field_mask) << pos)
        BN::ExprId src_field =
            il.ShiftLeft(Sizes::LEN32BIT,
                         il.And(Sizes::LEN32BIT,
                                il.Register(Sizes::LEN32BIT, reg1),
                                il.Const(Sizes::LEN32BIT, field_mask)),
                         il.Const(Sizes::LEN32BIT, pos));
        BN::ExprId kept =
            il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg2),
                   il.Const(Sizes::LEN32BIT, keep_mask));
        il.AddInstruction(il.SetRegister(
            Sizes::LEN32BIT, reg2,
            il.Or(Sizes::LEN32BIT, kept, src_field)));
        len = Sizes::LEN32BIT;
        return true;
      }
    }
  }

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
          OpcodeFields::OPCODE_BIT_5) {  // 01101 bsw/bsh/hsw; 01111 mac/macu
        if (opcode >> 16 & OpcodeFields::OPCODE_BIT_4) {
          // MAC / MACU reg1, reg2, reg3, reg4 (Format XI, V850E2/E3)
          // GR[reg4+1]||GR[reg4] <- GR[reg2]*GR[reg1] + GR[reg3+1]||GR[reg3]
          // reg3 field = bits[20:16], reg4 field = bits[31:27]
          // Variable `reg3` above already extracted = MAC reg4 (bits[31:27]).
          // G3MH Software Manual p. 215 (MAC) / p. 216 (MACU).
          const auto mac_reg3 =
              static_cast<uint8_t>(opcode >> 16 & OpcodeFields::MASK_REG1);
          const auto mac_reg4 = reg3;
          const auto mac_reg3_hi =
              static_cast<uint8_t>(mac_reg3 + 1);  // GR[reg3+1]
          const auto mac_reg4_hi =
              static_cast<uint8_t>(mac_reg4 + 1);  // GR[reg4+1]

          // 64-bit product: manual describes both MAC and MACU with signed
          // operands; MACU differs only in intermediate treatment (documented
          // signed in manual; implementation-defined). We use signed for MAC
          // and unsigned for MACU per mnemonic semantics.
          BN::ExprId prod;
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {  // MACU
            prod = il.MultDoublePrecUnsigned(
                Sizes::LEN64BIT, il.Register(Sizes::LEN32BIT, reg2),
                il.Register(Sizes::LEN32BIT, reg1));
          } else {  // MAC
            prod = il.MultDoublePrecSigned(
                Sizes::LEN64BIT, il.Register(Sizes::LEN32BIT, reg2),
                il.Register(Sizes::LEN32BIT, reg1));
          }

          // Build 64-bit accumulator: (GR[reg3+1] << 32) | GR[reg3]
          BN::ExprId acc = il.Or(
              Sizes::LEN64BIT,
              il.ShiftLeft(Sizes::LEN64BIT,
                           il.ZeroExtend(Sizes::LEN64BIT,
                                         il.Register(Sizes::LEN32BIT,
                                                     mac_reg3_hi)),
                           il.Const(Sizes::LEN32BIT, 32)),
              il.ZeroExtend(Sizes::LEN64BIT,
                            il.Register(Sizes::LEN32BIT, mac_reg3)));

          BN::ExprId sum = il.Add(Sizes::LEN64BIT, prod, acc);

          // Low 32 bits -> reg4; high 32 bits -> reg4+1. r0 always reads 0 and
          // writes discarded, so skip the SetRegister on r0.
          if (mac_reg4 != Registers::R0) {
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, mac_reg4, il.LowPart(Sizes::LEN32BIT, sum)));
          }
          if (mac_reg4_hi != Registers::R0) {
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, mac_reg4_hi,
                il.LowPart(Sizes::LEN32BIT,
                           il.LogicalShiftRight(
                               Sizes::LEN64BIT, sum,
                               il.Const(Sizes::LEN32BIT, 32)))));
          }
          len = Sizes::LEN32BIT;
          return true;
        }
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

          case Opcodes::SUBOP_XII_HSH:
            // Halfword swap halfword (V850E3); reg3 = reg2 (value unchanged)
            // Text format: hsh reg2, reg3
            // Flags: OV=0, S=sign(reg3), Z=(reg3==0), CY=(reg2[15:0]==0)
            il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3,
                                             il.Register(Sizes::LEN32BIT, reg2)));
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
          // Conditional move (register source form)
          // Text format: cmov cccc, reg1, reg2, reg3
          // Semantics: reg3 = cond ? reg1 : reg2
          // Condition must be evaluated at runtime, not at lift time; emit
          // an if/else branch so both data paths are represented in LLIL.
          BN::LowLevelILLabel cmov_true, cmov_false, cmov_done;
          il.AddInstruction(il.If(ConditionToIL(condition_CMOV, il), cmov_true,
                                  cmov_false));
          il.MarkLabel(cmov_true);
          il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3,
                                           il.Register(Sizes::LEN32BIT, reg1)));
          il.AddInstruction(il.Goto(cmov_done));
          il.MarkLabel(cmov_false);
          il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3,
                                           il.Register(Sizes::LEN32BIT, reg2)));
          il.AddInstruction(il.Goto(cmov_done));
          il.MarkLabel(cmov_done);
          len = Sizes::LEN32BIT;
          return true;

        } else {  // 011000, format XII cmov
          // Conditional move (immediate source form)
          // Text format: cmov ccc, imm5, reg2, reg3
          // Semantics: reg3 = cond ? sign_extend(imm5) : reg2
          BN::LowLevelILLabel cmov_true, cmov_false, cmov_done;
          il.AddInstruction(il.If(ConditionToIL(condition_CMOV, il), cmov_true,
                                  cmov_false));
          il.MarkLabel(cmov_true);
          il.AddInstruction(
              il.SetRegister(Sizes::LEN32BIT, reg3,
                             il.SignExtend(Sizes::LEN32BIT,
                                           il.Const(Sizes::LEN8BIT, imm5))));
          il.AddInstruction(il.Goto(cmov_done));
          il.MarkLabel(cmov_false);
          il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3,
                                           il.Register(Sizes::LEN32BIT, reg2)));
          il.AddInstruction(il.Goto(cmov_done));
          il.MarkLabel(cmov_done);
          len = Sizes::LEN32BIT;
          return true;
        }
      }
    } else {                                            // 010
      if (opcode >> 16 & OpcodeFields::OPCODE_BIT_4) {  // 0101
        if (opcode >> 16 &
            OpcodeFields::OPCODE_BIT_5) {  // 01011; format XI div/divu/divq/divqu
          // G3MH p. 179 (DIV), 187 (DIVU), 183 (DIVQ), 185 (DIVQU):
          //   reg2 <- reg2 / reg1   (quotient)
          //   reg3 <- (original reg2) % reg1   (remainder)
          // If reg2 == reg3, final value is remainder (per manual).
          // DIV/DIVU and DIVQ/DIVQU share this register-effect pattern; they
          // only differ in microarchitectural execution time (variable-step
          // for the Q forms). Semantically identical for LLIL purposes.
          const bool is_unsigned =
              (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) != 0;
          // Save original reg2 so we can compute remainder correctly even
          // after we overwrite reg2 with the quotient.
          il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, LLIL_TEMP(0),
                                           il.Register(Sizes::LEN32BIT, reg2)));
          // Quotient -> reg2
          il.AddInstruction(il.SetRegister(
              Sizes::LEN32BIT, reg2,
              is_unsigned
                  ? il.DivUnsigned(Sizes::LEN32BIT,
                                   il.Register(Sizes::LEN32BIT, LLIL_TEMP(0)),
                                   il.Register(Sizes::LEN32BIT, reg1))
                  : il.DivSigned(Sizes::LEN32BIT,
                                 il.Register(Sizes::LEN32BIT, LLIL_TEMP(0)),
                                 il.Register(Sizes::LEN32BIT, reg1)),
              Flags::FLAGS_WRITE_OV_S_Z));
          // Remainder -> reg3 (using saved original reg2)
          if (reg3 != Registers::R0) {
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg3,
                is_unsigned
                    ? il.ModUnsigned(Sizes::LEN32BIT,
                                     il.Register(Sizes::LEN32BIT, LLIL_TEMP(0)),
                                     il.Register(Sizes::LEN32BIT, reg1))
                    : il.ModSigned(Sizes::LEN32BIT,
                                   il.Register(Sizes::LEN32BIT, LLIL_TEMP(0)),
                                   il.Register(Sizes::LEN32BIT, reg1))));
          }
          len = Sizes::LEN32BIT;
          return true;
        } else {  // 01010; format XI divh, divhu
          // G3MH p. 180 (DIVH format 2), 182 (DIVHU):
          //   reg2 <- reg2 / {sign,zero}_extend(reg1[15:0])
          //   reg3 <- (original reg2) % {sign,zero}_extend(reg1[15:0])
          // reg2's dividend is the full 32-bit word; only reg1 is narrowed to
          // its lower halfword (sign- or zero-extended to 32 bits).
          const bool is_unsigned =
              (opcode >> 16 & OpcodeFields::MASK_SUBOP_BIT_17) != 0;
          // Save original reg2
          il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, LLIL_TEMP(0),
                                           il.Register(Sizes::LEN32BIT, reg2)));
          // Narrow reg1 to 16 bits and re-extend to 32 bits per signedness.
          // Using the low half of reg1 as a 16-bit load-register-field, then
          // sign/zero-extending, gives a divisor with proper sign semantics.
          BN::ExprId divisor =
              is_unsigned
                  ? il.ZeroExtend(
                        Sizes::LEN32BIT,
                        il.And(Sizes::LEN32BIT,
                               il.Register(Sizes::LEN32BIT, reg1),
                               il.Const(Sizes::LEN32BIT, 0xFFFF)))
                  : il.SignExtend(
                        Sizes::LEN32BIT,
                        il.LowPart(Sizes::LEN16BIT,
                                   il.Register(Sizes::LEN32BIT, reg1)));
          // Quotient -> reg2
          il.AddInstruction(il.SetRegister(
              Sizes::LEN32BIT, reg2,
              is_unsigned
                  ? il.DivUnsigned(Sizes::LEN32BIT,
                                   il.Register(Sizes::LEN32BIT, LLIL_TEMP(0)),
                                   divisor)
                  : il.DivSigned(Sizes::LEN32BIT,
                                 il.Register(Sizes::LEN32BIT, LLIL_TEMP(0)),
                                 divisor),
              Flags::FLAGS_WRITE_OV_S_Z));
          // Remainder -> reg3
          if (reg3 != Registers::R0) {
            // Re-materialize divisor for the mod expression (each IL expr is
            // single-use; can't share ExprId across two SetRegister calls).
            BN::ExprId divisor2 =
                is_unsigned
                    ? il.ZeroExtend(
                          Sizes::LEN32BIT,
                          il.And(Sizes::LEN32BIT,
                                 il.Register(Sizes::LEN32BIT, reg1),
                                 il.Const(Sizes::LEN32BIT, 0xFFFF)))
                    : il.SignExtend(
                          Sizes::LEN32BIT,
                          il.LowPart(Sizes::LEN16BIT,
                                     il.Register(Sizes::LEN32BIT, reg1)));
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg3,
                is_unsigned
                    ? il.ModUnsigned(Sizes::LEN32BIT,
                                     il.Register(Sizes::LEN32BIT, LLIL_TEMP(0)),
                                     divisor2)
                    : il.ModSigned(Sizes::LEN32BIT,
                                   il.Register(Sizes::LEN32BIT, LLIL_TEMP(0)),
                                   divisor2)));
          }
          len = Sizes::LEN32BIT;
          return true;
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
            // reg2 = (reg2 << 1) | (cond ? 1 : 0)
            // BoolToInt widens the flag-condition bool to a 32-bit int so
            // the OR operands agree in size.
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2,
                il.Or(Sizes::LEN32BIT,
                      il.ShiftLeft(Sizes::LEN32BIT,
                                   il.Register(Sizes::LEN32BIT, reg2),
                                   il.Const(Sizes::LEN32BIT, 1)),
                      il.BoolToInt(Sizes::LEN32BIT,
                                   ConditionToIL(condition_sasf, il)))));
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
          // 001010; format X reti, ctret, dbret
          switch (opcode >> 16 & OpcodeFields::MASK_SUBOP_BITS_17_18) {
            case Opcodes::SUBOP_X_RETI: {
              // Return from trap or interrupt. Per V850E3/G3MH manual:
              //   if (PSW.NP == 1) { pc <- FEPC; PSW <- FEPSW }
              //   else             { pc <- EIPC; PSW <- EIPSW }
              // The old code evaluated PSW.NP at *lift time* via
              // il.GetExprValue, which always returns 0 for a runtime load —
              // so the FEPC/FEPSW path was unreachable. Emit a proper If on
              // a runtime load of PSW.NP and let the optimizer prune it only
              // if a constant PSW has actually been propagated.
              BN::LowLevelILLabel np_true, np_false, done;
              BN::ExprId psw_np_set = il.And(
                  Sizes::LEN32BIT,
                  il.LogicalShiftRight(
                      Sizes::LEN32BIT,
                      il.Load(Sizes::LEN32BIT,
                              il.ConstPointer(
                                  Sizes::LEN32BIT,
                                  Registers::SYSTEM_REG_BASE +
                                      Registers::V850_REG_PSW *
                                          Registers::REGISTER_SIZE)),
                      il.Const(Sizes::LEN8BIT, Flags::FLAG_NP_NMI_PENDING)),
                  il.Const(Sizes::LEN32BIT, 1));
              il.AddInstruction(il.If(
                  il.CompareEqual(Sizes::LEN32BIT, psw_np_set,
                                  il.Const(Sizes::LEN32BIT, 1)),
                  np_true, np_false));

              // NP == 1: FEPC / FEPSW path
              il.MarkLabel(np_true);
              il.AddInstruction(il.Store(
                  Sizes::LEN32BIT,  // psw <- fepsw
                  il.ConstPointer(
                      Sizes::LEN32BIT,
                      Registers::SYSTEM_REG_BASE +
                          Registers::V850_REG_PSW * Registers::REGISTER_SIZE),
                  il.Load(Sizes::LEN32BIT,
                          il.ConstPointer(Sizes::LEN32BIT,
                                          Registers::SYSTEM_REG_BASE +
                                              Registers::V850_REG_FEPSW *
                                                  Registers::REGISTER_SIZE))));
              il.AddInstruction(il.Return(il.Load(
                  Sizes::LEN32BIT,  // pc <- fepc
                  il.ConstPointer(Sizes::LEN32BIT,
                                  Registers::SYSTEM_REG_BASE +
                                      Registers::V850_REG_FEPC *
                                          Registers::REGISTER_SIZE))));

              // NP == 0: EIPC / EIPSW path
              il.MarkLabel(np_false);
              il.AddInstruction(il.Store(
                  Sizes::LEN32BIT,  // psw <- eipsw
                  il.ConstPointer(
                      Sizes::LEN32BIT,
                      Registers::SYSTEM_REG_BASE +
                          Registers::V850_REG_PSW * Registers::REGISTER_SIZE),
                  il.Load(Sizes::LEN32BIT,
                          il.ConstPointer(Sizes::LEN32BIT,
                                          Registers::SYSTEM_REG_BASE +
                                              Registers::V850_REG_EIPSW *
                                                  Registers::REGISTER_SIZE))));
              il.AddInstruction(il.Return(il.Load(
                  Sizes::LEN32BIT,  // pc <- eipc
                  il.ConstPointer(Sizes::LEN32BIT,
                                  Registers::SYSTEM_REG_BASE +
                                      Registers::V850_REG_EIPC *
                                          Registers::REGISTER_SIZE))));

              len = Sizes::LEN32BIT;
              return true;
            }

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
          // Halt; stop operating clock of CPU and place into halt mode.
          // Execution resumes on reset/NMI/unmasked maskable interrupt
          // request (G3MH software manual p. 192), so control DOES fall
          // through to the next instruction. Using il.NoReturn() here
          // would terminate the basic block / function in BN's CFG, which
          // is wrong for idle-loop patterns where code after halt is live
          // after wakeup. Model as Nop until a v850.halt intrinsic exists.
          // TODO: replace with il.Intrinsic({}, "v850.halt", {}) once the
          // architecture registers intrinsics.
          il.AddInstruction(il.Nop());

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
                    il.And(Sizes::LEN8BIT,  // Bitnum is lower 3 bits of reg2
                           il.Register(Sizes::LEN8BIT, reg2),
                           il.Const(Sizes::LEN8BIT, 0b111)));
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
                    il.And(Sizes::LEN8BIT,  // Bitnum is lower 3 bits of reg2
                           il.Register(Sizes::LEN8BIT, reg2),
                           il.Const(Sizes::LEN8BIT, 0b111)));
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
            // Logical shift left (G3MH manual: shl reg1, reg2)
            // Shift count = reg1[4:0]; flags: CY = last bit shifted out,
            // S = sign of result, Z = zero, OV = 0.
            len = Sizes::LEN32BIT;
            if (reg2 == Registers::R0) {
              return true;  // r0 is hardwired zero; writes discarded
            }
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2,
                il.ShiftLeft(Sizes::LEN32BIT,
                             il.Register(Sizes::LEN32BIT, reg2),
                             il.And(Sizes::LEN32BIT,
                                    il.Register(Sizes::LEN32BIT, reg1),
                                    il.Const(Sizes::LEN32BIT, 0x1F)),
                             Flags::FLAGS_WRITE_CY_OV_S_Z)));
            return true;
          }
        } else {                                            // 00010
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {  // 000101; sar
            // Arithmetic shift right (G3MH manual: sar reg1, reg2)
            // Shift count = reg1[4:0]; sign-propagating. Flags: CY = last bit
            // shifted out, S = sign of result, Z = zero, OV = 0.
            len = Sizes::LEN32BIT;
            if (reg2 == Registers::R0) {
              return true;
            }
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2,
                il.ArithShiftRight(Sizes::LEN32BIT,
                                   il.Register(Sizes::LEN32BIT, reg2),
                                   il.And(Sizes::LEN32BIT,
                                          il.Register(Sizes::LEN32BIT, reg1),
                                          il.Const(Sizes::LEN32BIT, 0x1F)),
                                   Flags::FLAGS_WRITE_CY_OV_S_Z)));
            return true;

          } else {  // 000100; shr
            // Logical shift right (G3MH manual: shr reg1, reg2)
            // Shift count = reg1[4:0]; zero-fill. Flags: CY = last bit shifted
            // out, S = sign of result, Z = zero, OV = 0.
            len = Sizes::LEN32BIT;
            if (reg2 == Registers::R0) {
              return true;
            }
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2,
                il.LogicalShiftRight(Sizes::LEN32BIT,
                                     il.Register(Sizes::LEN32BIT, reg2),
                                     il.And(Sizes::LEN32BIT,
                                            il.Register(Sizes::LEN32BIT, reg1),
                                            il.Const(Sizes::LEN32BIT, 0x1F)),
                                     Flags::FLAGS_WRITE_CY_OV_S_Z)));
            return true;
          }
        }
      } else {  // 0000
        if (opcode >> 16 &
            OpcodeFields::OPCODE_BIT_5) {  // 00001; Only option is 000010; stsr
          // Store contents of system register (regID in reg1 field) into
          // general-purpose register reg2. regID/selID select the banked
          // system register per G3MH p.~195.
          const uint8_t sel_id = static_cast<uint8_t>((opcode >> 27) & 0x1F);
          const uint32_t sysreg_handle =
              Registers::SysregHandle(reg1, sel_id);
          il.AddInstruction(il.SetRegister(
              Sizes::LEN32BIT, reg2,
              il.Register(Sizes::LEN32BIT, sysreg_handle)));
          len = Sizes::LEN32BIT;
          return true;

        } else {                                            // 00000
          if (opcode >> 16 & OpcodeFields::OPCODE_BIT_6) {  // 000001; ldsr
            // Load to system register (regID in reg1 field) from reg2.
            // regID/selID select the banked system register per G3MH p.~130.
            // TODO special handling of flags! if the system register is the
            // PSW (regID=5, selID=0), the values of the corresponding bits
            // of PSW (aka the flags) are set according to contents of reg2.
            const uint8_t sel_id = static_cast<uint8_t>((opcode >> 27) & 0x1F);
            const uint32_t sysreg_handle =
                Registers::SysregHandle(reg1, sel_id);
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, sysreg_handle,
                il.Register(Sizes::LEN32BIT, reg2)));

            len = Sizes::LEN32BIT;
            return true;

          } else {  // 000000; setf
            // Set flag condition; if condition is met, sets reg2 to 1; if not,
            // sets to 0 Text format: setf cond, reg2
            auto condition_setf =
                static_cast<uint8_t>(opcode & OpcodeFields::MASK_IX_COND);

            // reg2 = (cond) ? 1 : 0 ; BoolToInt widens the flag-condition
            // bool to a 32-bit integer so the type matches the destination
            // register.
            il.AddInstruction(il.SetRegister(
                Sizes::LEN32BIT, reg2,
                il.BoolToInt(Sizes::LEN32BIT,
                             ConditionToIL(condition_setf, il))));

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

bool BinsR1PosWidthR2::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
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

bool SyncBarrier::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  (void)opcode;
  (void)addr;
  (void)arch;
  (void)intrinsic;  // no intrinsic registry on this arch yet; lower to NOP.
  // SYNCE/SYNCI/SYNCM/SYNCP have no observable register-level side effects on
  // this CPU (G3MH Software Manual pp. 287-290). Emit a NOP as a standalone
  // instruction (not nested in an expression) so LLIL stays well-formed.
  il.AddInstruction(il.Nop());
  len = Sizes::LEN16BIT;
  return true;
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

bool HshR2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
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

bool MacR1R2R3R4::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il,
                       BinaryNinja::Architecture *arch) {
  return Format_Ext_Lift(opcode, addr, len, il);
}

bool MacuR1R2R3R4::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
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

static void LiftReturnFromException(BN::LowLevelILFunction &il,
                                    uint32_t pc_src_reg_idx,
                                    uint32_t psw_src_reg_idx) {
  il.AddInstruction(il.Store(
      Sizes::LEN32BIT,
      il.ConstPointer(
          Sizes::LEN32BIT,
          Registers::SYSTEM_REG_BASE +
              Registers::V850_REG_PSW * Registers::REGISTER_SIZE),
      il.Load(Sizes::LEN32BIT,
              il.ConstPointer(Sizes::LEN32BIT,
                              Registers::SYSTEM_REG_BASE +
                                  psw_src_reg_idx * Registers::REGISTER_SIZE))));
  il.AddInstruction(il.Return(il.Load(
      Sizes::LEN32BIT,
      il.ConstPointer(Sizes::LEN32BIT,
                      Registers::SYSTEM_REG_BASE +
                          pc_src_reg_idx * Registers::REGISTER_SIZE))));
}

bool Eiret::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  // EIRET: pc <- EIPC; PSW <- EIPSW
  LiftReturnFromException(il, Registers::V850_REG_EIPC,
                          Registers::V850_REG_EIPSW);
  len = Sizes::LEN32BIT;
  return true;
}

bool Feret::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  // FERET: pc <- FEPC; PSW <- FEPSW
  LiftReturnFromException(il, Registers::V850_REG_FEPC,
                          Registers::V850_REG_FEPSW);
  len = Sizes::LEN32BIT;
  return true;
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

/* -----------------------------------------------------------------
 * V850E3 / RH850 G3MH additions
 * ----------------------------------------------------------------- */

/* Lift a PUSHSP as a sequence of 4-byte stores in ascending-register /
 * descending-address order, followed by SP update.
 *
 *   cur = rh
 *   while cur <= rt {
 *     sp -= 4
 *     mem[sp & ~3] = GR[cur]
 *     cur += 1
 *   }
 *
 * We unroll the loop at decode time because rh, rt are both immediates in
 * the instruction encoding.
 */
bool PushspRhRt::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  const auto rh = ExtractReg1OpcodeField(opcode);
  const auto rt = ExtractReg3OpcodeField(opcode);
  len = Sizes::LEN32BIT;

  if (rh > rt) {
    // Undefined per G3MH; emit nop so analysis keeps moving.
    il.AddInstruction(il.Nop());
    return true;
  }

  for (uint8_t cur = rh; cur <= rt; ++cur) {
    // sp = sp - 4
    il.AddInstruction(
        il.SetRegister(Sizes::LEN32BIT, Registers::SP,
                       il.Sub(Sizes::LEN32BIT,
                              il.Register(Sizes::LEN32BIT, Registers::SP),
                              il.Const(Sizes::LEN32BIT, 4))));
    // store(sp & ~3, GR[cur])
    il.AddInstruction(il.Store(
        Sizes::LEN32BIT,
        il.And(Sizes::LEN32BIT,
               il.Register(Sizes::LEN32BIT, Registers::SP),
               il.Const(Sizes::LEN32BIT, 0xFFFFFFFCu)),
        il.Register(Sizes::LEN32BIT, cur)));
    if (cur == 31) break;  // guard against wrap
  }
  return true;
}

/* Lift a POPSP:
 *   cur = rt
 *   while cur >= rh {
 *     GR[cur] = mem[sp & ~3]   (but if cur == SP the load is discarded)
 *     sp += 4
 *     cur -= 1
 *   }
 * Again unrolled because rh, rt are immediates.
 */
bool PopspRhRt::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture *arch) {
  const auto rh = ExtractReg1OpcodeField(opcode);
  const auto rt = ExtractReg3OpcodeField(opcode);
  len = Sizes::LEN32BIT;

  if (rh > rt) {
    il.AddInstruction(il.Nop());
    return true;
  }

  for (int cur = rt; cur >= rh; --cur) {
    if (cur != Registers::SP && cur != Registers::R0) {
      il.AddInstruction(il.SetRegister(
          Sizes::LEN32BIT, static_cast<uint32_t>(cur),
          il.Load(Sizes::LEN32BIT,
                  il.And(Sizes::LEN32BIT,
                         il.Register(Sizes::LEN32BIT, Registers::SP),
                         il.Const(Sizes::LEN32BIT, 0xFFFFFFFCu)))));
    }
    il.AddInstruction(
        il.SetRegister(Sizes::LEN32BIT, Registers::SP,
                       il.Add(Sizes::LEN32BIT,
                              il.Register(Sizes::LEN32BIT, Registers::SP),
                              il.Const(Sizes::LEN32BIT, 4))));
  }
  return true;
}

/* CAXI [reg1], reg2, reg3 — compare-and-swap.
 *
 * Semantics (G3MH p.167):
 *   adr    = GR[reg1]
 *   token  = mem[adr]
 *   result = GR[reg2] - token           (and sets flags, which we skip)
 *   if result == 0
 *     mem[adr] = GR[reg3]
 *   GR[reg3] = token
 *
 * We lift this structurally; atomicity isn't expressible in LLIL without an
 * intrinsic, but the visible data-flow is correct. Flag side-effects are
 * approximated by leaving them to the decompiler (no flag writes emitted).
 */
// Format XI saturated arithmetic (3-operand) — G3MH p.244/246/249.
// reg3 = saturated(op(reg1, reg2)). Stickiness modelled by only setting
// SAT on the saturation branches (matches 2-operand SATADD/SATSUB lifts).
static bool LiftSatFmtXi(uint64_t opcode, size_t &len,
                         BN::LowLevelILFunction &il, bool is_add,
                         bool reverse) {
  const auto reg1 = ExtractReg1OpcodeField(opcode);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg3 = ExtractReg3OpcodeField(opcode);
  BN::LowLevelILLabel sat_pos_true, sat_pos_false, sat_neg_true,
      sat_neg_false, done;
  BN::ExprId lhs = il.Register(Sizes::LEN32BIT, reverse ? reg1 : reg2);
  BN::ExprId rhs = il.Register(Sizes::LEN32BIT, reverse ? reg2 : reg1);
  BN::ExprId result =
      is_add ? il.Add(Sizes::LEN64BIT, il.Register(Sizes::LEN32BIT, reg1),
                      il.Register(Sizes::LEN32BIT, reg2),
                      Flags::FLAGS_WRITE_CY_OV_S_Z)
             : il.Sub(Sizes::LEN64BIT, lhs, rhs,
                      Flags::FLAGS_WRITE_CY_OV_S_Z);
  il.AddInstruction(il.If(
      il.CompareSignedLessThan(Sizes::LEN64BIT, result,
                               il.Const(Sizes::LEN32BIT, 0x80000000)),
      sat_neg_true, sat_neg_false));
  il.MarkLabel(sat_neg_true);
  il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3,
                                   il.Const(Sizes::LEN32BIT, 0x80000000)));
  il.AddInstruction(il.SetFlag(Flags::FLAG_SAT_SATURATED,
                               il.Const(Sizes::LEN8BIT, 1)));
  il.AddInstruction(il.Goto(done));
  il.MarkLabel(sat_neg_false);
  il.AddInstruction(il.If(
      il.CompareSignedGreaterThan(Sizes::LEN64BIT, result,
                                  il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)),
      sat_pos_true, sat_pos_false));
  il.MarkLabel(sat_pos_true);
  il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3,
                                   il.Const(Sizes::LEN32BIT, 0x7FFFFFFF)));
  il.AddInstruction(il.SetFlag(Flags::FLAG_SAT_SATURATED,
                               il.Const(Sizes::LEN8BIT, 1)));
  il.AddInstruction(il.Goto(done));
  il.MarkLabel(sat_pos_false);
  il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3, result));
  il.AddInstruction(il.Goto(done));
  il.MarkLabel(done);
  len = Sizes::LEN32BIT;
  return true;
}

bool SataddR1R2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture * /*arch*/) {
  return LiftSatFmtXi(opcode, len, il, /*is_add=*/true, /*reverse=*/false);
}
bool SatsubR1R2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture * /*arch*/) {
  return LiftSatFmtXi(opcode, len, il, /*is_add=*/false, /*reverse=*/false);
}
bool SatsubrR1R2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture * /*arch*/) {
  return LiftSatFmtXi(opcode, len, il, /*is_add=*/false, /*reverse=*/true);
}

bool CaxiR1R2R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture *arch) {
  const auto reg1 = ExtractReg1OpcodeField(opcode);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg3 = ExtractReg3OpcodeField(opcode);
  len = Sizes::LEN32BIT;

  // temp = mem[reg1]
  BN::LowLevelILLabel true_label, false_label, done_label;

  // Use SP-distinct scratch: write token into reg3 last (as spec), but we
  // need it before the conditional store. Use a temp via LLIL_TEMP.
  const uint32_t tmp = LLIL_TEMP(0);
  il.AddInstruction(il.SetRegister(
      Sizes::LEN32BIT, tmp,
      il.Load(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1))));

  // if (GR[reg2] == tmp) goto true else goto false
  il.AddInstruction(
      il.If(il.CompareEqual(Sizes::LEN32BIT,
                            il.Register(Sizes::LEN32BIT, reg2),
                            il.Register(Sizes::LEN32BIT, tmp)),
            true_label, false_label));

  // true: mem[reg1] = GR[reg3]
  il.MarkLabel(true_label);
  il.AddInstruction(il.Store(Sizes::LEN32BIT,
                             il.Register(Sizes::LEN32BIT, reg1),
                             il.Register(Sizes::LEN32BIT, reg3)));
  il.AddInstruction(il.Goto(done_label));

  // false: nothing (spec also stores token back, but that's a no-op read)
  il.MarkLabel(false_label);
  il.AddInstruction(il.Goto(done_label));

  il.MarkLabel(done_label);
  // GR[reg3] = tmp
  if (reg3 != Registers::R0) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3,
                                     il.Register(Sizes::LEN32BIT, tmp)));
  }
  return true;
}

/* JARL [reg1], reg3 — call through register, saving PC+4 in reg3. */
bool JarlR1R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) {
  const auto reg1 = ExtractReg1OpcodeField(opcode);
  const auto reg3 = ExtractReg3OpcodeField(opcode);
  len = Sizes::LEN32BIT;

  if (reg3 != Registers::R0) {
    il.AddInstruction(il.SetRegister(
        Sizes::LEN32BIT, reg3,
        il.Const(Sizes::LEN32BIT, addr + Sizes::LEN32BIT)));
  }
  il.AddInstruction(
      il.Call(il.And(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                     il.Const(Sizes::LEN32BIT, 0xFFFFFFFEu))));
  return true;
}

/* SNOOZE — pause until release event. Treated as an observable no-op so
 * control flow continues to the next instruction. */
bool Snooze::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                  BN::LowLevelILFunction &il,
                  BinaryNinja::Architecture *arch) {
  len = Sizes::LEN32BIT;
  il.AddInstruction(il.Nop());
  return true;
}

/* RIE — reserved instruction exception. Both forms raise the handler at
 * offset 0x60 (FE-level exception). Lift as a trap so BN treats it as
 * control flow termination. */
bool RieI::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  len = Sizes::LEN16BIT;
  il.AddInstruction(il.Trap(0x60));
  return true;
}

bool RieX::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch) {
  len = Sizes::LEN32BIT;
  il.AddInstruction(il.Trap(0x60));
  return true;
}

/* SCH0L / SCH0R / SCH1L / SCH1R — bit-search instructions (Format IX).
 *
 * Per RH850 G3MH §7 (SCH0L/SCH0R/SCH1L/SCH1R, pp.251–254):
 *   GR[reg3] = (number of non-matching bits before first matching bit) + 1
 *              counted from MSB (L-variants) or LSB (R-variants),
 *              searching for 0 (SCH0*) or 1 (SCH1*).
 *   If no matching bit is found: reg3 = 0, Z = 1.
 *   CY = 1 iff the match is at the furthest bit (i.e. reg3 == 32).
 *   S = 0, OV = 0, SAT unchanged.
 *
 * LLIL has no native find-first-bit primitive, so lift to a registered
 * intrinsic (BitIntrinsic::Sch*) with reg3 as the output and reg2 as the
 * input. The PSW flag side-effects are modelled explicitly after the
 * intrinsic so Binary Ninja's data-flow can still reason about callers
 * that branch on CY/Z (e.g. the common "is value zero?" idiom).
 */
static void LiftSchCommon(const uint64_t opcode, size_t &len,
                          BN::LowLevelILFunction &il, uint32_t intrinsic_id) {
  len = Sizes::LEN32BIT;
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg3 = ExtractReg3OpcodeField(opcode);
  constexpr size_t W = Sizes::LEN32BIT;

  il.AddInstruction(il.Intrinsic(
      {BN::RegisterOrFlag::Register(reg3)}, intrinsic_id,
      {il.Register(W, reg2)}));

  // S, OV are always cleared.
  il.AddInstruction(il.SetFlag(Flags::FLAG_S_SIGN, il.Const(0, 0)));
  il.AddInstruction(il.SetFlag(Flags::FLAG_OV_OVERFLOW, il.Const(0, 0)));
  // Z = (reg3 == 0) — "not found".
  il.AddInstruction(il.SetFlag(
      Flags::FLAG_Z_ZERO,
      il.CompareEqual(W, il.Register(W, reg3), il.Const(W, 0))));
  // CY = (reg3 == 32) — match at the farthest bit (MSB for SCH*R / LSB for
  // SCH*L). 32 also happens to be the max legal value.
  il.AddInstruction(il.SetFlag(
      Flags::FLAG_CY_CARRY,
      il.CompareEqual(W, il.Register(W, reg3), il.Const(W, 32))));
}

bool Sch0lR2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture * /*arch*/) {
  LiftSchCommon(opcode, len, il, BitIntrinsic::Sch0l);
  return true;
}
bool Sch0rR2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture * /*arch*/) {
  LiftSchCommon(opcode, len, il, BitIntrinsic::Sch0r);
  return true;
}
bool Sch1lR2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture * /*arch*/) {
  LiftSchCommon(opcode, len, il, BitIntrinsic::Sch1l);
  return true;
}
bool Sch1rR2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture * /*arch*/) {
  LiftSchCommon(opcode, len, il, BitIntrinsic::Sch1r);
  return true;
}
/* --- Single-precision FPU lifting --- */

bool FpuDouble::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture * /*arch*/) {
  // Generic double-precision FPU lift. Double-precision operands live in
  // register pairs {rN, rN+1} on RH850 (even N), but we don't know which
  // operand is source vs destination without per-op knowledge, so we model
  // it conservatively as "reads reg1/reg2, writes reg3", matching how the
  // single-precision intrinsics in this plugin behave. Per-op correct
  // lifts will supersede this as they land.
  constexpr size_t W = Sizes::LEN32BIT;
  const auto reg1 = ExtractReg1OpcodeField(opcode);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg3 = ExtractReg3OpcodeField(opcode);
  il.AddInstruction(il.Intrinsic(
      {BN::RegisterOrFlag::Register(reg3)},
      FpuIntrinsic::FpuD,
      {il.Register(W, reg1), il.Register(W, reg2)}));
  len = Sizes::LEN32BIT;
  return true;
}

bool FpuSingle::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                     BN::LowLevelILFunction &il,
                     BN::Architecture * /*arch*/) {
  const auto reg1 = ExtractReg1OpcodeField(opcode);
  const auto reg2 = ExtractReg2OpcodeField(opcode);
  const auto reg3 = ExtractReg3OpcodeField(opcode);
  const auto hw2 = static_cast<uint16_t>(opcode >> 16);
  const uint32_t fcbit = (hw2 >> 1) & 0b111;
  const uint32_t fcond = (reg3 >> 3) & 0b1111;
  constexpr size_t W = Sizes::LEN32BIT;

  auto Ra = [&](uint8_t r) { return il.Register(W, r); };
  auto Set = [&](uint8_t r, BN::ExprId v) {
    il.AddInstruction(il.SetRegister(W, r, v));
  };
  auto Intr = [&](uint32_t id, std::vector<BN::ExprId> ins,
                  std::vector<BN::RegisterOrFlag> outs) {
    il.AddInstruction(il.Intrinsic(outs, id, ins));
  };

  switch (op) {
    /* ---- arithmetic: reg3 = fop(reg2, reg1) ---- */
    case FpuOp::AddfS:
      Set(reg3, il.FloatAdd(W, Ra(reg2), Ra(reg1)));
      break;
    case FpuOp::SubfS:
      Set(reg3, il.FloatSub(W, Ra(reg2), Ra(reg1)));
      break;
    case FpuOp::MulfS:
      Set(reg3, il.FloatMult(W, Ra(reg2), Ra(reg1)));
      break;
    case FpuOp::DivfS:
      Set(reg3, il.FloatDiv(W, Ra(reg2), Ra(reg1)));
      break;
    case FpuOp::MaxfS:
      Intr(FpuIntrinsic::MaxfS, {Ra(reg2), Ra(reg1)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::MinfS:
      Intr(FpuIntrinsic::MinfS, {Ra(reg2), Ra(reg1)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;

    /* ---- unary arithmetic ---- */
    case FpuOp::AbsfS:
      Set(reg3, il.FloatAbs(W, Ra(reg2)));
      break;
    case FpuOp::NegfS:
      Set(reg3, il.FloatNeg(W, Ra(reg2)));
      break;
    case FpuOp::SqrtfS:
      Set(reg3, il.FloatSqrt(W, Ra(reg2)));
      break;
    case FpuOp::RecipfS:
      Intr(FpuIntrinsic::RecipfS, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::RsqrtfS:
      Intr(FpuIntrinsic::RsqrtfS, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;

    /* ---- float -> signed-int 32 conversions. BN's FloatToInt is
            round-toward-zero (truncation). For floor/ceil/round we use
            intrinsics to preserve semantics. CVTF.SW uses the current
            FPSR rounding mode -- we approximate with FloatToInt. ---- */
    case FpuOp::TrncfSw:
      Set(reg3, il.FloatToInt(W, Ra(reg2)));
      break;
    case FpuOp::CvtfSw:
      Set(reg3, il.FloatToInt(W, Ra(reg2)));
      break;
    case FpuOp::RoundfSw:
      Intr(FpuIntrinsic::RoundfSw, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::CeilfSw:
      Intr(FpuIntrinsic::CeilfSw, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::FloorfSw:
      Intr(FpuIntrinsic::FloorfSw, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;

    /* ---- float -> unsigned-int 32 conversions (all intrinsic) ---- */
    case FpuOp::RoundfSuw:
      Intr(FpuIntrinsic::RoundfSuw, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::TrncfSuw:
      Intr(FpuIntrinsic::TrncfSuw, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::CeilfSuw:
      Intr(FpuIntrinsic::CeilfSuw, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::FloorfSuw:
      Intr(FpuIntrinsic::FloorfSuw, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::CvtfSuw:
      Intr(FpuIntrinsic::CvtfSuw, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;

    /* ---- int -> float, float <-> half ---- */
    case FpuOp::CvtfWs:
      Set(reg3, il.IntToFloat(W, Ra(reg2)));
      break;
    case FpuOp::CvtfUws:
      Intr(FpuIntrinsic::CvtfUws, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::CvtfHs:
      Intr(FpuIntrinsic::CvtfHs, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::CvtfSh:
      Intr(FpuIntrinsic::CvtfSh, {Ra(reg2)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;

    /* ---- FMA family: reg3 = fma(reg2, reg1, reg3) etc. ---- */
    case FpuOp::FmafS:
      Intr(FpuIntrinsic::FmafS, {Ra(reg2), Ra(reg1), Ra(reg3)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::FmsfS:
      Intr(FpuIntrinsic::FmsfS, {Ra(reg2), Ra(reg1), Ra(reg3)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::FnmafS:
      Intr(FpuIntrinsic::FnmafS, {Ra(reg2), Ra(reg1), Ra(reg3)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;
    case FpuOp::FnmsfS:
      Intr(FpuIntrinsic::FnmsfS, {Ra(reg2), Ra(reg1), Ra(reg3)},
           {BN::RegisterOrFlag::Register(reg3)});
      break;

    /* ---- CMPF.S: writes FPSR CC bit; model as intrinsic with FPSR out ---- */
    case FpuOp::CmpfS:
      Intr(FpuIntrinsic::CmpfS,
           {il.Const(W, fcond), Ra(reg2), Ra(reg1), il.Const(W, fcbit)},
           {BN::RegisterOrFlag::Register(Registers::FPSR)});
      break;

    /* ---- CMOVF.S: reg3 = FPSR.CC[fcbit] ? reg1 : reg2 ---- */
    case FpuOp::CmovfS: {
      /* FPSR CC bits live in bits 31..24. fcbit selects which CCn. */
      auto ccn = il.And(
          W,
          il.LogicalShiftRight(W, il.Register(W, Registers::FPSR),
                               il.Const(W, 24 + fcbit)),
          il.Const(W, 1));
      BN::LowLevelILLabel t, f, done;
      il.AddInstruction(il.If(ccn, t, f));
      il.MarkLabel(t);
      il.AddInstruction(il.SetRegister(W, reg3, Ra(reg1)));
      il.AddInstruction(il.Goto(done));
      il.MarkLabel(f);
      il.AddInstruction(il.SetRegister(W, reg3, Ra(reg2)));
      il.AddInstruction(il.Goto(done));
      il.MarkLabel(done);
      break;
    }

    /* ---- TRFSR: PSW.Z <- FPSR.CC[fcbit] ---- */
    case FpuOp::Trfsr:
      Intr(FpuIntrinsic::Trfsr,
           {il.Const(W, fcbit), il.Register(W, Registers::FPSR)},
           {});
      break;
  }

  len = Sizes::LEN32BIT;
  return true;
}

/* ---- Format XIV (48-bit disp23 LD/ST) lifts ---- */
bool LdbDisp23R1R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_XIV_Load(opcode, len, il, Sizes::LEN8BIT,
                       /*sign_extend=*/true, /*aligned=*/false);
}

bool LdhDisp23R1R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_XIV_Load(opcode, len, il, Sizes::LEN16BIT,
                       /*sign_extend=*/true, /*aligned=*/true);
}

bool LdwDisp23R1R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_XIV_Load(opcode, len, il, Sizes::LEN32BIT,
                       /*sign_extend=*/true, /*aligned=*/true);
}

bool LdbuDisp23R1R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                          BN::LowLevelILFunction &il,
                          BinaryNinja::Architecture *arch) {
  return Lift_XIV_Load(opcode, len, il, Sizes::LEN8BIT,
                       /*sign_extend=*/false, /*aligned=*/false);
}

bool LdhuDisp23R1R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                          BN::LowLevelILFunction &il,
                          BinaryNinja::Architecture *arch) {
  // Per SLEIGH: disp uses op2026 (byte-granular) for this variant.
  return Lift_XIV_Load(opcode, len, il, Sizes::LEN16BIT,
                       /*sign_extend=*/false, /*aligned=*/false);
}

bool StbR3Disp23R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_XIV_Store(opcode, len, il, Sizes::LEN8BIT,
                        /*aligned=*/false);
}

bool SthR3Disp23R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_XIV_Store(opcode, len, il, Sizes::LEN16BIT,
                        /*aligned=*/true);
}

bool StwR3Disp23R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                         BN::LowLevelILFunction &il,
                         BinaryNinja::Architecture *arch) {
  return Lift_XIV_Store(opcode, len, il, Sizes::LEN32BIT,
                        /*aligned=*/true);
}

/* TODO: ld.dw / st.dw operate on a V850E3 register pair (R2731pairEx).
 * Accurate lifting requires the reg-pair convention to be decided. Mark
 * as Unimplemented for now so decompilation surfaces the instruction
 * without silently lifting wrong semantics. */
bool LddwDisp23R1R3::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                          BN::LowLevelILFunction &il,
                          BinaryNinja::Architecture *arch) {
  il.AddInstruction(il.Unimplemented());
  len = Sizes::LEN48BIT;
  return true;
}

bool StdwR3Disp23R1::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                          BN::LowLevelILFunction &il,
                          BinaryNinja::Architecture *arch) {
  il.AddInstruction(il.Unimplemented());
  len = Sizes::LEN48BIT;
  return true;
}

/* ---- V850E3 post-inc / pre-dec LD/ST lifts ----
 *
 * Per Ghidra SLEIGH v850e3.sinc (lines 302..382), BOTH post-increment and
 * pre-decrement variants take the effective address as the original reg1
 * value; the SLEIGH semantics perform the memory access first and THEN
 * adjust reg1 by +/- access_size. Lift this exactly.
 *
 * Note: if reg1 == reg3 on a load, the SLEIGH behaviour is that reg3 is
 * written first (with the loaded value) and then reg1 (== reg3) is
 * overwritten by the writeback. We emit the same sequence, matching
 * SLEIGH. */
static bool Lift_PIpD_Load(const uint64_t opcode, size_t &len,
                           BN::LowLevelILFunction &il, size_t access_size,
                           bool sign_extend, int delta) {
  const auto reg1 = ExtractReg1OpcodeField(static_cast<uint16_t>(opcode));
  const auto reg3 = static_cast<uint8_t>(
      (static_cast<uint16_t>(opcode >> 16) & OpcodeFields::MASK_XI_REG3) >>
      OpcodeFields::SHIFT_XI_REG3);

  BN::ExprId load_il =
      il.Load(access_size, il.Register(Sizes::LEN32BIT, reg1));
  BN::ExprId extended = sign_extend ? il.SignExtend(Sizes::LEN32BIT, load_il)
                                    : il.ZeroExtend(Sizes::LEN32BIT, load_il);
  il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3, extended));

  // Writeback: reg1 <- reg1 +/- access_size.
  BN::ExprId writeback =
      (delta >= 0)
          ? il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                   il.Const(Sizes::LEN32BIT, static_cast<uint32_t>(delta)))
          : il.Sub(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                   il.Const(Sizes::LEN32BIT, static_cast<uint32_t>(-delta)));
  il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg1, writeback));

  len = Sizes::LEN32BIT;
  return true;
}

static bool Lift_PIpD_Store(const uint64_t opcode, size_t &len,
                            BN::LowLevelILFunction &il, size_t access_size,
                            int delta) {
  const auto reg1 = ExtractReg1OpcodeField(static_cast<uint16_t>(opcode));
  const auto reg3 = static_cast<uint8_t>(
      (static_cast<uint16_t>(opcode >> 16) & OpcodeFields::MASK_XI_REG3) >>
      OpcodeFields::SHIFT_XI_REG3);

  il.AddInstruction(il.Store(access_size, il.Register(Sizes::LEN32BIT, reg1),
                             il.Register(access_size, reg3)));

  BN::ExprId writeback =
      (delta >= 0)
          ? il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                   il.Const(Sizes::LEN32BIT, static_cast<uint32_t>(delta)))
          : il.Sub(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
                   il.Const(Sizes::LEN32BIT, static_cast<uint32_t>(-delta)));
  il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg1, writeback));

  len = Sizes::LEN32BIT;
  return true;
}

#define V850_PIPD_LOAD_LIFT(CLS, ACCESS, SIGN, DELTA)                      \
  bool CLS::Lift(const uint64_t opcode, uint64_t addr, size_t &len,        \
                 BN::LowLevelILFunction &il,                               \
                 BinaryNinja::Architecture *arch) {                        \
    return Lift_PIpD_Load(opcode, len, il, ACCESS, SIGN, DELTA);           \
  }
#define V850_PIPD_STORE_LIFT(CLS, ACCESS, DELTA)                           \
  bool CLS::Lift(const uint64_t opcode, uint64_t addr, size_t &len,        \
                 BN::LowLevelILFunction &il,                               \
                 BinaryNinja::Architecture *arch) {                        \
    return Lift_PIpD_Store(opcode, len, il, ACCESS, DELTA);                \
  }

V850_PIPD_LOAD_LIFT(LdbPostIncR1R3,  Sizes::LEN8BIT,  true,  +1)
V850_PIPD_LOAD_LIFT(LdhPostIncR1R3,  Sizes::LEN16BIT, true,  +2)
V850_PIPD_LOAD_LIFT(LdwPostIncR1R3,  Sizes::LEN32BIT, true,  +4)
V850_PIPD_LOAD_LIFT(LdbuPostIncR1R3, Sizes::LEN8BIT,  false, +1)
V850_PIPD_LOAD_LIFT(LdhuPostIncR1R3, Sizes::LEN16BIT, false, +2)
V850_PIPD_LOAD_LIFT(LdbPreDecR1R3,   Sizes::LEN8BIT,  true,  -1)
V850_PIPD_LOAD_LIFT(LdhPreDecR1R3,   Sizes::LEN16BIT, true,  -2)
V850_PIPD_LOAD_LIFT(LdwPreDecR1R3,   Sizes::LEN32BIT, true,  -4)
V850_PIPD_LOAD_LIFT(LdbuPreDecR1R3,  Sizes::LEN8BIT,  false, -1)
V850_PIPD_LOAD_LIFT(LdhuPreDecR1R3,  Sizes::LEN16BIT, false, -2)
V850_PIPD_STORE_LIFT(StbPostIncR3R1, Sizes::LEN8BIT,  +1)
V850_PIPD_STORE_LIFT(SthPostIncR3R1, Sizes::LEN16BIT, +2)
V850_PIPD_STORE_LIFT(StwPostIncR3R1, Sizes::LEN32BIT, +4)
V850_PIPD_STORE_LIFT(StbPreDecR3R1,  Sizes::LEN8BIT,  -1)
V850_PIPD_STORE_LIFT(SthPreDecR3R1,  Sizes::LEN16BIT, -2)
V850_PIPD_STORE_LIFT(StwPreDecR3R1,  Sizes::LEN32BIT, -4)

#undef V850_PIPD_LOAD_LIFT
#undef V850_PIPD_STORE_LIFT

// ----------------------------------------------------------------------
// V850E3 / RH850 G3MH: ADF / SBF / ROTL / LOOP / CACHE / PREF
// ----------------------------------------------------------------------

// ADF cond, reg1, reg2, reg3: reg3 = reg2 + reg1 + (cond ? 1 : 0)
// SBF cond, reg1, reg2, reg3: reg3 = reg2 - reg1 - (cond ? 1 : 0)
// Both write CY/OV/S/Z. SLEIGH adds an extra carry check for the lsb
// bump; modelled here with il.AddCarry / il.SubBorrow fed from the
// evaluated cond expression zero-extended to 32 bits.
static bool LiftAdfSbfCcc(const uint64_t opcode, size_t &len,
                          BN::LowLevelILFunction &il, bool is_add) {
  const auto reg1 = ExtractReg1OpcodeField(static_cast<uint16_t>(opcode));
  const auto reg2 = ExtractReg2OpcodeField(static_cast<uint16_t>(opcode));
  const auto reg3 = static_cast<uint8_t>(
      ((opcode >> 16) & OpcodeFields::MASK_XI_REG3) >>
      OpcodeFields::SHIFT_XI_REG3);
  const auto cond = ExtractTypeXICond(static_cast<uint32_t>(opcode));

  BN::ExprId cond_bit =
      il.BoolToInt(Sizes::LEN32BIT, ConditionToIL(cond, il));
  BN::ExprId result;
  if (is_add) {
    result = il.AddCarry(Sizes::LEN32BIT,
                         il.Register(Sizes::LEN32BIT, reg2),
                         il.Register(Sizes::LEN32BIT, reg1), cond_bit,
                         Flags::FLAGS_WRITE_CY_OV_S_Z);
  } else {
    result = il.SubBorrow(Sizes::LEN32BIT,
                          il.Register(Sizes::LEN32BIT, reg2),
                          il.Register(Sizes::LEN32BIT, reg1), cond_bit,
                          Flags::FLAGS_WRITE_CY_OV_S_Z);
  }
  if (reg3 != Registers::R0) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3, result));
  } else {
    il.AddInstruction(result);
  }
  len = Sizes::LEN32BIT;
  return true;
}

bool AdfCccR1R2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture * /*arch*/) {
  return LiftAdfSbfCcc(opcode, len, il, /*is_add=*/true);
}
bool SbfCccR1R2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture * /*arch*/) {
  return LiftAdfSbfCcc(opcode, len, il, /*is_add=*/false);
}

// ROTL shift, reg2, reg3: reg3 = (reg2 <<< (shift & 0x1F)).
// Writes S/Z (OV=0, CY=msb-of-result & shift!=0). Here we rely on LLIL
// RotateLeft to express the data effect cleanly; flag updates match the
// add-instruction pattern (S/Z only) since BN has no rotate-flag combo.
static bool LiftRotl(const uint64_t opcode, size_t &len,
                     BN::LowLevelILFunction &il, BN::ExprId shift_expr) {
  const auto reg2 = ExtractReg2OpcodeField(static_cast<uint16_t>(opcode));
  const auto reg3 = static_cast<uint8_t>(
      ((opcode >> 16) & OpcodeFields::MASK_XI_REG3) >>
      OpcodeFields::SHIFT_XI_REG3);
  BN::ExprId rotated = il.RotateLeft(Sizes::LEN32BIT,
                                     il.Register(Sizes::LEN32BIT, reg2),
                                     shift_expr, Flags::FLAGS_WRITE_S_Z);
  if (reg3 != Registers::R0) {
    il.AddInstruction(il.SetRegister(Sizes::LEN32BIT, reg3, rotated));
  } else {
    il.AddInstruction(rotated);
  }
  len = Sizes::LEN32BIT;
  return true;
}

bool RotlR1R2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                      BN::LowLevelILFunction &il,
                      BinaryNinja::Architecture * /*arch*/) {
  const auto reg1 = ExtractReg1OpcodeField(static_cast<uint16_t>(opcode));
  BN::ExprId shift = il.And(Sizes::LEN32BIT,
                            il.Register(Sizes::LEN32BIT, reg1),
                            il.Const(Sizes::LEN32BIT, 0x1F));
  return LiftRotl(opcode, len, il, shift);
}

bool RotlImm5R2R3::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture * /*arch*/) {
  const uint8_t imm5 = ExtractReg1OpcodeField(static_cast<uint16_t>(opcode));
  BN::ExprId shift = il.Const(Sizes::LEN32BIT, imm5);
  return LiftRotl(opcode, len, il, shift);
}

// LOOP reg1, disp16: reg1 = reg1 - 1; if (new reg1 != 0) goto target.
// CY flag mirrors the carry of (old_reg1 + (-1)) per SLEIGH. We only
// model the control-flow + decrement here; flag side-effects on loop
// iteration are not load-bearing for decompiler output.
bool LoopR1Disp16::Lift(const uint64_t opcode, uint64_t addr, size_t &len,
                        BN::LowLevelILFunction &il,
                        BinaryNinja::Architecture *arch) {
  const auto reg1 = ExtractReg1OpcodeField(static_cast<uint16_t>(opcode));
  const uint16_t hw2 = static_cast<uint16_t>(opcode >> 16);
  const uint16_t disp_field =
      static_cast<uint16_t>((hw2 & Opcodes::MASK_LOOP_DISP) >>
                            Opcodes::SHIFT_LOOP_DISP);
  const uint32_t target =
      static_cast<uint32_t>(addr) - (static_cast<uint32_t>(disp_field) << 1);

  len = Sizes::LEN32BIT;

  // reg1 = reg1 - 1 (flag write on the add of -1, matching SLEIGH).
  il.AddInstruction(il.SetRegister(
      Sizes::LEN32BIT, reg1,
      il.Add(Sizes::LEN32BIT, il.Register(Sizes::LEN32BIT, reg1),
             il.Const(Sizes::LEN32BIT, 0xFFFFFFFFu),
             Flags::FLAGS_WRITE_CY_OV_S_Z)));

  BN::ExprId dest_if_true = il.Const(Sizes::LEN32BIT, target);
  BN::ExprId dest_if_false =
      il.Const(Sizes::LEN32BIT, addr + Sizes::LEN32BIT);
  BNLowLevelILLabel *t = il.GetLabelForAddress(arch, target);
  BNLowLevelILLabel *f =
      il.GetLabelForAddress(arch, addr + Sizes::LEN32BIT);
  BN::LowLevelILLabel local_true, local_false;
  const bool indirect_true = (t == nullptr);
  const bool indirect_false = (f == nullptr);
  il.AddInstruction(il.If(il.CompareNotEqual(Sizes::LEN32BIT,
                                             il.Register(Sizes::LEN32BIT, reg1),
                                             il.Const(Sizes::LEN32BIT, 0)),
                          t ? *t : local_true,
                          f ? *f : local_false));
  if (indirect_true) {
    il.MarkLabel(local_true);
    il.AddInstruction(il.Jump(dest_if_true));
  }
  if (indirect_false) {
    il.MarkLabel(local_false);
  }
  (void)dest_if_false;
  return true;
}

// CACHE: cache maintenance; emit an intrinsic so decompiler output
// preserves the operation and its effective address without pretending
// it modifies memory.
bool CacheOpR1::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &len,
                     BN::LowLevelILFunction &il,
                     BinaryNinja::Architecture * /*arch*/) {
  const auto reg1 = ExtractReg1OpcodeField(static_cast<uint16_t>(opcode));
  const uint8_t cacheop = static_cast<uint8_t>(
      ((ExtractReg2OpcodeField(static_cast<uint16_t>(opcode)) &
        Opcodes::MASK_REG2_LO2) << 5) |
      ((static_cast<uint16_t>(opcode >> 16) &
        Opcodes::MASK_CACHE_PREF_OP2731) >>
       Opcodes::SHIFT_CACHE_PREF_OP2731));
  il.AddInstruction(il.Intrinsic(
      {}, CacheIntrinsic::Cache,
      {il.Const(Sizes::LEN32BIT, cacheop),
       il.Register(Sizes::LEN32BIT, reg1)}));
  len = Sizes::LEN32BIT;
  return true;
}

// PREF: prefetch hint — architecturally a NOP.
bool PrefOpR1::Lift(const uint64_t /*opcode*/, uint64_t /*addr*/, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture * /*arch*/) {
  il.AddInstruction(il.Nop());
  len = Sizes::LEN32BIT;
  return true;
}

/* V850E3 / RH850 supervisor / debug / TLB mnemonics.
 *
 * These ops have architectural effects (TLB shootdown, debug unit
 * interaction, SYSCALL vectoring through SCBP, scoped-trap activation)
 * that we do not model at the LLIL register level. Lower them as opaque
 * intrinsics so the decompiler preserves the call site. Control-flow
 * ops (SYSCALL) are additionally marked in Info so BN knows execution
 * transfers away. */
bool NoOperandSystemOp::Lift(const uint64_t /*opcode*/, uint64_t /*addr*/,
                             size_t &out_len, BN::LowLevelILFunction &il,
                             BinaryNinja::Architecture * /*arch*/) {
  il.AddInstruction(il.Intrinsic({}, intrinsic_id, {}));
  out_len = GetInstrLen();
  return true;
}

bool Syscall::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &out_len,
                   BN::LowLevelILFunction &il,
                   BinaryNinja::Architecture * /*arch*/) {
  // vector8 = (hw2[13:11] << 5) | hw1[4:0]. See v850_special.sinc.
  const uint16_t hw2 = static_cast<uint16_t>(opcode >> 16);
  const uint8_t vec_hi = static_cast<uint8_t>((hw2 >> 11) & 0x07);
  const uint8_t vec_lo = static_cast<uint8_t>(opcode & 0x1F);
  const uint8_t vector = static_cast<uint8_t>((vec_hi << 5) | vec_lo);
  // SYSCALL vectors through SCBP; we don't model SCBP so call an opaque
  // intrinsic then let fallthrough continue. Treating as an observable
  // side-effecting call preserves caller analysis without inventing a
  // register model.
  il.AddInstruction(il.Intrinsic(
      {}, SystemIntrinsic::Syscall,
      {il.Const(Sizes::LEN32BIT, vector)}));
  out_len = Sizes::LEN32BIT;
  return true;
}

bool Dbpush::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &out_len,
                  BN::LowLevelILFunction &il,
                  BinaryNinja::Architecture * /*arch*/) {
  const uint16_t hw2 = static_cast<uint16_t>(opcode >> 16);
  const uint8_t first = static_cast<uint8_t>(opcode & 0x1F);
  const uint8_t last = static_cast<uint8_t>((hw2 >> 11) & 0x1F);
  il.AddInstruction(il.Intrinsic(
      {}, SystemIntrinsic::Dbpush,
      {il.Const(Sizes::LEN32BIT, first), il.Const(Sizes::LEN32BIT, last)}));
  out_len = Sizes::LEN32BIT;
  return true;
}

bool Dbtag::Lift(const uint64_t opcode, uint64_t /*addr*/, size_t &out_len,
                 BN::LowLevelILFunction &il,
                 BinaryNinja::Architecture * /*arch*/) {
  const uint16_t hw2 = static_cast<uint16_t>(opcode >> 16);
  const uint16_t imm_hi = static_cast<uint16_t>((hw2 >> 11) & 0x1F);
  const uint16_t imm_lo = static_cast<uint16_t>(opcode & 0x1F);
  const uint16_t imm10 = static_cast<uint16_t>((imm_hi << 5) | imm_lo);
  il.AddInstruction(il.Intrinsic(
      {}, SystemIntrinsic::Dbtag,
      {il.Const(Sizes::LEN32BIT, imm10)}));
  out_len = Sizes::LEN32BIT;
  return true;
}

}  // namespace V850
