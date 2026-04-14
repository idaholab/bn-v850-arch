// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#ifndef BINARYNINJA_API_V850_REGISTERS_H
#define BINARYNINJA_API_V850_REGISTERS_H

namespace V850::Registers {
constexpr int REGISTER_SIZE = 4;
/* ----------------- *
 * Program Registers *
 * ----------------- */
constexpr uint8_t R0 =
    0;  // always holds 0; used in operations and offset-0 addressing
constexpr uint8_t R1 =
    1;  // address generation; used implicitly by the assembler and c compiler
constexpr uint8_t R2 =
    2;  // may be used by rtos; if not, can be used for a variable
constexpr uint8_t SP =
    3;  // stack pointer (SP); used implicitly by the assembler and c compiler
constexpr uint8_t R4 =
    4;  // global pointer (GP); used implicitly by the assembler and c compiler
constexpr uint8_t R5 =
    5;  // text pointer (TP); used implicitly by the assembler and c compiler
constexpr uint8_t R6 = 6;
constexpr uint8_t R7 = 7;
constexpr uint8_t R8 = 8;
constexpr uint8_t R9 = 9;
constexpr uint8_t R10 = 10;
constexpr uint8_t R11 = 11;
constexpr uint8_t R12 = 12;
constexpr uint8_t R13 = 13;
constexpr uint8_t R14 = 14;
constexpr uint8_t R15 = 15;
constexpr uint8_t R16 = 16;
constexpr uint8_t R17 = 17;
constexpr uint8_t R18 = 18;
constexpr uint8_t R19 = 19;
constexpr uint8_t R20 = 20;
constexpr uint8_t R21 = 21;
constexpr uint8_t R22 = 22;
constexpr uint8_t R23 = 23;
constexpr uint8_t R24 = 24;
constexpr uint8_t R25 = 25;
constexpr uint8_t R26 = 26;
constexpr uint8_t R27 = 27;
constexpr uint8_t R28 = 28;
constexpr uint8_t R29 = 29;
constexpr uint8_t EP = 30;  // element pointer (EP); base pointer when accessing
                            // memory using the SLD and SST instructions
constexpr uint8_t R31 =
    31;  // link pointer (LP); used implicitly by the assembler and c compiler

/* ---------------- *
 * System Registers *
 * ---------------- */
constexpr uint32_t SYSTEM_REG_BASE = 0xFFF00000;
constexpr uint32_t V850_REG_EIPC =
    0;  // Exception/interrupt PC; stores contents of PC if exception or
        // interrupt occurs
constexpr uint32_t V850_REG_EIPSW =
    1;  // Exception/interrupt PSW; stores contents of PSW if exception or
        // interrupt occurs
constexpr uint32_t V850_REG_FEPC = 2;  // Fatal error PC; saves PC if NMI occurs
constexpr uint32_t V850_REG_FEPSW =
    3;  // Fatal error PSW; saves PSW if NMI occurs
constexpr uint32_t V850_REG_ECR =
    4;  // Exception cause register; holds cause of exception, maskable
        // interrupt, or NMI; read-only
constexpr uint32_t V850_REG_PSW =
    5;  // Program status word; flags indicating program status system registers
        // 6-15 are reserved
constexpr uint32_t V850_REG_CTPC = 16;   // Callt caller status saving register
constexpr uint32_t V850_REG_CTPSW = 17;  // Callt caller status saving register
constexpr uint32_t V850_REG_DBPC =
    18;  // Exception/debug trap status saving register
constexpr uint32_t V850_REG_DBPSW =
    19;  // Exception/debug trap status saving register
constexpr uint32_t V850_REG_CTBP = 20;  // Callt base pointer
constexpr uint32_t V850_REG_DIR = 21;   // Debug interface register
constexpr uint32_t V850_REG_BPC = 22;   // Breakpoint control registers
constexpr uint32_t V850_REG_ASID = 23;  // Program ID register
constexpr uint32_t V850_REG_BPAV = 24;  // Breakpoint address setting registers
constexpr uint32_t V850_REG_BPAM = 25;  // Breakpoint address mask registers
constexpr uint32_t V850_REG_BPDV = 26;  // Breakpoint data setting registers
constexpr uint32_t V850_REG_BPDM = 27;  // Breakpoint data mask registers
// system registers 28-31 are reserved

/* -------------------------------- *
 *      Banked System Registers     *
 * -------------------------------- *
 * RH850 G3MH (V850E3v5) extends the V850 system register file with banked
 * sets addressed by (regID, selID). Each of the 32 regIDs may exist in up
 * to 32 banks selected by selID. The V850 base bank is selID = 0.
 *
 * We encode a banked system register as a flat register "handle":
 *     handle = SYSTEM_REG_BASE | (selID << 8) | (regID << 2)
 *
 * For selID = 0, this matches the legacy encoding (SYSTEM_REG_BASE +
 * regID * 4) that was previously emitted as a ConstPointer for memory-
 * backed system register loads/stores. Banked handles (selID > 0) occupy
 * unique addresses above the base bank and never collide with GPRs
 * (0..31) or with the float registers at 0xFFF10000. */
constexpr uint32_t SYSREG_HANDLE_STRIDE = 0x100;  // one bank of 32 regs
inline constexpr uint32_t SysregHandle(uint8_t regID, uint8_t selID) {
  return SYSTEM_REG_BASE | (static_cast<uint32_t>(selID) << 8) |
         (static_cast<uint32_t>(regID) << 2);
}
inline constexpr bool IsSysregHandle(uint32_t rid) {
  return (rid >= SYSTEM_REG_BASE) && (rid < 0xFFF10000);
}
inline constexpr uint8_t SysregHandleRegID(uint32_t rid) {
  return static_cast<uint8_t>((rid >> 2) & 0x1F);
}
inline constexpr uint8_t SysregHandleSelID(uint32_t rid) {
  return static_cast<uint8_t>((rid >> 8) & 0x1F);
}

/* -------------------------------- *
 *          Float Registers         *
 * -------------------------------- */
constexpr uint32_t V850_REG_FLOAT_EFG = 0xFFF10000;
constexpr uint32_t V850_REG_FLOAT_ECT = 0xFFF10004;
// Floating-point status register (RH850 FPSR). CC(7:0) bits live in bits
// 31..24 and are referenced by CMPF.S / CMOVF.S / TRFSR. Use a distinct
// small ID so it appears as a proper BN register in GetAllRegisters.
constexpr uint32_t V850_REG_FPSR = 0xFFF10008;
constexpr uint8_t FPSR = 64;  // pseudo-register slot for FPSR CC bits
}  // namespace V850::Registers

#endif  // BINARYNINJA_API_V850_REGISTERS_H
