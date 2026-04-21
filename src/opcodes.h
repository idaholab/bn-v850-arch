// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#ifndef BINARYNINJA_API_V850_OPCODES_H
#define BINARYNINJA_API_V850_OPCODES_H

#include <cstdint>

/*
 * V850 has 16-bit and 32-bit instructions.
 *
 * 16-bit instructions include binary operation, control, and conditional branch
 *      (formats I-IV)
 * 32-bit instructions include load/store, jump, and instructions that handle
 * 16-bit immediate data (formats V-X)
 *
 * ----------------------------------- *
 * 10 different instruction formats    *
 *                                     *
 * Note: RFU = Reserved for Future Use *
 * ----------------------------------- *
 * I: reg-reg
 *      15   11 10     5 4     0
 *      | reg2 | opcode | reg1 |
 *
 * II: imm-reg
 *      15   11 10     5 4    0
 *      | reg2 | opcode | imm |
 *
 * III: conditional branch
 *      15   11 10     7 6    4 3     0
 *      | disp | opcode | disp | cond |
 *
 * IV: 16-bit load/store
 *      15   11 10     7 6    1                  0
 *      | reg2 | opcode | disp | disp/sub-opcode |
 *
 * V: jump
 *      15   11 10     6 5   0 31   16
 *      | reg2 | opcode |    disp    |
 *
 * VI: 3-operand
 *      15   11 10     5 4    0 31  16
 *      | reg2 | opcode | reg1 | imm |
 *
 * VII: 32-bit load/store
 *      15   11 10     5 4    0 31  17                 16
 *      | reg2 | opcode | reg1 | disp | disp/sub-opcode |
 *
 * VIII: bit manipulation
 *      15         14 13  11 10     5 4    0 31   16
 *      | sub-opcode | bit# | opcode | reg1 | disp |
 *
 * IX: extended instruction format 1
 *      15   11 10     5 4               0 31 27 26        21 20  16
 *      | reg2 | opcode | reg1/regID/cond | RFU | sub-opcode | RFU |
 *
 * X: extended instruction format 2
 *      15             13 12 11 10     5 4                    0 31 27 26 21 20
 * 17   16 | RFU/sub-opcode | RFU | opcode | RFU/immediate/vector | RFU |
 * sub-opcode | RFU | 0 |
 *
 * XI: extended instruction format 3
 *      15   11 10     5 4    0 31  27 26        21 20 18      17        16
 *      | reg2 | opcode | reg1 | reg3 | sub-opcode | RFU | sub-opcode | 0 |
 *
 * XII: extended instruction format 4
 *      15   11 10     5 4        0 31  27 26        23 22       18      17 16
 *      | reg2 | opcode | imm(low) | reg3 | sub-opcode | imm(high) | sub-opcode
 * | 0 |
 *
 *
 * XIII: Stack manipulation instruction 1
 *      15  11 10     6 5   1 0   21 20              16
 *      | RFU | opcode | imm | list | reg2/sub-opcode |
 *
 * ----------------------------------------------------------- *
 * Additional notes:                                           *
 *      reg1 (source)                                          *
 *      reg2 (generally destination; some also used as source) *
 * ----------------------------------------------------------- *
 */

namespace V850 {
static uint64_t mask_and_shift_right(uint64_t opcode, uint64_t mask,
                                     uint8_t shift);
static uint64_t mask_and_shift_left(uint64_t opcode, uint64_t mask,
                                    uint8_t shift);
uint8_t Extract4BitOpcode(uint16_t opcode);
uint8_t Extract5BitOpcode(uint16_t opcode);
uint8_t Extract6BitOpcode(uint16_t opcode);
uint8_t Extract7BitOpcode(uint16_t opcode);
uint8_t ExtractTypeIIIBranchCond(uint16_t opcode);
uint8_t ExtractTypeIVSubop(uint16_t opcode);
int32_t ExtractTypeVDisp22(uint32_t opcode);
uint8_t ExtractTypeVIOpcode(uint16_t opcode);
uint16_t ExtractTypeVIImm16(uint32_t opcode);
uint32_t ExtractTypeVIImm32(uint64_t opcode);
uint8_t ExtractTypeVIIOpcode(uint16_t opcode);
uint8_t ExtractTypeVIISubop(uint16_t opcode);
uint16_t ExtractTypeVIIDisp16(uint32_t opcode);
uint16_t ExtractTypeVIIDisp16B(uint32_t opcode);
uint16_t ExtractTypeVIIDisp16H(uint32_t opcode);
uint16_t ExtractTypeVIIDisp16W(uint32_t opcode);
uint8_t ExtractTypeVIIISubop(uint16_t opcode);
uint8_t ExtractTypeVIIIBitnum(uint16_t opcode);
uint8_t ExtractTypeXICond(uint32_t opcode);
uint8_t ExtractTypeXIIImm5(uint16_t opcode);
uint16_t ExtractTypeXIIImm9(uint32_t opcode);
uint8_t ExtractTypeXIIIImm5(uint32_t opcode);
uint8_t ExtractReg1OpcodeField(uint16_t opcode);
uint8_t ExtractReg2OpcodeField(uint16_t opcode);
uint8_t ExtractReg3OpcodeField(uint32_t opcode);
uint8_t ExtractDisp7OpcodeField(uint16_t opcode);
uint8_t Extract6BitDisp8OpcodeField(uint16_t opcode);
uint8_t Extract7BitDisp8OpcodeField(uint16_t opcode);
uint8_t ExtractImm5OpcodeField(uint16_t opcode);
uint8_t ExtractImm6OpcodeField(uint16_t opcode);
}  // namespace V850

namespace V850::OpcodeFields {
// Masks to check which category of opcode is being used
constexpr uint16_t OPCODE_BIT_1 = 0b0000010000000000;
constexpr uint16_t OPCODE_BIT_2 = 0b0000001000000000;
constexpr uint16_t OPCODE_BIT_3 = 0b0000000100000000;
constexpr uint16_t OPCODE_BIT_4 = 0b0000000010000000;
constexpr uint16_t OPCODE_BIT_5 = 0b0000000001000000;
constexpr uint16_t OPCODE_BIT_6 = 0b0000000000100000;

// Masks to get entire opcode
constexpr uint16_t OPCODE_2_BITS = 0b0000011000000000;
constexpr uint16_t OPCODE_4_BITS = 0b0000011110000000;
constexpr uint16_t OPCODE_5_BITS = 0b0000011111000000;
constexpr uint16_t OPCODE_6_BITS = 0b0000011111100000;
constexpr uint16_t OPCODE_7_BITS = 0b0000011111110000;

// Masks to get register numbers from the opcode
constexpr uint16_t MASK_REG1 = 0b0000000000011111;
constexpr uint16_t MASK_REG2 = 0b1111100000000000;

// Shift counts
constexpr uint16_t SHIFT_REG2 = 11;
constexpr uint16_t SHIFT_2BIT_OPCODE = 9;
constexpr uint16_t SHIFT_4BIT_OPCODE = 7;
constexpr uint16_t SHIFT_5BIT_OPCODE = 6;
constexpr uint16_t SHIFT_6BIT_OPCODE = 5;
constexpr uint16_t SHIFT_7BIT_OPCODE = 4;

// Format-specific masks & shifts
/* ---------------------------- *
 *    FORMAT II: Field Masks    *
 * ---------------------------- */
constexpr uint16_t MASK_II_OPCODE = 0b0000011111100000;
constexpr uint16_t MASK_II_OPCODE_5BIT = 0b0000011111000000;
constexpr uint16_t MASK_II_IMM5 = 0b0000000000011111;
constexpr uint16_t MASK_II_IMM6 = 0b0000000000111111;

constexpr uint8_t SHIFT_II_OPCODE = 5;
constexpr uint8_t SHIFT_II_OPCODE_5BIT = 5;

/* ----------------------------- *
 *    FORMAT III: Field Masks    *
 * ----------------------------- */
constexpr uint16_t MASK_III_DISP_H = 0b1111100000000000;
constexpr uint16_t MASK_III_OPCODE = 0b0000011110000000;
constexpr uint16_t MASK_III_DISP_L = 0b0000000001110000;
constexpr uint16_t MASK_III_COND = 0b0000000000001111;

// Displacement is 9-bit, sign extended to 16-bit
// [ 5-bit DISP_H | 3-bit DISP_L | single zeroed bit ]
constexpr uint8_t SHIFT_III_DISP_H =
    7;  // shift to make byte 7 // Shift amount needed to obtain full
        // displacement, with [ disp_H | disp_L | zero ] concatenated
constexpr uint8_t SHIFT_III_OPCODE = 7;
constexpr uint8_t SHIFT_III_DISP_L = 3;  // shift to make byte 3

/* ---------------------------- *
 *    FORMAT IV: Field Masks    *
 * ---------------------------- */
constexpr uint16_t MASK_IV_OPCODE = 0b0000011110000000;
constexpr uint16_t MASK_IV_7BIT_DISP = 0b0000000001111111;
constexpr uint16_t MASK_IV_6BIT_DISP = 0b0000000001111110;
constexpr uint16_t MASK_IV_4BIT_DISP = 0b0000000000001111;
constexpr uint16_t MASK_IV_SUBOP = 0b0000000000000001;

constexpr uint8_t SHIFT_IV_OPCODE = 7;
constexpr uint8_t SHIFT_IV_DISP = 1;

/* --------------------------- *
 *    FORMAT V: Field Masks    *
 *    ** Note: is 32-bit       *
 * --------------------------- */
// Halfword 1
constexpr uint16_t MASK_V_OPCODE = 0b0000011111000000;
constexpr uint16_t MASK_V_DISP_H =
    0b0000000000111111;  // note: split between bytes; concatenate as | DISP_H |
                         // DISP_L |

constexpr uint8_t SHIFT_V_OPCODE = 6;
constexpr uint8_t SHIFT_V_DISP_H = 16;

// Halfword 2
constexpr uint16_t MASK_V_DISP_L = 0b1111111111111110;
constexpr uint16_t MASK_V_SUBOP_BIT_16 =
    0b0000000000000001;  // note: field will always be 0 for FORMAT V

constexpr uint8_t SHIFT_V_DISP_L = 1;

/* ---------------------------- *
 *    FORMAT VI: Field Masks    *
 *    ** Note: is 32-bit        *
 * ---------------------------- */
// Halfword 1
constexpr uint16_t MASK_VI_OPCODE = 0b0000011111100000;

constexpr uint8_t SHIFT_VI_OPCODE = 5;

// Halfword 2
constexpr uint16_t MASK_VI_IMM = 0b1111111111111110;
constexpr uint16_t MASK_VI_SUBOP_0 = 0b0000000000000001;

constexpr uint8_t SHIFT_VI_IMM = 1;

/* ----------------------------- *
 *    FORMAT VII: Field Masks    *
 *    ** Note: is 32-bit         *
 * ----------------------------- */
// Halfword 1
constexpr uint16_t MASK_VII_OPCODE = 0b0000011111100000;
constexpr uint16_t MASK_VII_DISP0 =
    0b0000000000100000;  // Specifically for ld.bu

constexpr uint8_t SHIFT_VII_OPCODE = 5;
constexpr uint8_t SHIFT_VII_DISP0 = 5;  // For ld.bu

// Halfword 2
constexpr uint16_t MASK_VII_DISP_B = 0b1111111111111111;
constexpr uint16_t MASK_VII_DISP_H = 0b1111111111111110;
constexpr uint16_t MASK_VII_DISP_W = 0b1111111111111110;
constexpr uint16_t MASK_VII_DISP = 0b1111111111111110;  // For ld.bu
constexpr uint16_t MASK_VII_SUBOP = 0b0000000000000001;

/* ------------------------------ *
 *    FORMAT VIII: Field Masks    *
 *    ** Note: is 32-bit          *
 * ------------------------------ */
// Halfword 1
constexpr uint16_t MASK_VIII_SUBOP = 0b1100000000000000;
constexpr uint16_t MASK_VIII_BITNUM = 0b0011100000000000;
constexpr uint16_t MASK_VIII_OPCODE = 0b0000011111100000;

constexpr uint8_t SHIFT_VIII_SUBOP = 14;
constexpr uint8_t SHIFT_VIII_BITNUM = 11;
constexpr uint8_t SHIFT_VIII_OPCODE = 5;

// Halfword 2
constexpr uint8_t MASK_VIII_DISP = 0xFF;  // the entire byte

/* ---------------------------- *
 *    FORMAT IX: Field Masks    *
 *    ** Note: is 32-bit        *
 * ---------------------------- */
// Halfword 1
constexpr uint16_t MASK_IX_OPCODE = 0b0000011111100000;
constexpr uint16_t MASK_IX_COND = 0b0000000000001111;

constexpr uint8_t SHIFT_IX_OPCODE = 5;

// Halfword 2
constexpr uint16_t MASK_IX_SUBOP = 0b0000011111100000;
constexpr uint8_t SHIFT_IX_SUBOP = 5;

constexpr uint8_t MASK_SUBOP_BITS_17_18 = 0b110;
constexpr uint8_t SHIFT_SUBOP_BITS_17_18 = 1;

constexpr uint8_t MASK_SUBOP_BIT_17 = 0b10;
constexpr uint8_t SHIFT_SUBOP_BIT_17 = 1;

// BINS (bitfield insert) field masks (G3MH p.162). Share Format IX slot.
constexpr uint16_t MASK_IX_SUBOP_BINS = 0b0000011111100000;
constexpr uint8_t SHIFT_IX_SUBOP_BINS = 5;
constexpr uint16_t MASK_IX_BINS_MMMM = 0b1111000000000000;
constexpr uint8_t SHIFT_IX_BINS_MMMM = 12;
constexpr uint16_t MASK_IX_BINS_K = 0b0000100000000000;
constexpr uint8_t SHIFT_IX_BINS_K = 11;
constexpr uint16_t MASK_IX_BINS_LLL = 0b0000000000001110;
constexpr uint8_t SHIFT_IX_BINS_LLL = 1;

/* --------------------------- *
 *    FORMAT X: Field Masks    *
 *    ** Note: is 32-bit       *
 * --------------------------- */
// Halfword 1
constexpr uint16_t MASK_X_SUBOP1 = 0b1110000000000000;
constexpr uint16_t MASK_X_OPCODE = 0b0000011111100000;
constexpr uint16_t MASK_X_IMM_VECTOR = 0b0000000000011111;
constexpr uint16_t MASK_X_SUBOP_EI_DI = 0b1000000000000000;

constexpr uint8_t SHIFT_X_SUBOP1 = 13;
constexpr uint8_t SHIFT_X_OPCODE = 5;
constexpr uint8_t SHIFT_X_SUBOP_EI_DI = 15;
// Halfword 2
constexpr uint16_t MASK_X_SUBOP2 = 0b0000011111100000;

constexpr uint8_t SHIFT_X_SUBOP2 = 5;

/* ---------------------------- *
 *    FORMAT XI: Field Masks    *
 *    ** Note: is 32-bit        *
 * ---------------------------- */
constexpr uint16_t MASK_XI_SUBOP = 0b0000000000000011;
constexpr uint16_t MASK_XI_REG3 = 0b1111100000000000;
constexpr uint16_t MASK_XI_COND = 0b0000000000011110;

constexpr uint8_t SHIFT_XI_REG3 = 11;
constexpr uint8_t SHIFT_XI_COND = 1;

/* ----------------------------- *
 *    FORMAT XII: Field Masks    *
 * ----------------------------- */
// Halfword 1
constexpr uint16_t MASK_XII_IMM9_LOW_OR_IMM5 = 0b0000000000011111;

// Halfword 2
constexpr uint16_t MASK_XII_OPCODE = 0b0000011111000000;
constexpr uint16_t MASK_XII_IMM9_HI = 0b0000000000111100;
constexpr uint16_t MASK_XII_SUBOP_MUL_MULU = 0b0000000000000011;
constexpr uint16_t MASK_XII_SUBOP_BSW_BSH_HSW = 0b0000000000000111;

constexpr uint8_t SHIFT_XII_OPCODE = 6;
constexpr uint8_t SHIFT_LEFT_XII_IMM9_HI = 3;

/* ------------------------------ *
 *    FORMAT XIII: Field Masks    *
 * ------------------------------ */
// Halfword 1
constexpr uint16_t MASK_XIII_IMM5 = 0b0000000000111110;
constexpr uint8_t SHIFT_XIII_IMM5 = 1;

/* Set bits correspond to general purpose registers (r20-r31) as follows:
 *
 *   31    20    29    28    27    26    25    24    23    22    21   20  1   0
 * | r24 | r25 | r26 | r27 | r20 | r21 | r22 | r23 | r28 | r29 | r31 | ... | r30
 * |
 */

// In halfword 1
constexpr uint16_t MASK_XIII_LIST12_WORD1 = 0b0000000000000001;
constexpr uint16_t MASK_XIII_BIT_R30 = 0b0000000000000001;

// Halfword 2
constexpr uint16_t MASK_XIII_LIST12_WORD2 = 0b1111111111100000;
constexpr uint16_t MASK_XIII_BIT_R24 = 0b1000000000000000;
constexpr uint16_t MASK_XIII_BIT_R25 = 0b0100000000000000;
constexpr uint16_t MASK_XIII_BIT_R26 = 0b0010000000000000;
constexpr uint16_t MASK_XIII_BIT_R27 = 0b0001000000000000;
constexpr uint16_t MASK_XIII_BIT_R20 = 0b0000100000000000;
constexpr uint16_t MASK_XIII_BIT_R21 = 0b0000010000000000;
constexpr uint16_t MASK_XIII_BIT_R22 = 0b0000001000000000;
constexpr uint16_t MASK_XIII_BIT_R23 = 0b0000000100000000;
constexpr uint16_t MASK_XIII_BIT_R28 = 0b0000000010000000;
constexpr uint16_t MASK_XIII_BIT_R29 = 0b0000000001000000;
constexpr uint16_t MASK_XIII_BIT_R31 = 0b0000000000100000;

/* ------------------------------ *
 *    FORMAT XIV: Field Masks     *
 *    ** Note: is 48-bit          *
 *                                *
 * Layout (per Ghidra SLEIGH      *
 * v850_load_store.sinc / v850e3  *
 * .sinc):                        *
 *                                *
 *  HW1 bits 0..15:               *
 *    [15..11]=reg2(=R1115, fixed *
 *              to 1 for XIV)     *
 *    [10..5] =6-bit op (0b111100 *
 *              or 0b111101)      *
 *    [4..0]  =reg1 (base/R0004)  *
 *                                *
 *  HW2 bits 16..31:              *
 *    [31..27]=R2731 (src/dst)    *
 *    [26..20]=op2026 (disp low   *
 *              7 bits, for .b /  *
 *              .bu / .hu) — or   *
 *    [26..21]=op2126 (disp low   *
 *              6 bits, for .h /  *
 *              .w / .hu? / .dw), *
 *              with op1620 low   *
 *              bit feeding the   *
 *              halfword/word     *
 *              alignment zero    *
 *    [19..16]=op1619 sub (.b,    *
 *              .bu, .hu, .st.b)  *
 *    [20..16]=op1620 sub (.h,    *
 *              .w, .st.h, .st.w, *
 *              .ld.dw, .st.dw)   *
 *                                *
 *  HW3 bits 32..47:              *
 *    [47..32]=s3247 (signed 16-  *
 *              bit upper disp)   *
 *                                *
 *  disp23 reconstruction:        *
 *    .b / .bu / .hu / .st.b:     *
 *      disp23 =                  *
 *        (s3247 << 7) | op2026   *
 *    .h / .w / .st.h / .st.w /   *
 *    .ld.dw / .st.dw:            *
 *      disp23 =                  *
 *        (s3247 << 7) |          *
 *        (op2126 << 1)           *
 *      (low bit always 0 due     *
 *      to alignment)             *
 * ------------------------------ */

// HW1: 6-bit opcode lives in bits 5..10 as usual; distinguished from
// Format VII ld.bu by (a) reg2==1 AND (b) op0515 matching one of the
// two 11-bit HW1 patterns below.
constexpr uint16_t MASK_XIV_OP0515 = 0b0000111111100000;  // bits 5..15
constexpr uint8_t SHIFT_XIV_OP0515 = 5;

// HW2 sub-selectors. 4-bit field in bits 16..19 for byte-granular
// variants (op1619); 5-bit field in bits 16..20 for halfword/word
// variants (op1620). Opcode *values* for these sub-selectors live in
// the V850::Opcodes namespace (see bottom of this file) alongside the
// other OP_*/SUBOP_* constants.
constexpr uint16_t MASK_XIV_OP1619 = 0b0000000000001111;  // HW2 bits 16..19
constexpr uint16_t MASK_XIV_OP1620 = 0b0000000000011111;  // HW2 bits 16..20

// HW2 field extractors. All masks below apply to the 16-bit HW2 value
// (obtained via `(opcode >> 16) & 0xFFFF`), with bit 0 of that halfword
// corresponding to bit 16 of the full 48-bit instruction.
//
//   full-instr bit 16..19 == HW2 bit 0..3   (op1619)
//   full-instr bit 16..20 == HW2 bit 0..4   (op1620)
//   full-instr bit 20..26 == HW2 bit 4..10  (op2026)
//   full-instr bit 21..26 == HW2 bit 5..10  (op2126)
//   full-instr bit 27..31 == HW2 bit 11..15 (R2731)
constexpr uint16_t MASK_XIV_OP2026 = 0b0000011111110000;  // HW2 bits 4..10
constexpr uint8_t SHIFT_XIV_OP2026 = 4;
constexpr uint16_t MASK_XIV_OP2126 = 0b0000011111100000;  // HW2 bits 5..10
constexpr uint8_t SHIFT_XIV_OP2126 = 5;

constexpr uint16_t MASK_XIV_R2731 = 0b1111100000000000;  // HW2 bits 11..15
constexpr uint8_t SHIFT_XIV_R2731 = 11;

}  // namespace V850::OpcodeFields

namespace V850::Opcodes {
/* ----------------------- */
/* FORMAT I: 6-bit opcodes */
/* ----------------------- */
constexpr uint8_t OP_I_6BIT_MOV_OR_NOP = 0b000000;
constexpr uint8_t OP_I_6BIT_NOT = 0b000001;
constexpr uint8_t OP_I_6BIT_SWITCH_DBTRAP_DIVH = 0b000010;
constexpr uint8_t OP_I_6BIT_JMP_OR_IV_SLD_HU_SLD_BU = 0b000011;
constexpr uint8_t OP_I_6BIT_SATSUBR_OR_ZXB = 0b000100;
constexpr uint8_t OP_I_6BIT_SATSUB_OR_SXB = 0b000101;
constexpr uint8_t OP_I_6BIT_SATADD_OR_ZXH = 0b000110;
constexpr uint8_t OP_I_6BIT_MULH_OR_SXH = 0b000111;
constexpr uint8_t OP_I_6BIT_OR = 0b001000;
constexpr uint8_t OP_I_6BIT_XOR = 0b001001;
constexpr uint8_t OP_I_6BIT_AND = 0b001010;
constexpr uint8_t OP_I_6BIT_TST = 0b001011;
constexpr uint8_t OP_I_6BIT_SUBR = 0b001100;
constexpr uint8_t OP_I_6BIT_SUB = 0b001101;
constexpr uint8_t OP_I_6BIT_ADD = 0b001110;
constexpr uint8_t OP_I_6BIT_CMP = 0b001111;
// Dbtrap opcode defined by this *exact* 16-bit value
constexpr uint16_t EXACT_OP_I_DBTRAP = 0b1111100001000000;

// V850E3/G3MH debug Format-I 16-bit exact encodings (Ghidra SLEIGH
// v850e3.sinc):
//   DBCP     = 0xE840
//   DBHVTRAP = 0xE040
// Both share the op0510 = 0b000010 field with DBTRAP / SWITCH / DIVH and
// are distinguished from DBTRAP (0xF840) only by the reg2 (bits[15:11])
// and reg1 (bits[4:0]) fields.
constexpr uint16_t EXACT_OP_I_DBCP = 0xE840;
constexpr uint16_t EXACT_OP_I_DBHVTRAP = 0xE040;

// V850E3/G3MH Format-X 32-bit exact HW2 encodings (Ghidra SLEIGH
// v850e3.sinc). HW1 is 0x87E0 (reg2 bit set) for TLB* / EI, 0x07E0 for
// DI / EST. All share op0510 = 0b111111.
constexpr uint16_t EXACT_OP_X_HW1_TLB_EI =
    0x87E0;  // HW1 for TLB*/EI (reg2 bit 15 set)
constexpr uint16_t EXACT_OP_X_HW2_EI_DI = 0x0160;
constexpr uint16_t EXACT_OP_X_HW2_TLBAI = 0x8960;
constexpr uint16_t EXACT_OP_X_HW2_TLBR = 0xE960;
constexpr uint16_t EXACT_OP_X_HW2_TLBS = 0xC160;
constexpr uint16_t EXACT_OP_X_HW2_TLBVI = 0x8160;
constexpr uint16_t EXACT_OP_X_HW2_TLBW = 0xE160;
constexpr uint16_t EXACT_OP_X_HW2_EST = 0x0132;

// SYSCALL / DBPUSH / DBTAG all share HW2 low-11 = 0x160 (same as EI/DI)
// and op0510 = 0b111111, but are distinguished by HW1 reg2 (bits[15:11]):
//   SYSCALL vector8 : reg2 = 0b11010  (op0515 = 0x6BF)
//   DBPUSH  R,R    : reg2 = 0b01011  (op0515 = 0x2FF)
//   DBTAG   imm10  : reg2 = 0b11001  (op0515 = 0x67F)
constexpr uint8_t REG2_FIELD_SYSCALL = 0b11010;
constexpr uint8_t REG2_FIELD_DBPUSH = 0b01011;
constexpr uint8_t REG2_FIELD_DBTAG = 0b11001;
constexpr uint8_t REG2_FIELD_SNOOZE =
    0b00001;  // distinguishes SNOOZE from HALT (0b00000)
constexpr uint8_t REG2_FIELD_PUSHSP = 0b01000;    // Format XI PUSHSP rh-rt
constexpr uint8_t REG2_FIELD_POPSP = 0b01100;     // Format XI POPSP rh-rt
constexpr uint8_t REG2_FIELD_JARL_IND = 0b11000;  // Format XI JARL [reg1], reg3
constexpr uint16_t MASK_X_HW2_LOW11 = 0x07FF;

// Synchronize-family instructions: Format I, exact 16-bit encodings.
// Per G3MH Software Manual Section 7.2, pp. 287-290 (SYNCE/SYNCI/SYNCM/SYNCP).
// All four are defined as pipeline/memory/instruction barriers; SYNCE is
// explicitly handled as NOP on this CPU (p. 287).
constexpr uint16_t EXACT_OP_I_SYNCI = 0b0000000000011100;  // 0x001C
constexpr uint16_t EXACT_OP_I_SYNCE = 0b0000000000011101;  // 0x001D
constexpr uint16_t EXACT_OP_I_SYNCM = 0b0000000000011110;  // 0x001E
constexpr uint16_t EXACT_OP_I_SYNCP = 0b0000000000011111;  // 0x001F

/* ------------------------ */
/* FORMAT II: 6-bit opcodes */
/* ------------------------ */
constexpr uint8_t FORMAT_II_CATEGORY = 0b010;
constexpr uint16_t FORMAT_II_MASK = 0b0000011100000000;
constexpr uint8_t FORMAT_II_SHIFT = 8;

constexpr uint8_t OP_II_5BIT_MOV_OR_CALLT = 0b01000;
constexpr uint8_t OP_II_6BIT_SATADD = 0b010001;
constexpr uint8_t OP_II_6BIT_ADD = 0b010010;
constexpr uint8_t OP_II_6BIT_CMP = 0b010011;
constexpr uint8_t OP_II_6BIT_SHR = 0b010100;
constexpr uint8_t OP_II_6BIT_SAR = 0b010101;
constexpr uint8_t OP_II_6BIT_SHL = 0b010110;
constexpr uint8_t OP_II_6BIT_MULH = 0b010111;

/* ------------------------- */
/* FORMAT III: 4-bit opcodes */
/* ------------------------- */
constexpr uint8_t FORMAT_III_CATEGORY = 0b1011;
constexpr uint16_t FORMAT_III_MASK = 0b0000011110000000;
constexpr uint8_t FORMAT_III_SHIFT = 7;

constexpr uint8_t OP_III_4BIT_BCOND =
    0b1011;  // Only instruction in the category

/* ------------------------ */
/* FORMAT IV: 4-bit opcodes */
/* ------------------------ */
constexpr uint16_t FORMAT_IV_MASK = 0b0000011110000000;
constexpr uint8_t FORMAT_IV_SHIFT = 7;

constexpr uint8_t OP_IV_4BIT_SLD_B = 0b0110;
constexpr uint8_t OP_IV_4BIT_SST_B = 0b0111;
constexpr uint8_t OP_IV_4BIT_SLD_H = 0b1000;
constexpr uint8_t OP_IV_4BIT_SST_H = 0b1001;
constexpr uint8_t OP_IV_4BIT_SLD_W_OR_SST_W =
    0b1010;  // note: look at sub-opcode/displacement to differentiate SLD.W and
             // SST.W

constexpr uint8_t OP_IV_SLD_BU = 0b0000110;
constexpr uint8_t OP_IV_SLD_HU = 0b0000111;

constexpr uint8_t SUBOP_IV_SLD_W = 0;
constexpr uint8_t SUBOP_IV_SST_W = 1;

/* ---------------------- */
/* FORMAT V: 5-bit opcode */
/* ---------------------- */
constexpr uint8_t OP_V_JARL_JR_VII_LDBU_XIII_PREPARE = 0b11110;

constexpr uint8_t SUBOP_V_JARL_JR = 0;

/* ------------------------ */
/* FORMAT VI: 6-bit opcodes */
/* ------------------------ */

// 6-bit opcodes
constexpr uint8_t OP_VI_6BIT_ADDI = 0b110000;
constexpr uint8_t OP_VI_6BIT_MOVEA_OR_MOV = 0b110001;
constexpr uint8_t OP_VI_6BIT_ORI = 0b110100;
constexpr uint8_t OP_VI_6BIT_XORI = 0b110101;
constexpr uint8_t OP_VI_6BIT_ANDI = 0b110110;
constexpr uint8_t OP_VI_6BIT_MULHI = 0b110111;

// NOTE: movhi and satsubi share same opcode as dispose
constexpr uint8_t OP_XIII_5BIT_DISPOSE = 0b11001;
constexpr uint8_t OP_VI_6BIT_MOVHI = 0b110010;
constexpr uint8_t OP_VI_6BIT_SATSUBI = 0b110011;

/* ------------------------- */
/* FORMAT VII: 6-bit opcodes */
/* ------------------------- */
constexpr uint8_t OP_VII_6BIT_LD_B = 0b111000;
constexpr uint8_t OP_VII_6BIT_LD_H_OR_LD_W = 0b111001;
constexpr uint8_t OP_VII_6BIT_ST_B = 0b111010;
constexpr uint8_t OP_VII_6BIT_ST_H_OR_ST_W = 0b111011;

constexpr uint8_t SUBOP_VII_LD_H = 0;
constexpr uint8_t SUBOP_VII_LD_W = 1;
constexpr uint8_t SUBOP_VII_ST_H = 0;
constexpr uint8_t SUBOP_VII_ST_W = 1;
constexpr uint8_t SUBOP_VII_LD_BU_HU = 1;

/* -------------------------- */
/* FORMAT VIII: 6-bit opcodes */
/* -------------------------- */
constexpr uint8_t OP_VIII_6BIT_BIT_MANIPULATION =
    0b111110;  // note: format VIII includes SET1, CLR1, NOT1, and TST11

/* These subops are used for both format VIII and format IX bit manipulation
 * instructions */
constexpr uint8_t SUBOP_SET1 = 0b00;
constexpr uint8_t SUBOP_NOT1 = 0b01;
constexpr uint8_t SUBOP_CLR1 = 0b10;
constexpr uint8_t SUBOP_TST1 = 0b11;

/* ------------------------ */
/* FORMAT IX-XII (Extended) */
/* ------------------------ */
constexpr uint8_t OP_EXT_6BIT = 0b111111;

/* Format IX - Opcode is 2nd word */
constexpr uint16_t OP_IX_SETF = 0b0000000000000000;
constexpr uint16_t OP_IX_LDSR = 0b0000000000100000;
constexpr uint16_t OP_IX_STSR = 0b0000000001000000;
constexpr uint16_t OP_IX_SHR = 0b0000000010000000;
constexpr uint16_t OP_IX_SAR = 0b0000000010100000;
constexpr uint16_t OP_IX_SHL = 0b0000000011000000;
constexpr uint16_t OP_IX_SET1 = 0b0000000011100000;
constexpr uint16_t OP_IX_NOT1 = 0b0000000011100010;
constexpr uint16_t OP_IX_CLR1 = 0b0000000011100100;
constexpr uint16_t OP_IX_TST1 = 0b0000000011100110;
constexpr uint16_t OP_IX_SASF = 0b0000001000000000;

/* Format X */
/* Operation determined by entire 2nd word of instruction */
constexpr uint16_t OP_X_TRAP = 0b0000000100000000;
constexpr uint16_t OP_X_HALT = 0b0000000100100000;
constexpr uint16_t OP_X_RETI = 0b0000000101000000;
constexpr uint16_t OP_X_EIRET =
    0b0000000101001000;  // 0x0148 — G3MH (bit 3 set)
constexpr uint16_t OP_X_FERET =
    0b0000000101001010;  // 0x014A — G3MH (bits 3+1 set)
constexpr uint16_t OP_X_CTRET = 0b0000000101000100;
constexpr uint16_t OP_X_DBRET = 0b0000000101000110;
constexpr uint16_t OP_X_DI_OR_EI = 0b0000000101100000;

constexpr uint8_t SUBOP_X_RETI = 0b00;
constexpr uint8_t SUBOP_X_CTRET = 0b10;
constexpr uint8_t SUBOP_X_DBRET = 0b11;

// The di/ei subop is bit 15, in word 1 of the instruction
constexpr uint8_t SUBOP_X_DI = 0;
constexpr uint8_t SUBOP_X_EI = 1;

/* Format XI */
// Bits 16-22 of word 2
constexpr uint8_t OP_XI_MUL_OR_MULU = 0b010001;
constexpr uint8_t OP_XI_DIVH_OR_DIVHU = 0b010100;
constexpr uint8_t OP_XI_DIV_OR_DIVU = 0b010110;
constexpr uint8_t OP_XI_CMOV = 0b011001;

// Last 2 bits of word 2
constexpr uint8_t SUBOP_XI_MUL = 0b00;
constexpr uint8_t SUBOP_XI_MULU = 0b10;
constexpr uint8_t SUBOP_XI_DIVH = 0b00;
constexpr uint8_t SUBOP_XI_DIFHU = 0b10;
constexpr uint8_t SUBOP_XI_DIV = 0b00;
constexpr uint8_t SUBOP_XI_DIVU = 0b10;

constexpr uint8_t SUBOP_XI_CMOV =
    0;  // Bit 16 of word 2; idk if needed currently

// 11-bit word2[10:0] selectors for Format XI instructions
constexpr uint16_t SUBOP_XI_PUSHSP_POPSP_JARL =
    0b00101100000;  // shared by PUSHSP/POPSP/jarl [reg1]
constexpr uint16_t SUBOP_XI_CAXI = 0b00011101110;  // CAXI [reg1], reg2, reg3
constexpr uint16_t MASK_XI_SCH = 0b11111111000;  // upper-8 mask for SCH family
constexpr uint16_t SUBOP_XI_SCH =
    0b01101100000;  // SCH0L/SCH0R/SCH1L/SCH1R base

/* Format XII */
// Bits 22-26, word2
constexpr uint8_t OP_XII_MUL_OR_MULU = 0b01001;
constexpr uint8_t OP_XII_CMOV = 0b01100;
constexpr uint8_t OP_XII_BSW_BSH_HSW = 0b01101;

// Bits 16-17, word2
constexpr uint8_t SUBOP_XII_MUL = 0b00;
constexpr uint8_t SUBOP_XII_MULU = 0b10;

// Bits 16-18, word2
constexpr uint8_t SUBOP_XII_BSW = 0b000;
constexpr uint8_t SUBOP_XII_BSH = 0b010;
constexpr uint8_t SUBOP_XII_HSW = 0b100;
constexpr uint8_t SUBOP_XII_HSH = 0b110;

/* Format IX: BINS (bitfield insert) -- sub-opcode constants */
// Sub-opcode values live in bits 5..10 of word 2 (G3MH p.162).
constexpr uint8_t SUBOP_IX_BINS_HI = 0b001001;   // msb >= 16, lsb >= 16
constexpr uint8_t SUBOP_IX_BINS_MID = 0b001011;  // msb >= 16, lsb <  16
constexpr uint8_t SUBOP_IX_BINS_LO = 0b001101;   // msb <  16, lsb <  16

/* Format XIII */
// 5-bit opcode is bits 6-10 of word 1
// Format XIII opcode bits[6..10] of hw1. DISPOSE uses forms (1) and (2):
//   (1) DISPOSE imm5, list12            — plain stack-frame delete
//   (2) DISPOSE imm5, list12, [reg1]    — delete + jump to GR[reg1]
// Both share OP_XIII_DISPOSE; form (2) is distinguished via reg1_field != 0.
// PREPARE forms are keyed off SUBOP_XIII_PREPARE_{001,011} below.
constexpr uint8_t OP_XIII_DISPOSE = 0b11001;

// Bits 16-18 of instruction halfword 2
constexpr uint8_t SUBOP_XIII_PREPARE_001 = 0b001;
constexpr uint8_t SUBOP_XIII_PREPARE_011 = 0b011;

// Halfword 2, subops for different forms of prepare and dispose respectively
constexpr uint16_t MASK_XIII_SUBOP_PREPARE = 0b0000000000000111;
constexpr uint16_t MASK_XIII_SUBOP_DISPOSE = 0b0000000000011111;

// Note: instruction size for prepare varies depending on subop; can be 32, 48,
// or 64 bit
constexpr uint16_t MASK_XIII_PREPARE_FORMAT = 0b0000000000011000;
constexpr uint8_t SHIFT_XIII_PREPARE_FORMAT = 3;

// Prepare has multiple options for what prepare loads to the element pointer
constexpr uint8_t PREPARE_LOAD_SP = 0b00;
constexpr uint8_t PREPARE_LOAD_SIGN_EXTENDED_IMM16 = 0b01;
constexpr uint8_t PREPARE_LOAD_LSL_IMM16 = 0b10;
constexpr uint8_t PREPARE_LOAD_IMM32 = 0b11;

/* ------------------------- */
/* FORMAT XIV: 48-bit disp23 load/store (V850E3 / RH850 G3MH).
 *
 * Field masks & extractors live in V850::OpcodeFields. Values below
 * select which XIV instruction a given (op0515, op1619/op1620) pair
 * encodes. See opcodes.h Format XIV comment block and Ghidra SLEIGH
 * v850_load_store.sinc / v850e3.sinc for the source of truth. */
/* ------------------------- */

// 6-bit opcode values (bits 5..10) — but note that Format XIV is actually
// distinguished by the full 11-bit op0515 field, because reg2 (bits 11..15)
// is fixed to 1. We compare against the full op0515 value in the decoder.
constexpr uint16_t OP_XIV_6BIT_GROUP_A = 0x03C;  // ld.b/h/w, st.b/w
constexpr uint16_t OP_XIV_6BIT_GROUP_B = 0x03D;  // ld.bu/hu, st.h, ld.dw/st.dw

// op1619 values (byte-granular / hu variants):
//   0x3C group -> ld.b=5, st.b=0xD
//   0x3D group -> ld.bu=5, ld.hu=7
constexpr uint8_t SUBOP_XIV_LDB = 0x5;
constexpr uint8_t SUBOP_XIV_STB = 0xD;
constexpr uint8_t SUBOP_XIV_LDBU = 0x5;
constexpr uint8_t SUBOP_XIV_LDHU = 0x7;

// op1620 values (aligned halfword / word / doubleword variants):
//   0x3C group -> ld.h=7, ld.w=9, st.w=0xF
//   0x3D group -> st.h=0xD, ld.dw=9, st.dw=0xF
constexpr uint8_t SUBOP_XIV_LDH = 0x07;
constexpr uint8_t SUBOP_XIV_LDW = 0x09;
constexpr uint8_t SUBOP_XIV_STW = 0x0F;
constexpr uint8_t SUBOP_XIV_STH = 0x0D;
constexpr uint8_t SUBOP_XIV_LDDW = 0x09;
constexpr uint8_t SUBOP_XIV_STDW = 0x0F;

/* ------------------------- */
/* V850E3 post-increment / pre-decrement LD/ST (32-bit Format XI-ish).
 *
 * Encoding (see Ghidra SLEIGH v850e3.sinc lines 302..382):
 *   HW1 bits 15..5 (op0515) fix low 6 bits to 0b111111 (== OP_EXT_6BIT)
 *       and use bits 15..11 (reg2 field) to encode the direction:
 *         reg2 = 2 (0x0BF) -> post-increment, signed-load / store
 *         reg2 = 3 (0x0FF) -> post-increment, unsigned-load
 *         reg2 = 4 (0x13F) -> pre-decrement,  signed-load / store
 *         reg2 = 5 (0x17F) -> pre-decrement,  unsigned-load
 *   HW2 bits 26..16 (word2 low 11 bits) select the access width/op:
 *         0x370 = ld.b / ld.bu   0x372 = st.b
 *         0x374 = ld.h / ld.hu   0x376 = st.h
 *         0x378 = ld.w           0x37A = st.w
 *   HW1 bits 0..4  = reg1 (base / pointer register — also the writeback
 * target). HW2 bits 31..27 = reg3 (load destination / store source).
 *
 * After the memory access the base register reg1 is updated:
 *   post-inc: reg1 <- reg1 + access_size
 *   pre-dec : reg1 <- reg1 - access_size
 * SLEIGH models this as a post-operation write even for "pre-decrement"
 * (the effective address is the original reg1, NOT reg1-size). This matches
 * the V850E3 pipeline semantics used by Renesas.
 */
/* ------------------------- */

// reg2-field direction selectors (HW1 bits 15..11).
constexpr uint8_t REG2_PIPD_POSTINC_SIGNED =
    0b00010;  // 2: post-inc (b/h/w/st.*)
constexpr uint8_t REG2_PIPD_POSTINC_UNSIGNED = 0b00011;  // 3: post-inc (bu/hu)
constexpr uint8_t REG2_PIPD_PREDEC_SIGNED = 0b00100;  // 4: pre-dec (b/h/w/st.*)
constexpr uint8_t REG2_PIPD_PREDEC_UNSIGNED = 0b00101;  // 5: pre-dec  (bu/hu)

// HW2 low-11-bit (op1626) access-width / opcode selectors.
constexpr uint16_t SUBOP_PIPD_LDB_LDBU =
    0x370;  // ld.b or ld.bu (reg2 disambig)
constexpr uint16_t SUBOP_PIPD_STB = 0x372;
constexpr uint16_t SUBOP_PIPD_LDH_LDHU =
    0x374;  // ld.h or ld.hu (reg2 disambig)
constexpr uint16_t SUBOP_PIPD_STH = 0x376;
constexpr uint16_t SUBOP_PIPD_LDW = 0x378;
constexpr uint16_t SUBOP_PIPD_STW = 0x37A;

/* ------------------------------------------------------------------
 * V850E3 / RH850 G3MH extensions: ADF / SBF / ROTL / LOOP / CACHE / PREF
 *
 * ADF   cccc, reg1, reg2, reg3  Format XI  op2126=0x1D, op1616=0
 * SBF   cccc, reg1, reg2, reg3  Format XI  op2126=0x1C, op1616=0
 * ROTL  reg1,reg2,reg3 / imm5,reg2,reg3   op1626=0x0C6 / 0x0C4
 * LOOP  reg1, disp16  op0515=0x037 & reg2=0 ; op1616=1 (hw1 looks like MULHI)
 * CACHE cacheop, reg1  op0515=0x3F, op1315=0x7 (reg2 bits[15:13]=111);
 *                      hw2 op1626=0x160; cacheop=(op1112<<5)|op2731
 * PREF  prefop, reg1   op0515=0x6FF (reg2=0b11011); hw2 op1626=0x160;
 *                      prefop = op2731
 * ------------------------------------------------------------------ */

// op2126 values (hw2 bits 5..10) for Format XI ADF / SBF.
constexpr uint8_t SUBOP_XI_SBF = 0b011100;  // 0x1C
constexpr uint8_t SUBOP_XI_ADF = 0b011101;  // 0x1D

// Full low-11-bit hw2 sub-opcodes for ROTL (op1626 covers hw2[10:0]).
constexpr uint16_t SUBOP_XI_ROTL_REG = 0x0C6;  // reg1/reg2/reg3 form
constexpr uint16_t SUBOP_XI_ROTL_IMM = 0x0C4;  // imm5/reg2/reg3 form

// LOOP: full 32-bit encoding is distinguished from MULHI by reg2==0
// and hw2 bit 0 (op1616) == 1. op1731 in hw2 bits 1..15 is the
// unsigned 15-bit distance; target = addr - (op1731 << 1).
constexpr uint16_t MASK_LOOP_BIT16 = 0b0000000000000001;
constexpr uint16_t MASK_LOOP_DISP = 0b1111111111111110;  // hw2 bits 1..15
constexpr uint8_t SHIFT_LOOP_DISP = 1;

// CACHE / PREF share op1626 = 0x160 (hw2 bits [10:0]). op2731 in hw2
// bits [15:11] carries the low 5 bits of cacheop (for CACHE) or prefop
// (for PREF). Distinguished by hw1 reg2 field:
//   cache  hw1 reg2 bits [4:2] (=op1315) = 0b111; bits [1:0] (=op1112)
//          form the high 2 bits of cacheop. Full cacheop =
//          (op1112 << 5) | op2731.
//   pref   hw1 reg2 = 0b11011 (fixed); prefop = op2731.
constexpr uint16_t SUBOP_CACHE_PREF_HW2_LOW11 = 0x160;
constexpr uint16_t MASK_CACHE_PREF_OP2731 = 0b1111100000000000;  // hw2[15:11]
constexpr uint8_t SHIFT_CACHE_PREF_OP2731 = 11;

constexpr uint8_t REG2_CACHE_HI3 = 0b111;  // hw1 reg2 bits[4:2] (op1315)
constexpr uint8_t MASK_REG2_HI3 = 0b11100;
constexpr uint8_t MASK_REG2_LO2 = 0b00011;
constexpr uint8_t REG2_PREF = 0b11011;  // hw1 reg2 (exact)

}  // namespace V850::Opcodes

#endif  // BINARYNINJA_API_V850_OPCODES_H
