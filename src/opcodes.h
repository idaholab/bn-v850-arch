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
constexpr uint16_t MASK_VII_DISP_W = 0b1111111111111100;
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

/* Format XIII */
// 5-bit opcode is bits 6-10 of word 1
constexpr uint8_t OP_XIII_DISPOSE =
    0b11001;  // TODO both dispose and prepare have 2 forms

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
}  // namespace V850::Opcodes

#endif  // BINARYNINJA_API_V850_OPCODES_H
