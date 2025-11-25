// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#ifndef BINARYNINJA_API_V850_CONDITION_H
#define BINARYNINJA_API_V850_CONDITION_H

/* ------------------------------------------------------ */
/*   Condition codes for BCOND branch instruction         */
/*                                                        */
/*   (ordered by condition code to identify duplicates)   */
/* ------------------------------------------------------ */

namespace V850::Conditions {
constexpr uint8_t CONDITION_CODE_BV = 0b0000;  // OV == 1 Overflow
constexpr uint8_t CONDITION_CODE_BL =
    0b0001;  // CY == 1                   Lower (unsigned less than)
constexpr uint8_t CONDITION_CODE_BC = 0b0001;  // CY == 1 Carry
constexpr uint8_t CONDITION_CODE_BZ = 0b0010;  // Z == 1                    Zero
constexpr uint8_t CONDITION_CODE_BE = 0b0010;  // Z == 1 Equal
constexpr uint8_t CONDITION_CODE_BNH =
    0b0011;  // (CY or Z) == 1            Not higher (unsigned less than or
             // equal)
constexpr uint8_t CONDITION_CODE_BN = 0b0100;  // S == 1 Negative
constexpr uint8_t CONDITION_CODE_BR = 0b0101;  // NA Always (unconditional)
constexpr uint8_t CONDITION_CODE_BLT =
    0b0110;  // (S xor OV) == 1           Less than signed
constexpr uint8_t CONDITION_CODE_BLE =
    0b0111;  // ((S xor OV) or Z) == 1    Less than or equal signed
constexpr uint8_t CONDITION_CODE_BNV =
    0b1000;  // OV == 0                   No overflow
constexpr uint8_t CONDITION_CODE_BNC =
    0b1001;  // CY == 0                   No carry
constexpr uint8_t CONDITION_CODE_BNL =
    0b1001;  // CY == 0                   Not lower (unsigned greater than or
             // equal)
constexpr uint8_t CONDITION_CODE_BNZ =
    0b1010;  // Z == 0                    Not zero
constexpr uint8_t CONDITION_CODE_BNE =
    0b1010;  // Z == 0                    Not equal
constexpr uint8_t CONDITION_CODE_BH =
    0b1011;  // (CY or Z) == 0            Higher (unsigned greater than)
constexpr uint8_t CONDITION_CODE_BP = 0b1100;   // S == 0 Positive
constexpr uint8_t CONDITION_CODE_BSA = 0b1101;  // SAT == 1 Saturated
constexpr uint8_t CONDITION_CODE_BGE =
    0b1110;  // (S xor OV) == 0           Greater than or equal signed
constexpr uint8_t CONDITION_CODE_BGT =
    0b1111;  // ((S xor OV) or Z) == 0    Greater than signed

/* -------------------------------------- */
/*   Conditions for other instructions    */
/* -------------------------------------- */
constexpr uint8_t CONDITION_CODE_V = 0b0000;  // OV == 1 Overflow
constexpr uint8_t CONDITION_CODE_NV =
    0b1000;  // OV == 0                   No overflow
constexpr uint8_t CONDITION_CODE_C_L =
    0b0001;  // CY == 1                   Carry/Lower (unsigned less than)
constexpr uint8_t CONDITION_CODE_NC_NL =
    0b1001;  // CY == 0                   No carry/Not lower (unsigned greater
             // than or equal)
constexpr uint8_t CONDITION_CODE_Z = 0b0010;  // Z == 1                    Zero
constexpr uint8_t CONDITION_CODE_NZ =
    0b1010;  // Z == 0                    Not zero
constexpr uint8_t CONDITION_CODE_NH =
    0b0011;  // (CY or Z) == 1            Not higher (unsigned less than or
             // equal)
constexpr uint8_t CONDITION_CODE_H =
    0b1011;  // (CY or Z) == 0            Higher (unsigned greater than)
constexpr uint8_t CONDITION_CODE_S_N = 0b0100;   // S == 1 Negative
constexpr uint8_t CONDITION_CODE_NS_P = 0b1100;  // S == 0 Positive
constexpr uint8_t CONDITION_CODE_T = 0b0101;     // NA Always (unconditional)
constexpr uint8_t CONDITION_CODE_SA = 0b1101;    // SAT == 1 Saturated
constexpr uint8_t CONDITION_CODE_LT =
    0b0110;  // (S xor OV) == 1           Less than signed
constexpr uint8_t CONDITION_CODE_GE =
    0b1110;  // (S xor OV) == 0           Greater than or equal signed
constexpr uint8_t CONDITION_CODE_LE =
    0b0111;  // ((S xor OV) or Z) == 1    Less than or equal signed
constexpr uint8_t CONDITION_CODE_GT =
    0b1111;  // ((S xor OV) or Z) == 0    Greater than signed
}  // namespace V850::Conditions

#endif  // BINARYNINJA_API_V850_CONDITION_H
