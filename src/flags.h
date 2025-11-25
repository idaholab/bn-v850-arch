// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#ifndef BINARYNINJA_API_V850_FLAGS_H
#define BINARYNINJA_API_V850_FLAGS_H

namespace V850::Flags {
/* -------------------------------- */
/*        Flag fields in PSW        */
/* -------------------------------- */
/*  31   8 7  6  5   4  3  2  1 0   */
/*  [ ... |NP|EP|ID|SAT|CY|OV|S|Z]  */
/* -------------------------------- */
constexpr uint8_t FLAG_Z_ZERO = 0;                // Z
constexpr uint8_t FLAG_S_SIGN = 1;                // S
constexpr uint8_t FLAG_OV_OVERFLOW = 2;           // OV
constexpr uint8_t FLAG_CY_CARRY = 3;              // CY
constexpr uint8_t FLAG_SAT_SATURATED = 4;         // SAT
constexpr uint8_t FLAG_ID_INTERRUPT_DISABLE = 5;  // ID
constexpr uint8_t FLAG_EP_EXCEPTION_PENDING = 6;  // EP
constexpr uint8_t FLAG_NP_NMI_PENDING = 7;        // NP
// Note: 8-31 reserved for future use

constexpr uint32_t MASK_CLEAR_ID_FLAG = 0xFFFFFFDF;
constexpr uint32_t MASK_SET_ID_FLAG = 0x00000020;
constexpr uint32_t MASK_SET_EP_FLAG = 0x00000040;
constexpr uint32_t MASK_SET_NP_FLAG = 0x00000080;

/* -------------------- */
/*   Flag role groups   */
/* -------------------- */
constexpr uint8_t FLAGS_WRITE_Z = 0;
constexpr uint8_t FLAGS_WRITE_S_Z = 1;
constexpr uint8_t FLAGS_WRITE_OV_S_Z = 2;
constexpr uint8_t FLAGS_WRITE_CY_OV_S_Z = 3;
constexpr uint8_t FLAGS_WRITE_SAT_CY_OV_S_Z = 4;
constexpr uint8_t FLAGS_WRITE_ID = 5;
}  // namespace V850::Flags

#endif  // BINARYNINJA_API_V850_FLAGS_H
