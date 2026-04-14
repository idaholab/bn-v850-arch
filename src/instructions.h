// Copyright (c) 2025. Battelle Energy Alliance, LLC
// ALL RIGHTS RESERVED

#ifndef SRC_INSTRUCTIONS_H_
#define SRC_INSTRUCTIONS_H_

#include <binaryninjaapi.h>

#include <cstdint>

namespace BN = BinaryNinja;

namespace V850 {
enum IsaType { V850_E1_ISA };

/* Get instruction info, including size and effect on control flow */
bool Info_III_BCOND(uint64_t opcode, uint64_t addr,
                    BN::InstructionInfo &result);
bool Info_V_JARL_JR(uint64_t opcode, uint64_t addr,
                    BN::InstructionInfo &result);
bool Info_VI_MOVEA_MOV(const uint16_t *instruction_data,
                       BN::InstructionInfo &result);
bool Format_Ext_Info(uint64_t opcode, BN::InstructionInfo &result);

/* Get instruction text, parse arguments and assign to tokens, and format
 * strings to print mnemonics
 *
 * Note: Individual functions are used for instructions that share an opcode or
 * do not conform to the standard format. Others share the same format. */
bool Text_I_Generic_Reg1_Reg2(const char *mnemonic, uint16_t opcode,
                              size_t &len,
                              std::vector<BN::InstructionTextToken> &result);
bool Text_I_MOV_NOP(uint16_t opcode, size_t &len,
                    std::vector<BN::InstructionTextToken> &result);
bool Text_I_SWITCH_DBTRAP_DIVH(uint16_t opcode, size_t &len,
                               std::vector<BN::InstructionTextToken> &result);
bool Text_I_SATSUBR_ZXB(uint16_t opcode, size_t &len,
                        std::vector<BN::InstructionTextToken> &result);
bool Text_I_SATSUB_SXB(uint16_t opcode, size_t &len,
                       std::vector<BN::InstructionTextToken> &result);
bool Text_I_SATADD_ZXH(uint16_t opcode, size_t &len,
                       std::vector<BN::InstructionTextToken> &result);
bool Text_I_MULH_SXH(uint16_t opcode, size_t &len,
                     std::vector<BN::InstructionTextToken> &result);
bool Text_I_JMP_IV_SLDHU_SLDBU(uint16_t opcode, size_t &len,
                               std::vector<BN::InstructionTextToken> &result);
bool Text_II_Generic_Imm5_Reg2(const char *mnemonic, uint16_t opcode,
                               size_t &len,
                               std::vector<BN::InstructionTextToken> &result);
bool Text_II_CALLT_SATADD_MOV(uint16_t opcode, size_t &len,
                              std::vector<BN::InstructionTextToken> &result);
bool Text_III_BCOND(uint16_t opcode, const uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result);
bool Text_IV_SLDB(uint16_t opcode, size_t &len,
                  std::vector<BN::InstructionTextToken> &result);
bool Text_IV_SSTB(uint16_t opcode, size_t &len,
                  std::vector<BN::InstructionTextToken> &result);
bool Text_IV_SLDH(uint16_t opcode, size_t &len,
                  std::vector<BN::InstructionTextToken> &result);
bool Text_IV_SSTH(uint16_t opcode, size_t &len,
                  std::vector<BN::InstructionTextToken> &result);
bool Text_IV_SLDW_SSTW(uint16_t opcode, size_t &len,
                       std::vector<BN::InstructionTextToken> &result);
bool Text_V_JARL_JR_VII_LDBU_XIII_PREPARE(
    uint64_t opcode, size_t &len,
    std::vector<BN::InstructionTextToken> &result);
bool Text_VI(uint64_t opcode, size_t &len,
             std::vector<BN::InstructionTextToken> &result);
bool Text_VI_MOVHI_SATSUBI_XIII_DISPOSE(
    uint64_t opcode, size_t &len,
    std::vector<BN::InstructionTextToken> &result);
bool Text_VII_LDB_LDH_LDW_STB_STH_STW(
    uint64_t opcode, size_t &len,
    std::vector<BN::InstructionTextToken> &result);
bool Text_VIII_SET1_NOT1_CLR1_TST1(
    uint64_t opcode, size_t &len,
    std::vector<BN::InstructionTextToken> &result);
bool Format_Ext_Text(uint64_t opcode, size_t &len,
                     std::vector<BN::InstructionTextToken> &result);

/*
 * Abstract instruction class that all instructions must use.
 * This allows the return type of the decoder function (DecodeInstruction) to
 * be generic.
 */
class Instruction {
  IsaType isa_type;
  uint8_t len;

 public:
  virtual ~Instruction() = default;
  explicit Instruction(const IsaType &t, uint8_t len);

  virtual bool Text(uint64_t opcode, uint64_t addr, size_t &len,
                    std::vector<BN::InstructionTextToken> &result) = 0;

  virtual bool Info(uint64_t opcode, uint64_t addr,
                    BN::InstructionInfo &result);

  virtual bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il,
                    BinaryNinja::Architecture *arch) = 0;

  [[nodiscard]] IsaType GetIsaType()
      const;  // can be used by each method to make isa-specific decisions

  [[nodiscard]] uint8_t GetInstrLen() const;
};

// Main method to begin decoding an instruction via its opcode
std::optional<std::unique_ptr<Instruction>> DecodeInstruction(const IsaType &t,
                                                              uint32_t opcode);
std::optional<std::unique_ptr<Instruction>> ParsePrefix0b0(const IsaType &t,
                                                           uint16_t opcode);
std::optional<std::unique_ptr<Instruction>> ParsePrefix0b1(const IsaType &t,
                                                           uint32_t opcode);
std::optional<std::unique_ptr<Instruction>> ParsePrefix0b01(const IsaType &t,
                                                            uint16_t opcode);
std::optional<std::unique_ptr<Instruction>> ParsePrefix0b001(const IsaType &t,
                                                             uint16_t opcode);
std::optional<std::unique_ptr<Instruction>> ParsePrefix0b0001(const IsaType &t,
                                                              uint16_t opcode);
std::optional<std::unique_ptr<Instruction>> ParsePrefix0b00001(const IsaType &t,
                                                               uint16_t opcode);
std::optional<std::unique_ptr<Instruction>> ParsePrefix0b000001(
    const IsaType &t, uint16_t opcode);

/* Get instruction logic as Binary Ninja low level IL (LLIL) */
bool Lift_I_JMP_IV_SLDHU_SLDBU(uint64_t opcode, size_t &len,
                               BN::LowLevelILFunction &il);
bool Lift_I_MOV_NOP(uint64_t opcode, uint64_t addr, size_t &len,
                    BN::LowLevelILFunction &il);
bool Lift_I_NOT(uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il);
bool Lift_I_SWITCH_DBTRAP_DIVH(uint64_t opcode, uint64_t addr, size_t &len,
                               BN::LowLevelILFunction &il,
                               BinaryNinja::Architecture *arch);
bool Lift_I_SATSUBR_ZXB(uint64_t opcode, uint64_t addr, size_t &len,
                        BN::LowLevelILFunction &il);
bool Lift_I_SATSUB_SXB(uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il);
bool Lift_I_SATADD_ZXH(uint64_t opcode, uint64_t addr, size_t &len,
                       BN::LowLevelILFunction &il);
bool Lift_I_MULH_SXH(uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il);
bool Lift_I_OR(uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il);
bool Lift_I_XOR(uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il);
bool Lift_I_AND(uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il);
bool Lift_I_TST(uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il);
bool Lift_I_SUBR(uint64_t opcode, uint64_t addr, size_t &len,
                 BN::LowLevelILFunction &il);
bool Lift_I_CMP(uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il);
bool Lift_I_SUB(uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il);
bool Lift_I_ADD(uint64_t opcode, uint64_t addr, size_t &len,
                BN::LowLevelILFunction &il);

bool Lift_II(uint64_t opcode, uint64_t addr, size_t &len,
             BN::LowLevelILFunction &il);
bool Lift_III(uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il, BinaryNinja::Architecture *arch);
bool Lift_IV_SLDB(uint64_t opcode, uint64_t addr, size_t &len,
                  BN::LowLevelILFunction &il);
bool Lift_IV_SSTB(uint64_t opcode, uint64_t addr, size_t &len,
                  BN::LowLevelILFunction &il);
bool Lift_IV(uint64_t opcode, uint64_t addr, size_t &len,
             BN::LowLevelILFunction &il);
bool Lift_V_JARL_JR_VII_LDBU_XIII_PREPARE(uint64_t opcode, const uint64_t addr,
                                          size_t &len,
                                          BN::LowLevelILFunction &il,
                                          BinaryNinja::Architecture *arch);
bool Lift_VI_MOVHI_SATSUBI_XIII_DISPOSE(uint64_t opcode, uint64_t addr,
                                        size_t &len,
                                        BN::LowLevelILFunction &il);
bool Lift_VI(uint64_t opcode, uint64_t addr, size_t &len,
             BN::LowLevelILFunction &il);
bool Lift_VII(uint64_t opcode, uint64_t addr, size_t &len,
              BN::LowLevelILFunction &il);
bool Lift_VIII(uint64_t opcode, uint64_t addr, size_t &len,
               BN::LowLevelILFunction &il);
bool Format_Ext_Lift(uint64_t opcode, uint64_t addr, size_t &len,
                     BN::LowLevelILFunction &il);

/*
 * Concrete classes for CPU instructions that should be returned from
 * DecodeInstruction.
 */

class AddImm5R2 : public Instruction {
 public:
  explicit AddImm5R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class AddR1R2 : public Instruction {
 public:
  explicit AddR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class AndiImm16R1R2 : public Instruction {
 public:
  explicit AndiImm16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class AddiImm32R1R2 : public Instruction {
 public:
  explicit AddiImm32R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class AndR1R2 : public Instruction {
 public:
  explicit AndR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bc : public Instruction {
 public:
  explicit Bc(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bge : public Instruction {
 public:
  explicit Bge(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bgt : public Instruction {
 public:
  explicit Bgt(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bh : public Instruction {
 public:
  explicit Bh(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Ble : public Instruction {
 public:
  explicit Ble(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Blt : public Instruction {
 public:
  explicit Blt(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bn : public Instruction {
 public:
  explicit Bn(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bnc : public Instruction {
 public:
  explicit Bnc(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bnh : public Instruction {
 public:
  explicit Bnh(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bnv : public Instruction {
 public:
  explicit Bnv(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bnz : public Instruction {
 public:
  explicit Bnz(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bp : public Instruction {
 public:
  explicit Bp(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Br : public Instruction {
 public:
  explicit Br(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bsa : public Instruction {
 public:
  explicit Bsa(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class BshR2R3 : public Instruction {
 public:
  explicit BshR2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class BswR2R3 : public Instruction {
 public:
  explicit BswR2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class BinsR1PosWidthR2 : public Instruction {
 public:
  explicit BinsR1PosWidthR2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bv : public Instruction {
 public:
  explicit Bv(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Bz : public Instruction {
 public:
  explicit Bz(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class CalltImm6 : public Instruction {
 public:
  explicit CalltImm6(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Clr1Bit3Disp16R1 : public Instruction {
 public:
  explicit Clr1Bit3Disp16R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Clr1R2R1 : public Instruction {
 public:
  explicit Clr1R2R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class CmovCcccR1R2R3 : public Instruction {
 public:
  explicit CmovCcccR1R2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class CmovCccImm5R2R3 : public Instruction {
 public:
  explicit CmovCccImm5R2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class CmpImm5R2 : public Instruction {
 public:
  explicit CmpImm5R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class CmpR1R2 : public Instruction {
 public:
  explicit CmpR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Ctret : public Instruction {
 public:
  explicit Ctret(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Dbret : public Instruction {
 public:
  explicit Dbret(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Dbtrap : public Instruction {
 public:
  explicit Dbtrap(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// Format I synchronization instructions: SYNCE, SYNCI, SYNCM, SYNCP.
// All four share the same 16-bit Format I shape (differ only by low 5 bits)
// and lower to architectural barriers with no observable register effect on
// this CPU (G3MH Software Manual pp. 287-290). Represented as a single class
// carrying the decoded mnemonic for rendering; lift emits an intrinsic so the
// decompiler preserves the barrier.
class SyncBarrier : public Instruction {
  const char *mnemonic;
  const char *intrinsic;

 public:
  explicit SyncBarrier(const IsaType &t, uint8_t len, const char *mnemonic,
                       const char *intrinsic);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Di : public Instruction {
 public:
  explicit Di(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class DisposeImm5List12 : public Instruction {
 public:
  explicit DisposeImm5List12(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class DisposeImm5List12R1 : public Instruction {
 public:
  explicit DisposeImm5List12R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class DivR1R2R3 : public Instruction {
 public:
  explicit DivR1R2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class DivhR1R2 : public Instruction {
 public:
  explicit DivhR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class DivhR1R2R3 : public Instruction {
 public:
  explicit DivhR1R2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class DivhuR1R2R3 : public Instruction {
 public:
  explicit DivhuR1R2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class DivuR1R2R3 : public Instruction {
 public:
  explicit DivuR1R2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Ei : public Instruction {
 public:
  explicit Ei(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Halt : public Instruction {
 public:
  explicit Halt(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class HswR2R3 : public Instruction {
 public:
  explicit HswR2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class JarlDisp22R2 : public Instruction {
 public:
  explicit JarlDisp22R2(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class JmpR1 : public Instruction {
 public:
  explicit JmpR1(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class JrDisp22 : public Instruction {
 public:
  explicit JrDisp22(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class LdbDisp16R1R2 : public Instruction {
 public:
  explicit LdbDisp16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class LdbuDisp16R1R2 : public Instruction {
 public:
  explicit LdbuDisp16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class LdhDisp16R1R2 : public Instruction {
 public:
  explicit LdhDisp16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class LdhuDisp16R1R2 : public Instruction {
 public:
  explicit LdhuDisp16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class LdsrR1Rid : public Instruction {
 public:
  explicit LdsrR1Rid(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class LdwDisp16R1R2 : public Instruction {
 public:
  explicit LdwDisp16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MovhiImm16R1R2 : public Instruction {
 public:
  explicit MovhiImm16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MoveaImm16R1R2 : public Instruction {
 public:
  explicit MoveaImm16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MovImm32R1 : public Instruction {
 public:
  explicit MovImm32R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MovImm5R2 : public Instruction {
 public:
  explicit MovImm5R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MovR1R2 : public Instruction {
 public:
  explicit MovR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MulhiImm16R1R2 : public Instruction {
 public:
  explicit MulhiImm16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MulhImm5R2 : public Instruction {
 public:
  explicit MulhImm5R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MulImm9R2R3 : public Instruction {
 public:
  explicit MulImm9R2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MulhR1R2 : public Instruction {
 public:
  explicit MulhR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MulR1R2R3 : public Instruction {
 public:
  explicit MulR1R2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MuluImm9R2R3 : public Instruction {
 public:
  explicit MuluImm9R2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class MuluR1R2R3 : public Instruction {
 public:
  explicit MuluR1R2R3(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// MAC reg1, reg2, reg3, reg4 (Format XI, V850E2/E3)
// GR[reg4+1] || GR[reg4] <- GR[reg2] * GR[reg1] + GR[reg3+1] || GR[reg3]
// G3MH Software Manual p. 215
class MacR1R2R3R4 : public Instruction {
 public:
  explicit MacR1R2R3R4(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// MACU reg1, reg2, reg3, reg4 (Format XI, V850E2/E3)
// G3MH Software Manual p. 216
class MacuR1R2R3R4 : public Instruction {
 public:
  explicit MacuR1R2R3R4(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Nop : public Instruction {
 public:
  explicit Nop(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Not1Bit3Disp16R1 : public Instruction {
 public:
  explicit Not1Bit3Disp16R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Not1R2R1 : public Instruction {
 public:
  explicit Not1R2R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class NotR1R2 : public Instruction {
 public:
  explicit NotR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class OriImm16R1R2 : public Instruction {
 public:
  explicit OriImm16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class OrR1R2 : public Instruction {
 public:
  explicit OrR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class PrepareList12Imm5 : public Instruction {
 public:
  explicit PrepareList12Imm5(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class PrepareList12Imm5Sp : public Instruction {
 public:
  explicit PrepareList12Imm5Sp(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class PrepareList12Imm5SpImm16SignExt : public Instruction {
 public:
  explicit PrepareList12Imm5SpImm16SignExt(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class PrepareList12Imm5SpImm16LogicShift : public Instruction {
 public:
  explicit PrepareList12Imm5SpImm16LogicShift(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class PrepareList12Imm5SpImm32 : public Instruction {
 public:
  explicit PrepareList12Imm5SpImm32(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Reti : public Instruction {
 public:
  explicit Reti(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// V850E3/RH850 G3MH split RETI into EIRET (return from EI-level exception)
// and FERET (return from FE-level exception / NMI). Encodings:
//   EIRET = 0x07E0 0148  -> pc <- EIPC,  PSW <- EIPSW
//   FERET = 0x07E0 014A  -> pc <- FEPC,  PSW <- FEPSW
class Eiret : public Instruction {
 public:
  explicit Eiret(const IsaType &t, uint8_t len);
  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Feret : public Instruction {
 public:
  explicit Feret(const IsaType &t, uint8_t len);
  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SarImm5R2 : public Instruction {
 public:
  explicit SarImm5R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SarR1R2 : public Instruction {
 public:
  explicit SarR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SasfCondR2 : public Instruction {
 public:
  explicit SasfCondR2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SataddImm5 : public Instruction {
 public:
  explicit SataddImm5(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SataddR1R2 : public Instruction {
 public:
  explicit SataddR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SatsubiImm16R1R2 : public Instruction {
 public:
  explicit SatsubiImm16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SatsubR1R2 : public Instruction {
 public:
  explicit SatsubR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SatsubrR1R2 : public Instruction {
 public:
  explicit SatsubrR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Set1Bit3Disp16R1 : public Instruction {
 public:
  explicit Set1Bit3Disp16R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Set1R2R1 : public Instruction {
 public:
  explicit Set1R2R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SetfCondR2 : public Instruction {
 public:
  explicit SetfCondR2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class ShlImm5R2 : public Instruction {
 public:
  explicit ShlImm5R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class ShlR1R2 : public Instruction {
 public:
  explicit ShlR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class ShrImm5R2 : public Instruction {
 public:
  explicit ShrImm5R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class ShrR1R2 : public Instruction {
 public:
  explicit ShrR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SldbuDisp4R2 : public Instruction {
 public:
  explicit SldbuDisp4R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SldbDisp7R2 : public Instruction {
 public:
  explicit SldbDisp7R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SldhuDisp5R2 : public Instruction {
 public:
  explicit SldhuDisp5R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SldhDisp8R2 : public Instruction {
 public:
  explicit SldhDisp8R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SldwDisp8R2 : public Instruction {
 public:
  explicit SldwDisp8R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SstbR2Disp7 : public Instruction {
 public:
  explicit SstbR2Disp7(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SsthR2Disp8 : public Instruction {
 public:
  explicit SsthR2Disp8(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SstwR2Disp8 : public Instruction {
 public:
  explicit SstwR2Disp8(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class StbR2Disp16R1 : public Instruction {
 public:
  explicit StbR2Disp16R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SthR2Disp26R1 : public Instruction {
 public:
  explicit SthR2Disp26R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class StsrRidR2 : public Instruction {
 public:
  explicit StsrRidR2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class StwR2Disp16R1 : public Instruction {
 public:
  explicit StwR2Disp16R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SubR1R2 : public Instruction {
 public:
  explicit SubR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SubrR1R2 : public Instruction {
 public:
  explicit SubrR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SwitchR1 : public Instruction {
 public:
  explicit SwitchR1(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SxbR1 : public Instruction {
 public:
  explicit SxbR1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class SxhR1 : public Instruction {
 public:
  explicit SxhR1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Trap : public Instruction {
 public:
  explicit Trap(const IsaType &t, uint8_t len);

  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Tst1Bit3Disp16R1 : public Instruction {
 public:
  explicit Tst1Bit3Disp16R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class Tst1R2R1 : public Instruction {
 public:
  explicit Tst1R2R1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class TstR1R2 : public Instruction {
 public:
  explicit TstR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class XoriImm16R1R2 : public Instruction {
 public:
  explicit XoriImm16R1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class XorR1R2 : public Instruction {
 public:
  explicit XorR1R2(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class ZxbR1 : public Instruction {
 public:
  explicit ZxbR1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

class ZxhR1 : public Instruction {
 public:
  explicit ZxhR1(const IsaType &t, uint8_t len);

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;

  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

/* -----------------------------------------------------------------
 * V850E3 / RH850 G3MH additions (decoder-gap fills)
 * ----------------------------------------------------------------- */

// PUSHSP rh-rt (Format XI, G3MH p.237)
class PushspRhRt : public Instruction {
 public:
  explicit PushspRhRt(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

/* ==========================================================================
 * V850E3 / RH850 single-precision FPU instructions (Format F:I).
 *
 * Every single-precision FPU instruction in the manual has:
 *   HW1 bits 10..5 == 0b111111 (same extended-opcode prefix)
 *   HW2 bit 10     == 1          (category = 0b100 -- floating point)
 *
 * Disambiguation is performed in ParseFpuSingle() by (type, sub-op, R).
 * References (RH850G3MH Software Manual R01US0143EJ0130):
 *   ADDF.S  p.319   SUBF.S  p.391   MULF.S  p.373
 *   DIVF.S  p.348   MAXF.S  p.366   MINF.S  p.370
 *   ABSF.S  p.315   NEGF.S  p.378   SQRTF.S p.389
 *   RECIPF.S p.381  RSQRTF.S p.385
 *   FLOORF.SW/SUW p.357    CEILF.SW/SUW  p.327
 *   TRNCF.SW/SUW  p.395    CVTF.SW/SUW   p.341
 *   CVTF.WS/UWS   p.345    CVTF.SH/HS    p.339
 *   ROUNDF.SW/SUW (RH850 G3KH only)
 *   CMPF.S  p.335   CMOVF.S p.330   TRFSR p.399
 *   FMAF.S  p.351   FMSF.S  p.353   FNMAF.S p.355  FNMSF.S p.357
 * ==========================================================================*/
enum class FpuOp : uint8_t {
  // arithmetic (type=01, three-operand)
  AddfS, SubfS, MulfS, DivfS, MaxfS, MinfS,
  // unary (type=01)
  AbsfS, NegfS, SqrtfS, RecipfS, RsqrtfS,
  // round/cast unary (type=01)
  RoundfSw, TrncfSw, CeilfSw, FloorfSw, CvtfSw,
  RoundfSuw, TrncfSuw, CeilfSuw, FloorfSuw, CvtfSuw,
  CvtfWs, CvtfHs, CvtfSh, CvtfUws,
  // FMA (type=11)
  FmafS, FmsfS, FnmafS, FnmsfS,
  // compare/move/transfer (type=00 or 01, special)
  CmpfS, CmovfS, Trfsr,
};

class FpuSingle : public Instruction {
  FpuOp op;

 public:
  explicit FpuSingle(const IsaType &t, uint8_t len, FpuOp op);

  [[nodiscard]] FpuOp GetOp() const { return op; }

  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// POPSP rh-rt (Format XI, G3MH p.232)
class PopspRhRt : public Instruction {
 public:
  explicit PopspRhRt(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// CAXI [reg1], reg2, reg3 (Format XI, G3MH p.167)
class CaxiR1R2R3 : public Instruction {
 public:
  explicit CaxiR1R2R3(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// JARL [reg1], reg3 (Format XI form, G3MH p.197)
class JarlR1R3 : public Instruction {
 public:
  explicit JarlR1R3(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// SNOOZE (Format X, G3MH p.268)
class Snooze : public Instruction {
 public:
  explicit Snooze(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// RIE (Format I 16-bit, G3MH p.239)
class RieI : public Instruction {
 public:
  explicit RieI(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// RIE imm5, imm4 (Format X 32-bit, G3MH p.239)
class RieX : public Instruction {
 public:
  explicit RieX(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Info(uint64_t opcode, uint64_t addr,
            BN::InstructionInfo &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// SCH0L reg2, reg3 (G3MH p.251)
class Sch0lR2R3 : public Instruction {
 public:
  explicit Sch0lR2R3(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// SCH0R reg2, reg3 (G3MH p.252)
class Sch0rR2R3 : public Instruction {
 public:
  explicit Sch0rR2R3(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// SCH1L reg2, reg3 (G3MH p.253)
class Sch1lR2R3 : public Instruction {
 public:
  explicit Sch1lR2R3(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

// SCH1R reg2, reg3 (G3MH p.254)
class Sch1rR2R3 : public Instruction {
 public:
  explicit Sch1rR2R3(const IsaType &t, uint8_t len);
  bool Text(uint64_t opcode, uint64_t addr, size_t &len,
            std::vector<BN::InstructionTextToken> &result) override;
  bool Lift(uint64_t opcode, uint64_t addr, size_t &len,
            BN::LowLevelILFunction &il,
            BinaryNinja::Architecture *arch) override;
};

/* Dispatcher for single-precision FPU ops. Called from ParsePrefix0b1 when
   op6bit == OP_EXT_6BIT and HW2 bit 10 (category bit 2) is set. */
std::optional<std::unique_ptr<Instruction>> ParseFpuSingle(const IsaType &t,
                                                           uint32_t opcode);

/* Intrinsic identifiers for FPU ops without a native BN LLIL primitive.
   Wired into V850E1Architecture::GetAllIntrinsics / GetIntrinsicName /
   GetIntrinsicInputs / GetIntrinsicOutputs. */
namespace FpuIntrinsic {
enum : uint32_t {
  MaxfS = 0x1000,
  MinfS,
  RecipfS,
  RsqrtfS,
  RoundfSw,
  RoundfSuw,
  TrncfSuw,
  CeilfSuw,
  FloorfSuw,
  CvtfSuw,
  CvtfUws,
  CvtfHs,
  CvtfSh,
  FmafS,
  FmsfS,
  FnmafS,
  FnmsfS,
  CmpfS,
  Trfsr,
  _END
};
}  // namespace FpuIntrinsic

}  // namespace V850

#endif  // SRC_INSTRUCTIONS_H_
