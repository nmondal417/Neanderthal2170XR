/*
 * Generated by Bluespec Compiler, version 2023.01-6-g034050db (build 034050db)
 * 
 * On Tue Apr 18 16:35:13 EDT 2023
 * 
 */

/* Generation options: */
#ifndef __mkpipelined_h__
#define __mkpipelined_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"
#include "mkScoreboard.h"


/* Class declaration for the mkpipelined module */
class MOD_mkpipelined : public Module {
 
 /* Clock handles */
 private:
  tClock __clk_handle_0;
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
  MOD_Reg<tUInt64> INST_commit_id;
  MOD_Fifo<tUWide> INST_d2e;
  MOD_Fifo<tUWide> INST_e2w;
  MOD_Fifo<tUWide> INST_f2d;
  MOD_Reg<tUInt64> INST_fresh_id;
  MOD_CReg<tUWide> INST_fromDmem_rv;
  MOD_CReg<tUWide> INST_fromImem_rv;
  MOD_CReg<tUWide> INST_fromMMIO_rv;
  MOD_Reg<tUInt32> INST_lfh;
  MOD_Reg<tUInt8> INST_mEpoch;
  MOD_Reg<tUInt32> INST_program_counter;
  MOD_Fifo<tUInt64> INST_retired;
  MOD_Reg<tUInt32> INST_rf_0;
  MOD_Reg<tUInt32> INST_rf_1;
  MOD_Reg<tUInt32> INST_rf_10;
  MOD_Reg<tUInt32> INST_rf_11;
  MOD_Reg<tUInt32> INST_rf_12;
  MOD_Reg<tUInt32> INST_rf_13;
  MOD_Reg<tUInt32> INST_rf_14;
  MOD_Reg<tUInt32> INST_rf_15;
  MOD_Reg<tUInt32> INST_rf_16;
  MOD_Reg<tUInt32> INST_rf_17;
  MOD_Reg<tUInt32> INST_rf_18;
  MOD_Reg<tUInt32> INST_rf_19;
  MOD_Reg<tUInt32> INST_rf_2;
  MOD_Reg<tUInt32> INST_rf_20;
  MOD_Reg<tUInt32> INST_rf_21;
  MOD_Reg<tUInt32> INST_rf_22;
  MOD_Reg<tUInt32> INST_rf_23;
  MOD_Reg<tUInt32> INST_rf_24;
  MOD_Reg<tUInt32> INST_rf_25;
  MOD_Reg<tUInt32> INST_rf_26;
  MOD_Reg<tUInt32> INST_rf_27;
  MOD_Reg<tUInt32> INST_rf_28;
  MOD_Reg<tUInt32> INST_rf_29;
  MOD_Reg<tUInt32> INST_rf_3;
  MOD_Reg<tUInt32> INST_rf_30;
  MOD_Reg<tUInt32> INST_rf_31;
  MOD_Reg<tUInt32> INST_rf_4;
  MOD_Reg<tUInt32> INST_rf_5;
  MOD_Reg<tUInt32> INST_rf_6;
  MOD_Reg<tUInt32> INST_rf_7;
  MOD_Reg<tUInt32> INST_rf_8;
  MOD_Reg<tUInt32> INST_rf_9;
  MOD_mkScoreboard INST_scoreboard;
  MOD_Fifo<tUInt64> INST_squashed;
  MOD_Reg<tUInt8> INST_starting;
  MOD_CReg<tUWide> INST_toDmem_rv;
  MOD_CReg<tUWide> INST_toImem_rv;
  MOD_CReg<tUWide> INST_toMMIO_rv;
 
 /* Constructor */
 public:
  MOD_mkpipelined(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUWide PORT_getIResp_a;
  tUWide PORT_getDResp_a;
  tUWide PORT_getMMIOResp_a;
  tUWide PORT_getIReq;
  tUWide PORT_getDReq;
  tUWide PORT_getMMIOReq;
 
 /* Publicly accessible definitions */
 public:
  tUWide DEF_toMMIO_rv_port1__read____d558;
  tUWide DEF_toDmem_rv_port1__read____d554;
  tUWide DEF_toImem_rv_port1__read____d550;
  tUInt8 DEF_rd_idx__h11773;
  tUInt8 DEF_d2e_first__21_BIT_188_26_OR_NOT_d2e_first__21__ETC___d230;
  tUInt8 DEF_d2e_first__21_BIT_188___d226;
  tUInt32 DEF_d2e_first__21_BITS_116_TO_85_31_PLUS_IF_d2e_fi_ETC___d272;
  tUInt8 DEF_f2d_first__1_BIT_48_2_EQ_mEpoch_7___d23;
  tUInt8 DEF_rd_idx__h5079;
  tUInt8 DEF_rs1_idx__h5077;
  tUInt8 DEF_rs2_idx__h5078;
  tUWide DEF_d2e_first____d221;
  tUWide DEF_e2w_first____d414;
  tUWide DEF_f2d_first____d21;
  tUWide DEF_fromMMIO_rv_port1__read____d422;
  tUWide DEF_fromMMIO_rv_port0__read____d560;
  tUWide DEF_toMMIO_rv_port0__read____d278;
  tUWide DEF_fromDmem_rv_port1__read____d424;
  tUWide DEF_fromDmem_rv_port0__read____d556;
  tUWide DEF_toDmem_rv_port0__read____d281;
  tUWide DEF_fromImem_rv_port1__read____d24;
  tUWide DEF_fromImem_rv_port0__read____d552;
  tUWide DEF_toImem_rv_port0__read____d4;
  tUInt8 DEF_starting__h4059;
  tUInt8 DEF_scoreboard_search3_fromImem_rv_port1__read__4__ETC___d32;
  tUInt8 DEF_scoreboard_search2_fromImem_rv_port1__read__4__ETC___d29;
  tUInt8 DEF_scoreboard_search1_fromImem_rv_port1__read__4__ETC___d27;
  tUInt8 DEF_y__h8422;
  tUInt32 DEF_rv1__h8271;
  tUInt32 DEF_d2e_first__21_BITS_116_TO_85_31_PLUS_IF_d2e_fi_ETC___d271;
  tUInt32 DEF_x__h8643;
  tUInt8 DEF_e2w_first__14_BITS_125_TO_123___d432;
  tUInt8 DEF_d2e_first__21_BIT_217___d232;
  tUInt8 DEF_d2e_first__21_BIT_213___d247;
  tUInt8 DEF_dEpoch__h8268;
  tUInt8 DEF_e2w_first__14_BIT_120___d420;
  tUInt8 DEF_e2w_first__14_BIT_84___d428;
  tUInt8 DEF_e2w_first__14_BIT_54___d415;
  tUInt8 DEF_fEpoch__h5053;
  tUInt32 DEF_imm__h8426;
  tUInt8 DEF_IF_d2e_first__21_BIT_217_32_THEN_d2e_first__21_ETC___d234;
  tUInt8 DEF_e2w_first__14_BITS_59_TO_55_30_EQ_0___d431;
  tUInt8 DEF_e2w_first__14_BITS_52_TO_51_16_EQ_0b0___d417;
  tUInt8 DEF_d2e_first__21_BITS_186_TO_185_27_EQ_0b0___d228;
  tUInt8 DEF_d2e_first__21_BIT_117_22_EQ_mEpoch_7___d223;
  tUInt8 DEF_NOT_d2e_first__21_BIT_117_22_EQ_mEpoch_7_23___d224;
  tUInt32 DEF_x__h8924;
  tUInt32 DEF_x__h8761;
  tUInt32 DEF_x__h8691;
 
 /* Local definitions */
 private:
  tUInt32 DEF_TASK_fopen___d2;
  tUInt32 DEF_signed_0___d15;
  tUInt32 DEF_lfh___d3;
  tUWide DEF_f2d_first__1_BITS_112_TO_48___d216;
  tUWide DEF_IF_fromImem_rv_port1__read__4_BITS_6_TO_0_09_E_ETC___d219;
  tUWide DEF_IF_fromImem_rv_port1__read__4_BITS_19_TO_15_6__ETC___d218;
  tUWide DEF_NOT_d2e_first__21_BIT_188_26_91_AND_d2e_first__ETC___d405;
  tUWide DEF_d2e_first__21_BITS_221_TO_182_03_CONCAT_d2e_fi_ETC___d404;
  tUWide DEF_program_counter_1_CONCAT_program_counter_1_PLU_ETC___d19;
  tUWide DEF__16_CONCAT_program_counter_1_CONCAT_0___d16;
  tUWide DEF__1_CONCAT_IF_d2e_first__21_BIT_187_95_THEN_IF_d_ETC___d313;
  tUWide DEF__1_CONCAT_getMMIOResp_a___d559;
  tUWide DEF__1_CONCAT_getDResp_a___d555;
  tUWide DEF__1_CONCAT_getIResp_a___d551;
  tUWide DEF__0_CONCAT_DONTCARE___d47;
 
 /* Rules */
 public:
  void RL_do_tic_logging();
  void RL_fetch();
  void RL_decode();
  void RL_execute();
  void RL_writeback();
  void RL_administrative_konata_commit();
  void RL_administrative_konata_flush();
 
 /* Methods */
 public:
  tUWide METH_getIReq();
  tUInt8 METH_RDY_getIReq();
  void METH_getIResp(tUWide ARG_getIResp_a);
  tUInt8 METH_RDY_getIResp();
  tUWide METH_getDReq();
  tUInt8 METH_RDY_getDReq();
  void METH_getDResp(tUWide ARG_getDResp_a);
  tUInt8 METH_RDY_getDResp();
  tUWide METH_getMMIOReq();
  tUInt8 METH_RDY_getMMIOReq();
  void METH_getMMIOResp(tUWide ARG_getMMIOResp_a);
  tUInt8 METH_RDY_getMMIOResp();
 
 /* Reset routines */
 public:
  void reset_RST_N(tUInt8 ARG_rst_in);
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
  void set_clk_0(char const *s);
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkpipelined &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkpipelined &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkpipelined &backing);
  void vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkpipelined &backing);
};

#endif /* ifndef __mkpipelined_h__ */
