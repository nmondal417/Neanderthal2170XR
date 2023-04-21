/*
 * Generated by Bluespec Compiler, version 2023.01-6-g034050db (build 034050db)
 * 
 * On Fri Apr 21 01:50:39 EDT 2023
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
  MOD_Reg<tUInt32> INST_count;
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
  tUWide DEF_toMMIO_rv_port1__read____d547;
  tUWide DEF_toDmem_rv_port1__read____d543;
  tUWide DEF_toImem_rv_port1__read____d539;
  tUInt8 DEF_rd_idx__h10826;
  tUInt8 DEF_d2e_first__23_BIT_188___d228;
  tUInt32 DEF_d2e_first__23_BITS_116_TO_85_33_PLUS_IF_d2e_fi_ETC___d274;
  tUInt8 DEF_f2d_first__3_BIT_48_4_EQ_mEpoch_9___d25;
  tUInt8 DEF_scoreboard_search1_fromImem_rv_port1__read__6__ETC___d29;
  tUInt8 DEF_scoreboard_search2_fromImem_rv_port1__read__6__ETC___d31;
  tUInt8 DEF_scoreboard_search3_fromImem_rv_port1__read__6__ETC___d34;
  tUInt8 DEF_rd_idx__h5068;
  tUInt8 DEF_rs1_idx__h5066;
  tUInt8 DEF_rs2_idx__h5067;
  tUWide DEF_d2e_first____d223;
  tUWide DEF_e2w_first____d401;
  tUWide DEF_f2d_first____d23;
  tUWide DEF_fromMMIO_rv_port1__read____d414;
  tUWide DEF_fromMMIO_rv_port0__read____d549;
  tUWide DEF_toMMIO_rv_port0__read____d280;
  tUWide DEF_fromDmem_rv_port1__read____d416;
  tUWide DEF_fromDmem_rv_port0__read____d545;
  tUWide DEF_toDmem_rv_port0__read____d283;
  tUWide DEF_fromImem_rv_port1__read____d26;
  tUWide DEF_fromImem_rv_port0__read____d541;
  tUWide DEF_toImem_rv_port0__read____d6;
  tUInt8 DEF_starting__h4136;
  tUInt8 DEF_y__h8023;
  tUInt32 DEF_rv1__h7991;
  tUInt32 DEF_d2e_first__23_BITS_116_TO_85_33_PLUS_IF_d2e_fi_ETC___d273;
  tUInt32 DEF_x__h8334;
  tUInt8 DEF_e2w_first__01_BITS_125_TO_123___d422;
  tUInt8 DEF_d2e_first__23_BIT_217___d234;
  tUInt8 DEF_d2e_first__23_BIT_213___d249;
  tUInt8 DEF_dEpoch__h7988;
  tUInt8 DEF_e2w_first__01_BIT_120___d407;
  tUInt8 DEF_e2w_first__01_BIT_84___d409;
  tUInt8 DEF_e2w_first__01_BIT_54___d402;
  tUInt8 DEF_fEpoch__h5042;
  tUInt32 DEF_imm__h8051;
  tUInt8 DEF_IF_d2e_first__23_BIT_217_34_THEN_d2e_first__23_ETC___d236;
  tUInt8 DEF_e2w_first__01_BITS_59_TO_55_20_EQ_0___d421;
  tUInt8 DEF_e2w_first__01_BITS_52_TO_51_03_EQ_0b0___d404;
  tUInt8 DEF_d2e_first__23_BITS_186_TO_185_29_EQ_0b0___d230;
  tUInt8 DEF_d2e_first__23_BIT_117_24_EQ_mEpoch_9___d225;
  tUInt8 DEF_NOT_d2e_first__23_BIT_117_24_EQ_mEpoch_9_25___d226;
  tUInt8 DEF_NOT_e2w_first__01_BIT_120_07___d408;
  tUInt32 DEF_x__h8615;
  tUInt32 DEF_x__h8452;
  tUInt32 DEF_x__h8382;
 
 /* Local definitions */
 private:
  tUInt32 DEF_TASK_fopen___d4;
  tUInt32 DEF_signed_0___d17;
  tUInt32 DEF_lfh___d5;
  tUWide DEF_f2d_first__3_BITS_112_TO_48___d179;
  tUWide DEF_IF_fromImem_rv_port1__read__6_BITS_6_TO_0_0_EQ_ETC___d221;
  tUWide DEF_IF_fromImem_rv_port1__read__6_BITS_19_TO_15_8__ETC___d220;
  tUWide DEF_NOT_d2e_first__23_BIT_188_28_92_AND_d2e_first__ETC___d393;
  tUWide DEF_d2e_first__23_BITS_221_TO_182_90_CONCAT_d2e_fi_ETC___d392;
  tUWide DEF_program_counter_3_CONCAT_program_counter_3_PLU_ETC___d21;
  tUWide DEF__16_CONCAT_program_counter_3_CONCAT_0___d18;
  tUWide DEF__1_CONCAT_IF_d2e_first__23_BIT_187_96_THEN_1_EL_ETC___d304;
  tUWide DEF__1_CONCAT_getMMIOResp_a___d548;
  tUWide DEF__1_CONCAT_getDResp_a___d544;
  tUWide DEF__1_CONCAT_getIResp_a___d540;
  tUWide DEF__0_CONCAT_DONTCARE___d49;
 
 /* Rules */
 public:
  void RL_doTic();
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
