/*
 * Generated by Bluespec Compiler, version 2023.01-6-g034050db (build 034050db)
 * 
 * On Tue Apr 11 13:16:01 EDT 2023
 * 
 */
#include "bluesim_primitives.h"
#include "mkDelayLine.h"


/* String declarations */
static std::string const __str_literal_2("Stream %d", 9u);
static std::string const __str_literal_1("TIC %d", 6u);


/* Constructor */
MOD_mkDelayLine::MOD_mkDelayLine(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST_cnt_f(simHdl, "cnt_f", this, 10u, 0u, (tUInt8)0u),
    INST_cnt_s(simHdl, "cnt_s", this, 10u, 0u, (tUInt8)0u),
    INST_ctime(simHdl, "ctime", this, 32u, 0u, (tUInt8)0u),
    INST_dl_d_0_rv(simHdl, "dl_d_0_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_10_rv(simHdl, "dl_d_10_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_11_rv(simHdl, "dl_d_11_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_12_rv(simHdl, "dl_d_12_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_13_rv(simHdl, "dl_d_13_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_14_rv(simHdl, "dl_d_14_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_15_rv(simHdl, "dl_d_15_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_16_rv(simHdl, "dl_d_16_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_17_rv(simHdl, "dl_d_17_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_18_rv(simHdl, "dl_d_18_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_19_rv(simHdl, "dl_d_19_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_1_rv(simHdl, "dl_d_1_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_2_rv(simHdl, "dl_d_2_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_3_rv(simHdl, "dl_d_3_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_4_rv(simHdl, "dl_d_4_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_5_rv(simHdl, "dl_d_5_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_6_rv(simHdl, "dl_d_6_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_7_rv(simHdl, "dl_d_7_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_8_rv(simHdl, "dl_d_8_rv", this, 11u, 682u, (tUInt8)0u),
    INST_dl_d_9_rv(simHdl, "dl_d_9_rv", this, 11u, 682u, (tUInt8)0u),
    PORT_RST_N((tUInt8)1u)
{
  symbol_count = 46u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkDelayLine::init_symbols_0()
{
  init_symbol(&symbols[0u], "cnt_f", SYM_MODULE, &INST_cnt_f);
  init_symbol(&symbols[1u], "cnt_f__h10151", SYM_DEF, &DEF_cnt_f__h10151, 10u);
  init_symbol(&symbols[2u], "cnt_s", SYM_MODULE, &INST_cnt_s);
  init_symbol(&symbols[3u], "ctime", SYM_MODULE, &INST_ctime);
  init_symbol(&symbols[4u], "dl_d_0_rv", SYM_MODULE, &INST_dl_d_0_rv);
  init_symbol(&symbols[5u], "dl_d_10_rv", SYM_MODULE, &INST_dl_d_10_rv);
  init_symbol(&symbols[6u], "dl_d_11_rv", SYM_MODULE, &INST_dl_d_11_rv);
  init_symbol(&symbols[7u], "dl_d_12_rv", SYM_MODULE, &INST_dl_d_12_rv);
  init_symbol(&symbols[8u], "dl_d_13_rv", SYM_MODULE, &INST_dl_d_13_rv);
  init_symbol(&symbols[9u], "dl_d_14_rv", SYM_MODULE, &INST_dl_d_14_rv);
  init_symbol(&symbols[10u], "dl_d_15_rv", SYM_MODULE, &INST_dl_d_15_rv);
  init_symbol(&symbols[11u], "dl_d_16_rv", SYM_MODULE, &INST_dl_d_16_rv);
  init_symbol(&symbols[12u], "dl_d_17_rv", SYM_MODULE, &INST_dl_d_17_rv);
  init_symbol(&symbols[13u], "dl_d_18_rv", SYM_MODULE, &INST_dl_d_18_rv);
  init_symbol(&symbols[14u], "dl_d_19_rv", SYM_MODULE, &INST_dl_d_19_rv);
  init_symbol(&symbols[15u], "dl_d_1_rv", SYM_MODULE, &INST_dl_d_1_rv);
  init_symbol(&symbols[16u], "dl_d_2_rv", SYM_MODULE, &INST_dl_d_2_rv);
  init_symbol(&symbols[17u], "dl_d_3_rv", SYM_MODULE, &INST_dl_d_3_rv);
  init_symbol(&symbols[18u], "dl_d_4_rv", SYM_MODULE, &INST_dl_d_4_rv);
  init_symbol(&symbols[19u], "dl_d_5_rv", SYM_MODULE, &INST_dl_d_5_rv);
  init_symbol(&symbols[20u], "dl_d_6_rv", SYM_MODULE, &INST_dl_d_6_rv);
  init_symbol(&symbols[21u], "dl_d_7_rv", SYM_MODULE, &INST_dl_d_7_rv);
  init_symbol(&symbols[22u], "dl_d_8_rv", SYM_MODULE, &INST_dl_d_8_rv);
  init_symbol(&symbols[23u], "dl_d_9_rv", SYM_MODULE, &INST_dl_d_9_rv);
  init_symbol(&symbols[24u], "RL_dl_try_move", SYM_RULE);
  init_symbol(&symbols[25u], "RL_dl_try_move_1", SYM_RULE);
  init_symbol(&symbols[26u], "RL_dl_try_move_10", SYM_RULE);
  init_symbol(&symbols[27u], "RL_dl_try_move_11", SYM_RULE);
  init_symbol(&symbols[28u], "RL_dl_try_move_12", SYM_RULE);
  init_symbol(&symbols[29u], "RL_dl_try_move_13", SYM_RULE);
  init_symbol(&symbols[30u], "RL_dl_try_move_14", SYM_RULE);
  init_symbol(&symbols[31u], "RL_dl_try_move_15", SYM_RULE);
  init_symbol(&symbols[32u], "RL_dl_try_move_16", SYM_RULE);
  init_symbol(&symbols[33u], "RL_dl_try_move_17", SYM_RULE);
  init_symbol(&symbols[34u], "RL_dl_try_move_18", SYM_RULE);
  init_symbol(&symbols[35u], "RL_dl_try_move_2", SYM_RULE);
  init_symbol(&symbols[36u], "RL_dl_try_move_3", SYM_RULE);
  init_symbol(&symbols[37u], "RL_dl_try_move_4", SYM_RULE);
  init_symbol(&symbols[38u], "RL_dl_try_move_5", SYM_RULE);
  init_symbol(&symbols[39u], "RL_dl_try_move_6", SYM_RULE);
  init_symbol(&symbols[40u], "RL_dl_try_move_7", SYM_RULE);
  init_symbol(&symbols[41u], "RL_dl_try_move_8", SYM_RULE);
  init_symbol(&symbols[42u], "RL_dl_try_move_9", SYM_RULE);
  init_symbol(&symbols[43u], "RL_feed", SYM_RULE);
  init_symbol(&symbols[44u], "RL_stream", SYM_RULE);
  init_symbol(&symbols[45u], "RL_tic", SYM_RULE);
}


/* Rule actions */

void MOD_mkDelayLine::RL_dl_try_move()
{
  tUInt8 DEF_dl_d_18_rv_port0__read_BIT_10_AND_NOT_dl_d_19__ETC___d6;
  tUInt32 DEF__1_CONCAT_dl_d_18_rv_port0__read_BITS_9_TO_0___d9;
  tUInt32 DEF_x__h5201;
  tUInt32 DEF_dl_d_18_rv_port0__read____d1;
  DEF_dl_d_18_rv_port0__read____d1 = INST_dl_d_18_rv.METH_port0__read();
  DEF_x__h5201 = (tUInt32)(1023u & DEF_dl_d_18_rv_port0__read____d1);
  DEF__1_CONCAT_dl_d_18_rv_port0__read_BITS_9_TO_0___d9 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h5201);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_18_rv_port0__read_BIT_10_AND_NOT_dl_d_19__ETC___d6 = (tUInt8)(DEF_dl_d_18_rv_port0__read____d1 >> 10u) && !((tUInt8)(INST_dl_d_19_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_18_rv_port0__read_BIT_10_AND_NOT_dl_d_19__ETC___d6)
    INST_dl_d_18_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_18_rv_port0__read_BIT_10_AND_NOT_dl_d_19__ETC___d6)
    INST_dl_d_19_rv.METH_port1__write(DEF__1_CONCAT_dl_d_18_rv_port0__read_BITS_9_TO_0___d9);
}

void MOD_mkDelayLine::RL_dl_try_move_1()
{
  tUInt8 DEF_dl_d_17_rv_port0__read__0_BIT_10_1_AND_NOT_dl__ETC___d15;
  tUInt32 DEF__1_CONCAT_dl_d_17_rv_port0__read__0_BITS_9_TO_0_6___d17;
  tUInt32 DEF_x__h5459;
  tUInt32 DEF_dl_d_17_rv_port0__read____d10;
  DEF_dl_d_17_rv_port0__read____d10 = INST_dl_d_17_rv.METH_port0__read();
  DEF_x__h5459 = (tUInt32)(1023u & DEF_dl_d_17_rv_port0__read____d10);
  DEF__1_CONCAT_dl_d_17_rv_port0__read__0_BITS_9_TO_0_6___d17 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h5459);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_17_rv_port0__read__0_BIT_10_1_AND_NOT_dl__ETC___d15 = (tUInt8)(DEF_dl_d_17_rv_port0__read____d10 >> 10u) && !((tUInt8)(INST_dl_d_18_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_17_rv_port0__read__0_BIT_10_1_AND_NOT_dl__ETC___d15)
    INST_dl_d_17_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_17_rv_port0__read__0_BIT_10_1_AND_NOT_dl__ETC___d15)
    INST_dl_d_18_rv.METH_port1__write(DEF__1_CONCAT_dl_d_17_rv_port0__read__0_BITS_9_TO_0_6___d17);
}

void MOD_mkDelayLine::RL_dl_try_move_2()
{
  tUInt8 DEF_dl_d_16_rv_port0__read__8_BIT_10_9_AND_NOT_dl__ETC___d23;
  tUInt32 DEF__1_CONCAT_dl_d_16_rv_port0__read__8_BITS_9_TO_0_4___d25;
  tUInt32 DEF_x__h5717;
  tUInt32 DEF_dl_d_16_rv_port0__read____d18;
  DEF_dl_d_16_rv_port0__read____d18 = INST_dl_d_16_rv.METH_port0__read();
  DEF_x__h5717 = (tUInt32)(1023u & DEF_dl_d_16_rv_port0__read____d18);
  DEF__1_CONCAT_dl_d_16_rv_port0__read__8_BITS_9_TO_0_4___d25 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h5717);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_16_rv_port0__read__8_BIT_10_9_AND_NOT_dl__ETC___d23 = (tUInt8)(DEF_dl_d_16_rv_port0__read____d18 >> 10u) && !((tUInt8)(INST_dl_d_17_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_16_rv_port0__read__8_BIT_10_9_AND_NOT_dl__ETC___d23)
    INST_dl_d_16_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_16_rv_port0__read__8_BIT_10_9_AND_NOT_dl__ETC___d23)
    INST_dl_d_17_rv.METH_port1__write(DEF__1_CONCAT_dl_d_16_rv_port0__read__8_BITS_9_TO_0_4___d25);
}

void MOD_mkDelayLine::RL_dl_try_move_3()
{
  tUInt8 DEF_dl_d_15_rv_port0__read__6_BIT_10_7_AND_NOT_dl__ETC___d31;
  tUInt32 DEF__1_CONCAT_dl_d_15_rv_port0__read__6_BITS_9_TO_0_2___d33;
  tUInt32 DEF_x__h5975;
  tUInt32 DEF_dl_d_15_rv_port0__read____d26;
  DEF_dl_d_15_rv_port0__read____d26 = INST_dl_d_15_rv.METH_port0__read();
  DEF_x__h5975 = (tUInt32)(1023u & DEF_dl_d_15_rv_port0__read____d26);
  DEF__1_CONCAT_dl_d_15_rv_port0__read__6_BITS_9_TO_0_2___d33 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h5975);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_15_rv_port0__read__6_BIT_10_7_AND_NOT_dl__ETC___d31 = (tUInt8)(DEF_dl_d_15_rv_port0__read____d26 >> 10u) && !((tUInt8)(INST_dl_d_16_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_15_rv_port0__read__6_BIT_10_7_AND_NOT_dl__ETC___d31)
    INST_dl_d_15_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_15_rv_port0__read__6_BIT_10_7_AND_NOT_dl__ETC___d31)
    INST_dl_d_16_rv.METH_port1__write(DEF__1_CONCAT_dl_d_15_rv_port0__read__6_BITS_9_TO_0_2___d33);
}

void MOD_mkDelayLine::RL_dl_try_move_4()
{
  tUInt8 DEF_dl_d_14_rv_port0__read__4_BIT_10_5_AND_NOT_dl__ETC___d39;
  tUInt32 DEF__1_CONCAT_dl_d_14_rv_port0__read__4_BITS_9_TO_0_0___d41;
  tUInt32 DEF_x__h6233;
  tUInt32 DEF_dl_d_14_rv_port0__read____d34;
  DEF_dl_d_14_rv_port0__read____d34 = INST_dl_d_14_rv.METH_port0__read();
  DEF_x__h6233 = (tUInt32)(1023u & DEF_dl_d_14_rv_port0__read____d34);
  DEF__1_CONCAT_dl_d_14_rv_port0__read__4_BITS_9_TO_0_0___d41 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h6233);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_14_rv_port0__read__4_BIT_10_5_AND_NOT_dl__ETC___d39 = (tUInt8)(DEF_dl_d_14_rv_port0__read____d34 >> 10u) && !((tUInt8)(INST_dl_d_15_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_14_rv_port0__read__4_BIT_10_5_AND_NOT_dl__ETC___d39)
    INST_dl_d_14_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_14_rv_port0__read__4_BIT_10_5_AND_NOT_dl__ETC___d39)
    INST_dl_d_15_rv.METH_port1__write(DEF__1_CONCAT_dl_d_14_rv_port0__read__4_BITS_9_TO_0_0___d41);
}

void MOD_mkDelayLine::RL_dl_try_move_5()
{
  tUInt8 DEF_dl_d_13_rv_port0__read__2_BIT_10_3_AND_NOT_dl__ETC___d47;
  tUInt32 DEF__1_CONCAT_dl_d_13_rv_port0__read__2_BITS_9_TO_0_8___d49;
  tUInt32 DEF_x__h6491;
  tUInt32 DEF_dl_d_13_rv_port0__read____d42;
  DEF_dl_d_13_rv_port0__read____d42 = INST_dl_d_13_rv.METH_port0__read();
  DEF_x__h6491 = (tUInt32)(1023u & DEF_dl_d_13_rv_port0__read____d42);
  DEF__1_CONCAT_dl_d_13_rv_port0__read__2_BITS_9_TO_0_8___d49 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h6491);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_13_rv_port0__read__2_BIT_10_3_AND_NOT_dl__ETC___d47 = (tUInt8)(DEF_dl_d_13_rv_port0__read____d42 >> 10u) && !((tUInt8)(INST_dl_d_14_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_13_rv_port0__read__2_BIT_10_3_AND_NOT_dl__ETC___d47)
    INST_dl_d_13_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_13_rv_port0__read__2_BIT_10_3_AND_NOT_dl__ETC___d47)
    INST_dl_d_14_rv.METH_port1__write(DEF__1_CONCAT_dl_d_13_rv_port0__read__2_BITS_9_TO_0_8___d49);
}

void MOD_mkDelayLine::RL_dl_try_move_6()
{
  tUInt8 DEF_dl_d_12_rv_port0__read__0_BIT_10_1_AND_NOT_dl__ETC___d55;
  tUInt32 DEF__1_CONCAT_dl_d_12_rv_port0__read__0_BITS_9_TO_0_6___d57;
  tUInt32 DEF_x__h6749;
  tUInt32 DEF_dl_d_12_rv_port0__read____d50;
  DEF_dl_d_12_rv_port0__read____d50 = INST_dl_d_12_rv.METH_port0__read();
  DEF_x__h6749 = (tUInt32)(1023u & DEF_dl_d_12_rv_port0__read____d50);
  DEF__1_CONCAT_dl_d_12_rv_port0__read__0_BITS_9_TO_0_6___d57 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h6749);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_12_rv_port0__read__0_BIT_10_1_AND_NOT_dl__ETC___d55 = (tUInt8)(DEF_dl_d_12_rv_port0__read____d50 >> 10u) && !((tUInt8)(INST_dl_d_13_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_12_rv_port0__read__0_BIT_10_1_AND_NOT_dl__ETC___d55)
    INST_dl_d_12_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_12_rv_port0__read__0_BIT_10_1_AND_NOT_dl__ETC___d55)
    INST_dl_d_13_rv.METH_port1__write(DEF__1_CONCAT_dl_d_12_rv_port0__read__0_BITS_9_TO_0_6___d57);
}

void MOD_mkDelayLine::RL_dl_try_move_7()
{
  tUInt8 DEF_dl_d_11_rv_port0__read__8_BIT_10_9_AND_NOT_dl__ETC___d63;
  tUInt32 DEF__1_CONCAT_dl_d_11_rv_port0__read__8_BITS_9_TO_0_4___d65;
  tUInt32 DEF_x__h7007;
  tUInt32 DEF_dl_d_11_rv_port0__read____d58;
  DEF_dl_d_11_rv_port0__read____d58 = INST_dl_d_11_rv.METH_port0__read();
  DEF_x__h7007 = (tUInt32)(1023u & DEF_dl_d_11_rv_port0__read____d58);
  DEF__1_CONCAT_dl_d_11_rv_port0__read__8_BITS_9_TO_0_4___d65 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h7007);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_11_rv_port0__read__8_BIT_10_9_AND_NOT_dl__ETC___d63 = (tUInt8)(DEF_dl_d_11_rv_port0__read____d58 >> 10u) && !((tUInt8)(INST_dl_d_12_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_11_rv_port0__read__8_BIT_10_9_AND_NOT_dl__ETC___d63)
    INST_dl_d_11_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_11_rv_port0__read__8_BIT_10_9_AND_NOT_dl__ETC___d63)
    INST_dl_d_12_rv.METH_port1__write(DEF__1_CONCAT_dl_d_11_rv_port0__read__8_BITS_9_TO_0_4___d65);
}

void MOD_mkDelayLine::RL_dl_try_move_8()
{
  tUInt8 DEF_dl_d_10_rv_port0__read__6_BIT_10_7_AND_NOT_dl__ETC___d71;
  tUInt32 DEF__1_CONCAT_dl_d_10_rv_port0__read__6_BITS_9_TO_0_2___d73;
  tUInt32 DEF_x__h7265;
  tUInt32 DEF_dl_d_10_rv_port0__read____d66;
  DEF_dl_d_10_rv_port0__read____d66 = INST_dl_d_10_rv.METH_port0__read();
  DEF_x__h7265 = (tUInt32)(1023u & DEF_dl_d_10_rv_port0__read____d66);
  DEF__1_CONCAT_dl_d_10_rv_port0__read__6_BITS_9_TO_0_2___d73 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h7265);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_10_rv_port0__read__6_BIT_10_7_AND_NOT_dl__ETC___d71 = (tUInt8)(DEF_dl_d_10_rv_port0__read____d66 >> 10u) && !((tUInt8)(INST_dl_d_11_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_10_rv_port0__read__6_BIT_10_7_AND_NOT_dl__ETC___d71)
    INST_dl_d_10_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_10_rv_port0__read__6_BIT_10_7_AND_NOT_dl__ETC___d71)
    INST_dl_d_11_rv.METH_port1__write(DEF__1_CONCAT_dl_d_10_rv_port0__read__6_BITS_9_TO_0_2___d73);
}

void MOD_mkDelayLine::RL_dl_try_move_9()
{
  tUInt8 DEF_dl_d_9_rv_port0__read__4_BIT_10_5_AND_NOT_dl_d_ETC___d79;
  tUInt32 DEF__1_CONCAT_dl_d_9_rv_port0__read__4_BITS_9_TO_0_0___d81;
  tUInt32 DEF_x__h7523;
  tUInt32 DEF_dl_d_9_rv_port0__read____d74;
  DEF_dl_d_9_rv_port0__read____d74 = INST_dl_d_9_rv.METH_port0__read();
  DEF_x__h7523 = (tUInt32)(1023u & DEF_dl_d_9_rv_port0__read____d74);
  DEF__1_CONCAT_dl_d_9_rv_port0__read__4_BITS_9_TO_0_0___d81 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h7523);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_9_rv_port0__read__4_BIT_10_5_AND_NOT_dl_d_ETC___d79 = (tUInt8)(DEF_dl_d_9_rv_port0__read____d74 >> 10u) && !((tUInt8)(INST_dl_d_10_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_9_rv_port0__read__4_BIT_10_5_AND_NOT_dl_d_ETC___d79)
    INST_dl_d_9_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_9_rv_port0__read__4_BIT_10_5_AND_NOT_dl_d_ETC___d79)
    INST_dl_d_10_rv.METH_port1__write(DEF__1_CONCAT_dl_d_9_rv_port0__read__4_BITS_9_TO_0_0___d81);
}

void MOD_mkDelayLine::RL_dl_try_move_10()
{
  tUInt8 DEF_dl_d_8_rv_port0__read__2_BIT_10_3_AND_NOT_dl_d_ETC___d87;
  tUInt32 DEF__1_CONCAT_dl_d_8_rv_port0__read__2_BITS_9_TO_0_8___d89;
  tUInt32 DEF_x__h7781;
  tUInt32 DEF_dl_d_8_rv_port0__read____d82;
  DEF_dl_d_8_rv_port0__read____d82 = INST_dl_d_8_rv.METH_port0__read();
  DEF_x__h7781 = (tUInt32)(1023u & DEF_dl_d_8_rv_port0__read____d82);
  DEF__1_CONCAT_dl_d_8_rv_port0__read__2_BITS_9_TO_0_8___d89 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h7781);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_8_rv_port0__read__2_BIT_10_3_AND_NOT_dl_d_ETC___d87 = (tUInt8)(DEF_dl_d_8_rv_port0__read____d82 >> 10u) && !((tUInt8)(INST_dl_d_9_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_8_rv_port0__read__2_BIT_10_3_AND_NOT_dl_d_ETC___d87)
    INST_dl_d_8_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_8_rv_port0__read__2_BIT_10_3_AND_NOT_dl_d_ETC___d87)
    INST_dl_d_9_rv.METH_port1__write(DEF__1_CONCAT_dl_d_8_rv_port0__read__2_BITS_9_TO_0_8___d89);
}

void MOD_mkDelayLine::RL_dl_try_move_11()
{
  tUInt8 DEF_dl_d_7_rv_port0__read__0_BIT_10_1_AND_NOT_dl_d_ETC___d95;
  tUInt32 DEF__1_CONCAT_dl_d_7_rv_port0__read__0_BITS_9_TO_0_6___d97;
  tUInt32 DEF_x__h8039;
  tUInt32 DEF_dl_d_7_rv_port0__read____d90;
  DEF_dl_d_7_rv_port0__read____d90 = INST_dl_d_7_rv.METH_port0__read();
  DEF_x__h8039 = (tUInt32)(1023u & DEF_dl_d_7_rv_port0__read____d90);
  DEF__1_CONCAT_dl_d_7_rv_port0__read__0_BITS_9_TO_0_6___d97 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h8039);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_7_rv_port0__read__0_BIT_10_1_AND_NOT_dl_d_ETC___d95 = (tUInt8)(DEF_dl_d_7_rv_port0__read____d90 >> 10u) && !((tUInt8)(INST_dl_d_8_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_7_rv_port0__read__0_BIT_10_1_AND_NOT_dl_d_ETC___d95)
    INST_dl_d_7_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_7_rv_port0__read__0_BIT_10_1_AND_NOT_dl_d_ETC___d95)
    INST_dl_d_8_rv.METH_port1__write(DEF__1_CONCAT_dl_d_7_rv_port0__read__0_BITS_9_TO_0_6___d97);
}

void MOD_mkDelayLine::RL_dl_try_move_12()
{
  tUInt8 DEF_dl_d_6_rv_port0__read__8_BIT_10_9_AND_NOT_dl_d_ETC___d103;
  tUInt32 DEF__1_CONCAT_dl_d_6_rv_port0__read__8_BITS_9_TO_0_04___d105;
  tUInt32 DEF_x__h8297;
  tUInt32 DEF_dl_d_6_rv_port0__read____d98;
  DEF_dl_d_6_rv_port0__read____d98 = INST_dl_d_6_rv.METH_port0__read();
  DEF_x__h8297 = (tUInt32)(1023u & DEF_dl_d_6_rv_port0__read____d98);
  DEF__1_CONCAT_dl_d_6_rv_port0__read__8_BITS_9_TO_0_04___d105 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h8297);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_6_rv_port0__read__8_BIT_10_9_AND_NOT_dl_d_ETC___d103 = (tUInt8)(DEF_dl_d_6_rv_port0__read____d98 >> 10u) && !((tUInt8)(INST_dl_d_7_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_6_rv_port0__read__8_BIT_10_9_AND_NOT_dl_d_ETC___d103)
    INST_dl_d_6_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_6_rv_port0__read__8_BIT_10_9_AND_NOT_dl_d_ETC___d103)
    INST_dl_d_7_rv.METH_port1__write(DEF__1_CONCAT_dl_d_6_rv_port0__read__8_BITS_9_TO_0_04___d105);
}

void MOD_mkDelayLine::RL_dl_try_move_13()
{
  tUInt8 DEF_dl_d_5_rv_port0__read__06_BIT_10_07_AND_NOT_dl_ETC___d111;
  tUInt32 DEF__1_CONCAT_dl_d_5_rv_port0__read__06_BITS_9_TO_0_12___d113;
  tUInt32 DEF_x__h8555;
  tUInt32 DEF_dl_d_5_rv_port0__read____d106;
  DEF_dl_d_5_rv_port0__read____d106 = INST_dl_d_5_rv.METH_port0__read();
  DEF_x__h8555 = (tUInt32)(1023u & DEF_dl_d_5_rv_port0__read____d106);
  DEF__1_CONCAT_dl_d_5_rv_port0__read__06_BITS_9_TO_0_12___d113 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h8555);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_5_rv_port0__read__06_BIT_10_07_AND_NOT_dl_ETC___d111 = (tUInt8)(DEF_dl_d_5_rv_port0__read____d106 >> 10u) && !((tUInt8)(INST_dl_d_6_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_5_rv_port0__read__06_BIT_10_07_AND_NOT_dl_ETC___d111)
    INST_dl_d_5_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_5_rv_port0__read__06_BIT_10_07_AND_NOT_dl_ETC___d111)
    INST_dl_d_6_rv.METH_port1__write(DEF__1_CONCAT_dl_d_5_rv_port0__read__06_BITS_9_TO_0_12___d113);
}

void MOD_mkDelayLine::RL_dl_try_move_14()
{
  tUInt8 DEF_dl_d_4_rv_port0__read__14_BIT_10_15_AND_NOT_dl_ETC___d119;
  tUInt32 DEF__1_CONCAT_dl_d_4_rv_port0__read__14_BITS_9_TO_0_20___d121;
  tUInt32 DEF_x__h8813;
  tUInt32 DEF_dl_d_4_rv_port0__read____d114;
  DEF_dl_d_4_rv_port0__read____d114 = INST_dl_d_4_rv.METH_port0__read();
  DEF_x__h8813 = (tUInt32)(1023u & DEF_dl_d_4_rv_port0__read____d114);
  DEF__1_CONCAT_dl_d_4_rv_port0__read__14_BITS_9_TO_0_20___d121 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h8813);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_4_rv_port0__read__14_BIT_10_15_AND_NOT_dl_ETC___d119 = (tUInt8)(DEF_dl_d_4_rv_port0__read____d114 >> 10u) && !((tUInt8)(INST_dl_d_5_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_4_rv_port0__read__14_BIT_10_15_AND_NOT_dl_ETC___d119)
    INST_dl_d_4_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_4_rv_port0__read__14_BIT_10_15_AND_NOT_dl_ETC___d119)
    INST_dl_d_5_rv.METH_port1__write(DEF__1_CONCAT_dl_d_4_rv_port0__read__14_BITS_9_TO_0_20___d121);
}

void MOD_mkDelayLine::RL_dl_try_move_15()
{
  tUInt8 DEF_dl_d_3_rv_port0__read__22_BIT_10_23_AND_NOT_dl_ETC___d127;
  tUInt32 DEF__1_CONCAT_dl_d_3_rv_port0__read__22_BITS_9_TO_0_28___d129;
  tUInt32 DEF_x__h9071;
  tUInt32 DEF_dl_d_3_rv_port0__read____d122;
  DEF_dl_d_3_rv_port0__read____d122 = INST_dl_d_3_rv.METH_port0__read();
  DEF_x__h9071 = (tUInt32)(1023u & DEF_dl_d_3_rv_port0__read____d122);
  DEF__1_CONCAT_dl_d_3_rv_port0__read__22_BITS_9_TO_0_28___d129 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h9071);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_3_rv_port0__read__22_BIT_10_23_AND_NOT_dl_ETC___d127 = (tUInt8)(DEF_dl_d_3_rv_port0__read____d122 >> 10u) && !((tUInt8)(INST_dl_d_4_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_3_rv_port0__read__22_BIT_10_23_AND_NOT_dl_ETC___d127)
    INST_dl_d_3_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_3_rv_port0__read__22_BIT_10_23_AND_NOT_dl_ETC___d127)
    INST_dl_d_4_rv.METH_port1__write(DEF__1_CONCAT_dl_d_3_rv_port0__read__22_BITS_9_TO_0_28___d129);
}

void MOD_mkDelayLine::RL_dl_try_move_16()
{
  tUInt8 DEF_dl_d_2_rv_port0__read__30_BIT_10_31_AND_NOT_dl_ETC___d135;
  tUInt32 DEF__1_CONCAT_dl_d_2_rv_port0__read__30_BITS_9_TO_0_36___d137;
  tUInt32 DEF_x__h9329;
  tUInt32 DEF_dl_d_2_rv_port0__read____d130;
  DEF_dl_d_2_rv_port0__read____d130 = INST_dl_d_2_rv.METH_port0__read();
  DEF_x__h9329 = (tUInt32)(1023u & DEF_dl_d_2_rv_port0__read____d130);
  DEF__1_CONCAT_dl_d_2_rv_port0__read__30_BITS_9_TO_0_36___d137 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h9329);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_2_rv_port0__read__30_BIT_10_31_AND_NOT_dl_ETC___d135 = (tUInt8)(DEF_dl_d_2_rv_port0__read____d130 >> 10u) && !((tUInt8)(INST_dl_d_3_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_2_rv_port0__read__30_BIT_10_31_AND_NOT_dl_ETC___d135)
    INST_dl_d_2_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_2_rv_port0__read__30_BIT_10_31_AND_NOT_dl_ETC___d135)
    INST_dl_d_3_rv.METH_port1__write(DEF__1_CONCAT_dl_d_2_rv_port0__read__30_BITS_9_TO_0_36___d137);
}

void MOD_mkDelayLine::RL_dl_try_move_17()
{
  tUInt8 DEF_dl_d_1_rv_port0__read__38_BIT_10_39_AND_NOT_dl_ETC___d143;
  tUInt32 DEF__1_CONCAT_dl_d_1_rv_port0__read__38_BITS_9_TO_0_44___d145;
  tUInt32 DEF_x__h9587;
  tUInt32 DEF_dl_d_1_rv_port0__read____d138;
  DEF_dl_d_1_rv_port0__read____d138 = INST_dl_d_1_rv.METH_port0__read();
  DEF_x__h9587 = (tUInt32)(1023u & DEF_dl_d_1_rv_port0__read____d138);
  DEF__1_CONCAT_dl_d_1_rv_port0__read__38_BITS_9_TO_0_44___d145 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h9587);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_1_rv_port0__read__38_BIT_10_39_AND_NOT_dl_ETC___d143 = (tUInt8)(DEF_dl_d_1_rv_port0__read____d138 >> 10u) && !((tUInt8)(INST_dl_d_2_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_1_rv_port0__read__38_BIT_10_39_AND_NOT_dl_ETC___d143)
    INST_dl_d_1_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_1_rv_port0__read__38_BIT_10_39_AND_NOT_dl_ETC___d143)
    INST_dl_d_2_rv.METH_port1__write(DEF__1_CONCAT_dl_d_1_rv_port0__read__38_BITS_9_TO_0_44___d145);
}

void MOD_mkDelayLine::RL_dl_try_move_18()
{
  tUInt8 DEF_dl_d_0_rv_port0__read__46_BIT_10_47_AND_NOT_dl_ETC___d151;
  tUInt32 DEF__1_CONCAT_dl_d_0_rv_port0__read__46_BITS_9_TO_0_52___d153;
  tUInt32 DEF_x__h9845;
  tUInt32 DEF_dl_d_0_rv_port0__read____d146;
  DEF_dl_d_0_rv_port0__read____d146 = INST_dl_d_0_rv.METH_port0__read();
  DEF_x__h9845 = (tUInt32)(1023u & DEF_dl_d_0_rv_port0__read____d146);
  DEF__1_CONCAT_dl_d_0_rv_port0__read__46_BITS_9_TO_0_52___d153 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_x__h9845);
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_dl_d_0_rv_port0__read__46_BIT_10_47_AND_NOT_dl_ETC___d151 = (tUInt8)(DEF_dl_d_0_rv_port0__read____d146 >> 10u) && !((tUInt8)(INST_dl_d_1_rv.METH_port1__read() >> 10u));
  if (DEF_dl_d_0_rv_port0__read__46_BIT_10_47_AND_NOT_dl_ETC___d151)
    INST_dl_d_0_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (DEF_dl_d_0_rv_port0__read__46_BIT_10_47_AND_NOT_dl_ETC___d151)
    INST_dl_d_1_rv.METH_port1__write(DEF__1_CONCAT_dl_d_0_rv_port0__read__46_BITS_9_TO_0_52___d153);
}

void MOD_mkDelayLine::RL_tic()
{
  tUInt32 DEF_x__h9991;
  tUInt32 DEF__read__h9962;
  DEF__read__h9962 = INST_ctime.METH_read();
  DEF_x__h9991 = DEF__read__h9962 + 1u;
  INST_ctime.METH_write(DEF_x__h9991);
  if (!(PORT_RST_N == (tUInt8)0u))
    dollar_display(sim_hdl, this, "s,32", &__str_literal_1, DEF__read__h9962);
}

void MOD_mkDelayLine::RL_feed()
{
  tUInt32 DEF_x__h10032;
  tUInt32 DEF__1_CONCAT_cnt_f_59___d163;
  DEF_cnt_f__h10151 = INST_cnt_f.METH_read();
  DEF__1_CONCAT_cnt_f_59___d163 = 2047u & ((((tUInt32)((tUInt8)1u)) << 10u) | DEF_cnt_f__h10151);
  DEF_x__h10032 = 1023u & (DEF_cnt_f__h10151 + 1u);
  INST_cnt_f.METH_write(DEF_x__h10032);
  INST_dl_d_0_rv.METH_port1__write(DEF__1_CONCAT_cnt_f_59___d163);
}

void MOD_mkDelayLine::RL_stream()
{
  tUInt32 DEF_x__h10332;
  tUInt8 DEF_cnt_s_68_EQ_9___d170;
  tUInt32 DEF_v__h10162;
  tUInt32 DEF_x__h10289;
  tUInt32 DEF_x__h10351;
  DEF_dl_d_19_rv_port0__read____d164 = INST_dl_d_19_rv.METH_port0__read();
  DEF_x__h10351 = INST_cnt_s.METH_read();
  DEF_x__h10289 = (tUInt32)(1023u & DEF_dl_d_19_rv_port0__read____d164);
  DEF_v__h10162 = DEF_x__h10289;
  DEF_cnt_s_68_EQ_9___d170 = DEF_x__h10351 == 9u;
  DEF__0_CONCAT_DONTCARE___d7 = 682u;
  DEF_x__h10332 = 1023u & (DEF_x__h10351 + 1u);
  INST_dl_d_19_rv.METH_port0__write(DEF__0_CONCAT_DONTCARE___d7);
  if (!(PORT_RST_N == (tUInt8)0u))
    dollar_display(sim_hdl, this, "s,10", &__str_literal_2, DEF_v__h10162);
  INST_cnt_s.METH_write(DEF_x__h10332);
  if (!(PORT_RST_N == (tUInt8)0u))
    if (DEF_cnt_s_68_EQ_9___d170)
      dollar_finish(sim_hdl, "32", 0u);
}


/* Methods */


/* Reset routines */

void MOD_mkDelayLine::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
  INST_dl_d_9_rv.reset_RST(ARG_rst_in);
  INST_dl_d_8_rv.reset_RST(ARG_rst_in);
  INST_dl_d_7_rv.reset_RST(ARG_rst_in);
  INST_dl_d_6_rv.reset_RST(ARG_rst_in);
  INST_dl_d_5_rv.reset_RST(ARG_rst_in);
  INST_dl_d_4_rv.reset_RST(ARG_rst_in);
  INST_dl_d_3_rv.reset_RST(ARG_rst_in);
  INST_dl_d_2_rv.reset_RST(ARG_rst_in);
  INST_dl_d_1_rv.reset_RST(ARG_rst_in);
  INST_dl_d_19_rv.reset_RST(ARG_rst_in);
  INST_dl_d_18_rv.reset_RST(ARG_rst_in);
  INST_dl_d_17_rv.reset_RST(ARG_rst_in);
  INST_dl_d_16_rv.reset_RST(ARG_rst_in);
  INST_dl_d_15_rv.reset_RST(ARG_rst_in);
  INST_dl_d_14_rv.reset_RST(ARG_rst_in);
  INST_dl_d_13_rv.reset_RST(ARG_rst_in);
  INST_dl_d_12_rv.reset_RST(ARG_rst_in);
  INST_dl_d_11_rv.reset_RST(ARG_rst_in);
  INST_dl_d_10_rv.reset_RST(ARG_rst_in);
  INST_dl_d_0_rv.reset_RST(ARG_rst_in);
  INST_ctime.reset_RST(ARG_rst_in);
  INST_cnt_s.reset_RST(ARG_rst_in);
  INST_cnt_f.reset_RST(ARG_rst_in);
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkDelayLine::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkDelayLine::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST_cnt_f.dump_state(indent + 2u);
  INST_cnt_s.dump_state(indent + 2u);
  INST_ctime.dump_state(indent + 2u);
  INST_dl_d_0_rv.dump_state(indent + 2u);
  INST_dl_d_10_rv.dump_state(indent + 2u);
  INST_dl_d_11_rv.dump_state(indent + 2u);
  INST_dl_d_12_rv.dump_state(indent + 2u);
  INST_dl_d_13_rv.dump_state(indent + 2u);
  INST_dl_d_14_rv.dump_state(indent + 2u);
  INST_dl_d_15_rv.dump_state(indent + 2u);
  INST_dl_d_16_rv.dump_state(indent + 2u);
  INST_dl_d_17_rv.dump_state(indent + 2u);
  INST_dl_d_18_rv.dump_state(indent + 2u);
  INST_dl_d_19_rv.dump_state(indent + 2u);
  INST_dl_d_1_rv.dump_state(indent + 2u);
  INST_dl_d_2_rv.dump_state(indent + 2u);
  INST_dl_d_3_rv.dump_state(indent + 2u);
  INST_dl_d_4_rv.dump_state(indent + 2u);
  INST_dl_d_5_rv.dump_state(indent + 2u);
  INST_dl_d_6_rv.dump_state(indent + 2u);
  INST_dl_d_7_rv.dump_state(indent + 2u);
  INST_dl_d_8_rv.dump_state(indent + 2u);
  INST_dl_d_9_rv.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkDelayLine::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 27u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "_0_CONCAT_DONTCARE___d7", 11u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "cnt_f__h10151", 10u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "dl_d_19_rv_port0__read____d164", 11u);
  num = INST_cnt_f.dump_VCD_defs(num);
  num = INST_cnt_s.dump_VCD_defs(num);
  num = INST_ctime.dump_VCD_defs(num);
  num = INST_dl_d_0_rv.dump_VCD_defs(num);
  num = INST_dl_d_10_rv.dump_VCD_defs(num);
  num = INST_dl_d_11_rv.dump_VCD_defs(num);
  num = INST_dl_d_12_rv.dump_VCD_defs(num);
  num = INST_dl_d_13_rv.dump_VCD_defs(num);
  num = INST_dl_d_14_rv.dump_VCD_defs(num);
  num = INST_dl_d_15_rv.dump_VCD_defs(num);
  num = INST_dl_d_16_rv.dump_VCD_defs(num);
  num = INST_dl_d_17_rv.dump_VCD_defs(num);
  num = INST_dl_d_18_rv.dump_VCD_defs(num);
  num = INST_dl_d_19_rv.dump_VCD_defs(num);
  num = INST_dl_d_1_rv.dump_VCD_defs(num);
  num = INST_dl_d_2_rv.dump_VCD_defs(num);
  num = INST_dl_d_3_rv.dump_VCD_defs(num);
  num = INST_dl_d_4_rv.dump_VCD_defs(num);
  num = INST_dl_d_5_rv.dump_VCD_defs(num);
  num = INST_dl_d_6_rv.dump_VCD_defs(num);
  num = INST_dl_d_7_rv.dump_VCD_defs(num);
  num = INST_dl_d_8_rv.dump_VCD_defs(num);
  num = INST_dl_d_9_rv.dump_VCD_defs(num);
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkDelayLine::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkDelayLine &backing)
{
  vcd_defs(dt, backing);
  vcd_prims(dt, backing);
}

void MOD_mkDelayLine::vcd_defs(tVCDDumpType dt, MOD_mkDelayLine &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 11u);
    vcd_write_x(sim_hdl, num++, 10u);
    vcd_write_x(sim_hdl, num++, 11u);
  }
  else
    if (dt == VCD_DUMP_CHANGES)
    {
      if ((backing.PORT_RST_N) != PORT_RST_N)
      {
	vcd_write_val(sim_hdl, num, PORT_RST_N, 1u);
	backing.PORT_RST_N = PORT_RST_N;
      }
      ++num;
      if ((backing.DEF__0_CONCAT_DONTCARE___d7) != DEF__0_CONCAT_DONTCARE___d7)
      {
	vcd_write_val(sim_hdl, num, DEF__0_CONCAT_DONTCARE___d7, 11u);
	backing.DEF__0_CONCAT_DONTCARE___d7 = DEF__0_CONCAT_DONTCARE___d7;
      }
      ++num;
      if ((backing.DEF_cnt_f__h10151) != DEF_cnt_f__h10151)
      {
	vcd_write_val(sim_hdl, num, DEF_cnt_f__h10151, 10u);
	backing.DEF_cnt_f__h10151 = DEF_cnt_f__h10151;
      }
      ++num;
      if ((backing.DEF_dl_d_19_rv_port0__read____d164) != DEF_dl_d_19_rv_port0__read____d164)
      {
	vcd_write_val(sim_hdl, num, DEF_dl_d_19_rv_port0__read____d164, 11u);
	backing.DEF_dl_d_19_rv_port0__read____d164 = DEF_dl_d_19_rv_port0__read____d164;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, PORT_RST_N, 1u);
      backing.PORT_RST_N = PORT_RST_N;
      vcd_write_val(sim_hdl, num++, DEF__0_CONCAT_DONTCARE___d7, 11u);
      backing.DEF__0_CONCAT_DONTCARE___d7 = DEF__0_CONCAT_DONTCARE___d7;
      vcd_write_val(sim_hdl, num++, DEF_cnt_f__h10151, 10u);
      backing.DEF_cnt_f__h10151 = DEF_cnt_f__h10151;
      vcd_write_val(sim_hdl, num++, DEF_dl_d_19_rv_port0__read____d164, 11u);
      backing.DEF_dl_d_19_rv_port0__read____d164 = DEF_dl_d_19_rv_port0__read____d164;
    }
}

void MOD_mkDelayLine::vcd_prims(tVCDDumpType dt, MOD_mkDelayLine &backing)
{
  INST_cnt_f.dump_VCD(dt, backing.INST_cnt_f);
  INST_cnt_s.dump_VCD(dt, backing.INST_cnt_s);
  INST_ctime.dump_VCD(dt, backing.INST_ctime);
  INST_dl_d_0_rv.dump_VCD(dt, backing.INST_dl_d_0_rv);
  INST_dl_d_10_rv.dump_VCD(dt, backing.INST_dl_d_10_rv);
  INST_dl_d_11_rv.dump_VCD(dt, backing.INST_dl_d_11_rv);
  INST_dl_d_12_rv.dump_VCD(dt, backing.INST_dl_d_12_rv);
  INST_dl_d_13_rv.dump_VCD(dt, backing.INST_dl_d_13_rv);
  INST_dl_d_14_rv.dump_VCD(dt, backing.INST_dl_d_14_rv);
  INST_dl_d_15_rv.dump_VCD(dt, backing.INST_dl_d_15_rv);
  INST_dl_d_16_rv.dump_VCD(dt, backing.INST_dl_d_16_rv);
  INST_dl_d_17_rv.dump_VCD(dt, backing.INST_dl_d_17_rv);
  INST_dl_d_18_rv.dump_VCD(dt, backing.INST_dl_d_18_rv);
  INST_dl_d_19_rv.dump_VCD(dt, backing.INST_dl_d_19_rv);
  INST_dl_d_1_rv.dump_VCD(dt, backing.INST_dl_d_1_rv);
  INST_dl_d_2_rv.dump_VCD(dt, backing.INST_dl_d_2_rv);
  INST_dl_d_3_rv.dump_VCD(dt, backing.INST_dl_d_3_rv);
  INST_dl_d_4_rv.dump_VCD(dt, backing.INST_dl_d_4_rv);
  INST_dl_d_5_rv.dump_VCD(dt, backing.INST_dl_d_5_rv);
  INST_dl_d_6_rv.dump_VCD(dt, backing.INST_dl_d_6_rv);
  INST_dl_d_7_rv.dump_VCD(dt, backing.INST_dl_d_7_rv);
  INST_dl_d_8_rv.dump_VCD(dt, backing.INST_dl_d_8_rv);
  INST_dl_d_9_rv.dump_VCD(dt, backing.INST_dl_d_9_rv);
}
