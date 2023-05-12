import RVUtil::*;
import BRAM::*;
import FIFO::*;
import SpecialFIFOs::*;
import DelayLine::*;
import MemTypes::*;
import Cache::*;

// interface MainMem;
//     method Action put(MainMemReq req);
//     method ActionValue#(MainMemResp) get();
// endinterface

// interface MainMemFast;
//     method Action put(ProcReq req);
//     method ActionValue#(Word) get();
// endinterface

interface MainMem2;
    method Action put1(MainMemReq req);
    method Action put2(MainMemReq req);
    method ActionValue#(MainMemResp) get1();
    method ActionValue#(MainMemResp) get2();
endinterface
// interface MainMemFPGA;
//     method Action put1(MainMemReq req);
//     method Action put2(MainMemReq req);
//     method ActionValue#(MainMemResp) get1();
//     method ActionValue#(MainMemResp) get2();
// endinterface

//typedef struct {Bool icache, MainMemReq req} MainMemFPGAReq deriving (Eq, FShow, Bits);

// module mkMainMemFast(MainMemFast);
//     BRAM_Configure cfg = defaultValue();
//     BRAM1Port#(Word, Bit#(32)) bram <- mkBRAM1Server(cfg);
//     DelayLine#(1, Word) dl <- mkDL(); // Delay by 20 cycles

//     rule deq;
//         let r <- bram.portA.response.get();
//         dl.put(r);
//     endrule    

//     method Action put(ProcReq req);
//         bram.portA.request.put(BRAMRequest{
//                     write: unpack(req.write),
//                     responseOnWrite: False,
//                     address: req.addr,
//                     datain: req.data});
//     endmethod

//     method ActionValue#(Word) get();
//         let r <- dl.get();
//         return r;
//     endmethod
// endmodule

// module mkMainMem(MainMem);
//     BRAM_Configure cfg = defaultValue();
//     BRAM1Port#(LineAddr, Bit#(512)) bram <- mkBRAM1Server(cfg);
//     DelayLine#(40, MainMemResp) dl <- mkDL(); // Delay by 20 cycles

//     rule deq;
//         let r <- bram.portA.response.get();
//         dl.put(r);
//     endrule    

//     method Action put(MainMemReq req);
//         bram.portA.request.put(BRAMRequest{
//                     write: unpack(req.write),
//                     responseOnWrite: False,
//                     address: req.addr,
//                     datain: req.data});
//     endmethod

//     method ActionValue#(MainMemResp) get();
//         let r <- dl.get();
//         return r;
//     endmethod
// endmodule

module mkMainMem2(MainMem2);
    BRAM_Configure cfg = defaultValue();
    cfg.loadFormat = tagged Hex "mem.vmh";
    BRAM2Port#(LineAddr, MainMemResp) bram <- mkBRAM2Server(cfg);
    DelayLine#(4, MainMemResp) dl1 <- mkDL(); // Delay by 2 cycles
    DelayLine#(4, MainMemResp) dl2 <- mkDL(); // Delay by 2 cycles
    
    rule deq1;
        let r <- bram.portA.response.get();
        dl1.put(r);
    endrule 
    rule deq2;
        let r <- bram.portB.response.get();
        dl2.put(r);
    endrule 

    method Action put1(MainMemReq req);
        bram.portA.request.put(BRAMRequest{
            write: unpack(req.write),
            responseOnWrite: False,
            address: req.addr,
            datain: req.data
        });
    endmethod

    method Action put2(MainMemReq req);
        bram.portB.request.put(BRAMRequest{
            write: unpack(req.write),
            responseOnWrite: False,
            address: req.addr,
            datain: req.data
        });
    endmethod

    method ActionValue#(MainMemResp) get1();
        let r <- dl1.get();
        return r;
    endmethod

    method ActionValue#(MainMemResp) get2();
        let r <- dl2.get();
        return r;
    endmethod
endmodule
/*
module mkMainMemFPGA(MainMemFPGA);
    BRAM_Configure cfg = defaultValue();
    cfg.loadFormat = tagged Hex "mem.vmh";
    BRAM1Port#(LineAddr, MainMemResp) bram <- mkBRAM21erver(cfg);
    DelayLine#(40, MainMemResp) dl1 <- mkDL(); // Delay by 20 cycles
    
    FIFO#(MainMemFPGAReq) bQueue<- mkFIFO;

    rule deq1;
        let r <- bram.portA.response.get();
        dl1.put(r);
    endrule 
    
    method Action put1(MainMemReq req);
        bQueue.enq(MainMemFPGAReq{icache: });
        bram.portA.request.put(BRAMRequest{
            write: unpack(req.write),
            responseOnWrite: False,
            address: req.addr,
            datain: req.data
        });
    endmethod

    method Action put2(MainMemReq req);
        bQueue.enq(req);
        bram.portA.request.put(BRAMRequest{
            write: unpack(req.write),
            responseOnWrite: False,
            address: req.addr,
            datain: req.data
        });
    endmethod

    method ActionValue#(MainMemResp) get();
        let r <- dl1.get();
        if 
        return r;
    endmethod

    
endmodule */