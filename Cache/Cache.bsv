import BRAM::*;
import FIFO::*;
import SpecialFIFOs::*;
import MemTypes::*;
import Vector::*;
import Ehr::*;

//typedef Bit#(128) NumLines;
typedef Bit#(19) TagSize;
typedef Bit#(7) IdxSize;
typedef Bit#(4) oSize;
typedef enum {Ready, StartMiss_BRAMReq, StartMiss_BRAMResp, SendFillReq, WaitFillResp, HitQ} ReqStatus deriving (Bits, Eq);


interface Cache;
    method Action putFromProc(MainMemReq req);
    method ActionValue#(MainMemResp) getToProc();
    method ActionValue#(MainMemReq) getToMem();
    method Action putFromMem(MainMemResp resp);
endinterface

module mkCache(Cache);
  BRAM_Configure cfg = defaultValue();
  BRAM1Port#(IdxSize, MainMemResp) cache_data <- mkBRAM1Server(cfg);

  Vector#(128, Reg#(TagSize)) tagArray <- replicateM(mkReg(0));
  Vector#(128, Reg#(Bool)) validArray <- replicateM(mkReg(False));
  Vector#(128, Reg#(Bool)) dirtyArray <- replicateM(mkReg(False));

  Reg#(MainMemReq) missReq <- mkRegU;
  Reg#(ReqStatus) mshr <- mkReg(Ready);

  FIFO#(MainMemReq) fromProcQ <- mkFIFO;
  FIFO#(Word) hitQ <- mkBypassFIFO;
  FIFO#(MainMemReq) memReqQ <- mkFIFO;
  FIFO#(MainMemResp) memRespQ <- mkFIFO; 

  FIFO#(MainMemReq) storeQ <- mkSizedFIFO(1);

  Reg#(Bit#(32)) hitCount <- mkReg(0);
  Reg#(Bit#(32)) missCount <- mkReg(0);

  Ehr#(2, Bool) lockL1 <- mkEhr(False);

  rule bram_to_hitQ if (mshr == HitQ);
    let data <- cache_data.portA.response.get();
    // get offset
    hitQ.enq(data);
    mshr <= Ready;

  endrule


  rule startMiss_BRAMReq if (mshr == StartMiss_BRAMReq);
    let req_idx = missReq.addr[6:0];
    let old_line_valid = validArray[req_idx];
    let old_line_dirty = dirtyArray[req_idx];

    if (old_line_dirty && old_line_valid) begin    //the old cache line is dirty and must be written back to memory
      cache_data.portA.request.put(BRAMRequest{write: False,   //read the current data at the cache line
                         responseOnWrite: False,
                         address: req_idx,
                         datain: ?});

      mshr <= StartMiss_BRAMResp;
    end

    else mshr <= SendFillReq;    //if the old cache line is not dirty, then no writeback needed

  endrule


  rule startMiss_BRAMResp if (mshr == StartMiss_BRAMResp);
    let old_data <- cache_data.portA.response.get();  //get old data from the cache line

    let req_idx = missReq.addr[6:0];
    let old_tag = tagArray[req_idx];
    Bit#(26) old_addr = {old_tag, req_idx};   //concat tag and idx together to get the address

    memReqQ.enq(MainMemReq {write: 1,              //writeback old cache line to memory
                addr: old_addr,
                data: old_data});

    mshr <= SendFillReq;

  endrule

  rule sendFillReq if (mshr == SendFillReq);
    memReqQ.enq(MainMemReq {write: 0,              //load new line from memory
                addr: missReq.addr,
                data: ?});

    mshr <= WaitFillResp;
  endrule


  rule waitFillResp if (mshr == WaitFillResp);
    memRespQ.deq();
    let mem_data = memRespQ.first();

    let req_store = missReq.write;
    let req_idx = missReq.addr[6:0];
    let req_tag = missReq.addr[25:7];
    let req_data = missReq.data;

    tagArray[req_idx] <= req_tag;
    validArray[req_idx] <= True;

    if (req_store == 1)  begin  //store instruction
      dirtyArray[req_idx] <= True;
      cache_data.portA.request.put(BRAMRequest{write: True,   //write new data to cache
                         responseOnWrite: False,
                         address: req_idx,
                         datain: req_data});
    end
    else begin     //load instruction
      dirtyArray[req_idx] <= False;
      cache_data.portA.request.put(BRAMRequest{write: True,   //write new data to cache
                         responseOnWrite: False,
                         address: req_idx,
                         datain: mem_data});
      hitQ.enq(mem_data);
    end

    mshr <= Ready;

  endrule


  rule storeQ_handler if (mshr == Ready && !lockL1[1]);
    storeQ.deq();
    let req = storeQ.first();

    let req_addr = req.addr;
    let req_data = req.data;
    let req_idx = req_addr[6:0];
    let req_tag = req_addr[25:7];
    let cur_tag = tagArray[req_idx];
    let cur_valid = validArray[req_idx];

    //$display("Tag: %d, Idx: %d", req_tag, req_idx);

    if (cur_tag == req_tag && cur_valid) begin  //cache hit
      hitCount <= hitCount + 1;
      dirtyArray[req_idx] <= True;    //update dirty array
      cache_data.portA.request.put(BRAMRequest{write: True,   //write new data to cache
                        responseOnWrite: False,
                        address: req_idx,
                        datain: req_data});
    end
    else begin
      missCount <= missCount + 1;
      mshr <= StartMiss_BRAMReq;
      missReq <= req;
    end

  endrule

  rule displayPercents;
    if (missCount == 100) begin
      $display("Misses: %d Hits: %d", missCount, hitCount);
    end
  endrule

  rule clearL1Lock; lockL1[1] <= False; endrule

  method Action putFromProc(MainMemReq req) if (mshr == Ready);
    let req_store = req.write; //1 if store, 0 if load

    let req_addr = req.addr;
    let req_data = req.data;
    let req_idx = req_addr[6:0];
    let req_tag = req_addr[25:7];
    let cur_tag = tagArray[req_idx];
    let cur_valid = validArray[req_idx];
    //$display("Load/Store: %d, Tag: %d, Idx: %d", req_store, req_tag, req_idx);

    if (req_store == 1) begin //store instruction
        storeQ.enq(req);
    end
    else begin     //load instruction
      lockL1[0] <= True;    //lock L1 so that store buffer does not access it
      let storeQ_req = storeQ.first();
      if (storeQ_req.addr == req_addr)  begin   //hit in store Q
          //$display("here");
          hitCount <= hitCount + 1;
          let storeQ_data = storeQ_req.data;
          hitQ.enq(storeQ_data);
      end
      else if (cur_tag == req_tag && cur_valid) begin  //cache hit 
        //$display("Cache Hit");
        hitCount <= hitCount + 1;
        cache_data.portA.request.put(BRAMRequest{write: False,   //read data from cache
                         responseOnWrite: False,
                         address: req_idx,
                         datain: ?});
        mshr <= HitQ;
      end
      else begin     //cache miss
        //$display("Cache Miss");
        missCount <= missCount + 1;
        mshr <= StartMiss_BRAMReq;
        missReq <= req;
      end
    end
    
  endmethod

  method ActionValue#(MainMemResp) getToProc();
    hitQ.deq();
    return hitQ.first();
  endmethod

  method ActionValue#(MainMemReq) getToMem();
    memReqQ.deq();
    return memReqQ.first();
  endmethod

  method Action putFromMem(MainMemResp resp);
    memRespQ.enq(resp);
  endmethod


endmodule
