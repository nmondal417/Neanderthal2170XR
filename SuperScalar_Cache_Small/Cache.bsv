import BRAM::*;
import FIFO::*;
import SpecialFIFOs::*;
import MemTypes::*;
import Vector::*;
import Ehr::*;

typedef Bit#(24) TagSize;
typedef Bit#(4) IdxSize;
typedef Bit#(2) OffsetSize;
typedef enum {Ready, StartMiss_BRAMReq, StartMiss_BRAMResp, SendFillReq, WaitFillResp, HitQ, WaitStore} ReqStatus deriving (Bits, Eq);

interface Cache1;
    method Action putFromProc(ProcReq req);
    method ActionValue#(Word) getToProc();
    method ActionValue#(MainMemReq) getToMem();
    method Action putFromMem(MainMemResp resp);
endinterface

module mkCache1(Cache1);
  BRAM_Configure cfg = defaultValue();
  BRAM1Port#(IdxSize, Vector#(4, Word)) cache_data <- mkBRAM1Server(cfg);  //each cache line of data is 4 32-bit words (128 bits)

  Vector#(16, Reg#(TagSize)) tagArray <- replicateM(mkReg(0));
  Vector#(16, Reg#(Bool)) validArray <- replicateM(mkReg(False));
  Vector#(16, Reg#(Bool)) dirtyArray <- replicateM(mkReg(False));

  Reg#(ProcReq) missReq <- mkRegU;
  Ehr#(2, ReqStatus) mshr <- mkEhr(Ready);

  FIFO#(Word) hitQ <- mkBypassFIFO;
  FIFO#(OffsetSize) loadOffsetQ <- mkFIFO;
  FIFO#(MainMemReq) memReqQ <- mkFIFO;
  FIFO#(MainMemResp) memRespQ <- mkFIFO; 

  FIFO#(ProcReq) storeQ <- mkSizedFIFO(1);

  Reg#(Bit#(32)) hitCount <- mkReg(0);
  Reg#(Bit#(32)) missCount <- mkReg(0);

  Ehr#(2, Bool) lockL1 <- mkEhr(False);

  rule bram_to_hitQ if (mshr[0] == HitQ);
    Vector#(4, Word) line <- cache_data.portA.response.get();
    //$display("Line: ", fshow(line));
    let req_offset = loadOffsetQ.first();
    loadOffsetQ.deq();
    Word data = line[req_offset];
    //$display("Return data: ", fshow(data));
    hitQ.enq(data);
    mshr[0] <= Ready;
  endrule


  rule startMiss_BRAMReq if (mshr[0] == StartMiss_BRAMReq);
    let req_idx = missReq.addr[7:4];
    let old_line_valid = validArray[req_idx];
    let old_line_dirty = dirtyArray[req_idx];

    if (old_line_dirty && old_line_valid) begin    //the old cache line is dirty and must be written back to memory
      cache_data.portA.request.put(BRAMRequest{write: False,   //read the current cache line
                         responseOnWrite: False,
                         address: req_idx,
                         datain: ?});

      mshr[0] <= StartMiss_BRAMResp;
    end

    else mshr[0] <= SendFillReq;    //if the old cache line is not dirty, then no writeback needed

  endrule


  rule startMiss_BRAMResp if (mshr[0] == StartMiss_BRAMResp);
    Vector#(4, Word) old_line <- cache_data.portA.response.get();  //get old cache line
    MainMemResp old_data = pack(old_line);   //convert vector of 4 words into 128 bits

    let req_idx = missReq.addr[7:4];
    let old_tag = tagArray[req_idx];
    LineAddr old_addr = {old_tag, req_idx};   //concat tag and idx together to get the address

    memReqQ.enq(MainMemReq {write: 1,              //writeback old cache line to memory
                addr: old_addr,
                data: old_data});

    mshr[0] <= SendFillReq;

  endrule

  rule sendFillReq if (mshr[0] == SendFillReq);
    memReqQ.enq(MainMemReq {write: 0,              //load new line from memory
                addr: missReq.addr[31:4],          //take top 28 bits (the line address)
                data: ?});

    mshr[0] <= WaitFillResp;
  endrule


  rule waitFillResp if (mshr[0] == WaitFillResp);
    memRespQ.deq();
    MainMemResp mem_data = memRespQ.first();
    //$display("Mem Resp ", mem_data);
    Vector#(4, Word) new_line = unpack(mem_data); //unpack the 128-bit response into a vector of 4 words
    //$display("New Line ", fshow(new_line));
    let req_store = missReq.write;
    let req_offset = missReq.addr[3:2];
    let req_idx = missReq.addr[7:4];
    let req_tag = missReq.addr[31:8];
    let req_data = missReq.data;

    tagArray[req_idx] <= req_tag;
    validArray[req_idx] <= True;

    if (req_store == 1)  begin  //store instruction
      dirtyArray[req_idx] <= True;
      new_line[req_offset] = req_data;   //update the data at the specific word
      cache_data.portA.request.put(BRAMRequest{write: True,   //write new data to cache
                         responseOnWrite: False,
                         address: req_idx,
                        datain: new_line});
      //hitQ.enq(0);
    end
    else begin     //load instruction

      //$display("Offset: ", fshow(req_offset));
      //$display("Idx: ", fshow(req_idx));
      //$display("Tag: ", fshow(req_tag));
      //$display("New Line: ", fshow(new_line));

      dirtyArray[req_idx] <= False;
      cache_data.portA.request.put(BRAMRequest{write: True,   //write new data to cache
                         responseOnWrite: False,
                         address: req_idx,
                         datain: new_line});

      Word return_data = new_line[req_offset];
      //$display("Return data: ", fshow(return_data));
      hitQ.enq(return_data);
    end

    mshr[0] <= Ready;

  endrule

/*
  rule storeQ_handler if (mshr == Ready && !lockL1[1]);
    ProcReq req = storeQ.first();

    let req_addr = req.addr;
    let req_data = req.data;
    let req_idx = req_addr[12:6];
    let req_tag = req_addr[31:13];
    let cur_tag = tagArray[req_idx];
    let cur_valid = validArray[req_idx];

    //$display("Tag: %d, Idx: %d", req_tag, req_idx);

    if (cur_tag == req_tag && cur_valid) begin  //cache hit
      hitCount <= hitCount + 1;
      dirtyArray[req_idx] <= True;    //update dirty array
      cache_data.portA.request.put(BRAMRequest{write: False,   //we need to first read the entire line before writing the word
                        responseOnWrite: False,
                        address: req_idx,
                        datain: ?});
      mshr <= WaitStore;      //don't deq StoreQ yet since we need the request data in the waitStore rule
    end
    else begin
      missCount <= missCount + 1;
      mshr <= StartMiss_BRAMReq;
      missReq <= req;
      storeQ.deq();
    end

  endrule */

  rule waitStore if (mshr[0] == WaitStore);
    Vector#(4, Word) line <- cache_data.portA.response.get();
    ProcReq req = storeQ.first();

    Word req_data = req.data;
    Word req_addr = req.addr;
    let req_idx = req_addr[7:4];
    let req_offset = req_addr[3:2];

    line[req_offset] = req_data;

    cache_data.portA.request.put(BRAMRequest{write: True,   //write line back to cache
                        responseOnWrite: False,
                        address: req_idx,
                        datain: line});
    
    storeQ.deq();
    //hitQ.enq(0);
    mshr[0] <= Ready;
  endrule  
/*
  rule displayPercents;
    if (missCount == 100) begin
      $display("Misses: %d Hits: %d", missCount, hitCount);
    end
  endrule
*/
  //rule clearL1Lock; lockL1[1] <= False; endrule

  method Action putFromProc(ProcReq req) if (mshr[1] == Ready);
    let req_store = req.write; //1 if store, 0 if load
    Word req_addr = req.addr;
    Word req_data = req.data;

    let req_offset = req_addr[3:2];
    let req_idx = req_addr[7:4];
    let req_tag = req_addr[31:8];
    let cur_tag = tagArray[req_idx];
    let cur_valid = validArray[req_idx];
    //$display("Load/Store: %d, Tag: %d, Idx: %d, Offset: %d", req_store, req_tag, req_idx, req_offset);

    if (req_store == 1) begin //store instruction
      if (cur_tag == req_tag && cur_valid) begin  //cache hit
        //$display("Cache Store Hit");
        hitCount <= hitCount + 1;
        dirtyArray[req_idx] <= True;    //update dirty array
        cache_data.portA.request.put(BRAMRequest{write: False,   //we need to first read the entire line before writing the word
                          responseOnWrite: False,
                          address: req_idx,
                          datain: ?});
        mshr[1] <= WaitStore;
        storeQ.enq(req);
      end
      else begin
        //$display("Cache Store Miss");
        missCount <= missCount + 1;
        mshr[1] <= StartMiss_BRAMReq;
        missReq <= req;
      end
    end

    else begin     //load instruction
    /*
      lockL1[0] <= True;    //lock L1 so that store buffer does not access it
      ProcReq storeQ_req = storeQ.first();
      if (storeQ_req.addr == req_addr)  begin   //hit in store Q
          //$display("here");
          hitCount <= hitCount + 1;
          Word storeQ_data = storeQ_req.data;
          hitQ.enq(storeQ_data);
      end
      */
      if (cur_tag == req_tag && cur_valid) begin  //cache hit 
        //$display("Cache Load Hit");
        hitCount <= hitCount + 1;
        cache_data.portA.request.put(BRAMRequest{write: False,   //read corresponding line (128 bits) from cache
                         responseOnWrite: False,
                         address: req_idx,
                         datain: ?});

        loadOffsetQ.enq(req_offset);   //store the line offset so that it can be used later

        mshr[1] <= HitQ;
      end
      else begin     //cache miss
        //$display("Cache Load Miss");
        missCount <= missCount + 1;
        mshr[1] <= StartMiss_BRAMReq;
        missReq <= req;
      end
    end
    
  endmethod

  method ActionValue#(Word) getToProc();
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