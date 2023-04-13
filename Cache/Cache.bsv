import BRAM::*;
import FIFO::*;
import SpecialFIFOs::*;
import MemTypes::*;
import Vector::*;
import Ehr::*;

typedef Bit#(19) TagSize;
typedef Bit#(7) IdxSize;
typedef Bit#(4) OffsetSize;
typedef enum {Ready, StartMiss_BRAMReq, StartMiss_BRAMResp, SendFillReq, WaitFillResp, HitQ, WaitStore} ReqStatus deriving (Bits, Eq);


interface Cache;
    method Action putFromProc(ProcReq req);
    method ActionValue#(Word) getToProc();
    method ActionValue#(MainMemReq) getToMem();
    method Action putFromMem(MainMemResp resp);
endinterface

module mkCache(Cache);
  BRAM_Configure cfg = defaultValue();
  BRAM1Port#(IdxSize, Vector#(16, Word)) cache_data <- mkBRAM1Server(cfg);  //each cache line of data is 16 32-bit words (512 bits)

  Vector#(128, Reg#(TagSize)) tagArray <- replicateM(mkReg(0));
  Vector#(128, Reg#(Bool)) validArray <- replicateM(mkReg(False));
  Vector#(128, Reg#(Bool)) dirtyArray <- replicateM(mkReg(False));

  Reg#(ProcReq) missReq <- mkRegU;
  Reg#(ReqStatus) mshr <- mkReg(Ready);

  FIFO#(Word) hitQ <- mkBypassFIFO;
  FIFO#(OffsetSize) loadOffsetQ <- mkBypassFIFO;
  FIFO#(MainMemReq) memReqQ <- mkFIFO;
  FIFO#(MainMemResp) memRespQ <- mkFIFO; 

  FIFO#(ProcReq) storeQ <- mkSizedFIFO(1);

  Reg#(Bit#(32)) hitCount <- mkReg(0);
  Reg#(Bit#(32)) missCount <- mkReg(0);

  Ehr#(2, Bool) lockL1 <- mkEhr(False);

  rule bram_to_hitQ if (mshr == HitQ);
    Vector#(16, Word) line <- cache_data.portA.response.get();
    let req_offset = loadOffsetQ.first();
    loadOffsetQ.deq();
    Word data = line[req_offset];
    hitQ.enq(data);
    mshr <= Ready;
  endrule


  rule startMiss_BRAMReq if (mshr == StartMiss_BRAMReq);
    let req_idx = missReq.addr[12:6];
    let old_line_valid = validArray[req_idx];
    let old_line_dirty = dirtyArray[req_idx];

    if (old_line_dirty && old_line_valid) begin    //the old cache line is dirty and must be written back to memory
      cache_data.portA.request.put(BRAMRequest{write: False,   //read the current cache line
                         responseOnWrite: False,
                         address: req_idx,
                         datain: ?});

      mshr <= StartMiss_BRAMResp;
    end

    else mshr <= SendFillReq;    //if the old cache line is not dirty, then no writeback needed

  endrule


  rule startMiss_BRAMResp if (mshr == StartMiss_BRAMResp);
    Vector#(16, Word) old_line <- cache_data.portA.response.get();  //get old cache line
    MainMemResp old_data = pack(old_line);   //convert vector of 16 words into 512 bits

    let req_idx = missReq.addr[12:6];
    let old_tag = tagArray[req_idx];
    LineAddr old_addr = {old_tag, req_idx};   //concat tag and idx together to get the address

    memReqQ.enq(MainMemReq {write: 1,              //writeback old cache line to memory
                addr: old_addr,
                data: old_data});

    mshr <= SendFillReq;

  endrule

  rule sendFillReq if (mshr == SendFillReq);
    memReqQ.enq(MainMemReq {write: 0,              //load new line from memory
                addr: missReq.addr[31:6],          //take top 26 bits (the line address)
                data: ?});

    mshr <= WaitFillResp;
  endrule


  rule waitFillResp if (mshr == WaitFillResp);
    memRespQ.deq();
    MainMemResp mem_data = memRespQ.first();
    Vector#(16, Word) new_line = unpack(mem_data); //unpack the 512-bit response into a vector of 16 words

    let req_store = missReq.write;
    let req_offset = missReq.addr[5:2];
    let req_idx = missReq.addr[12:6];
    let req_tag = missReq.addr[31:13];
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
    end
    else begin     //load instruction
      dirtyArray[req_idx] <= False;
      cache_data.portA.request.put(BRAMRequest{write: True,   //write new data to cache
                         responseOnWrite: False,
                         address: req_idx,
                         datain: new_line});

      Word return_data = new_line[req_offset];
      hitQ.enq(return_data);
    end

    mshr <= Ready;

  endrule


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

  endrule

  rule waitStore if (mshr == WaitStore);
    Vector#(16, Word) line <- cache_data.portA.response.get();
    ProcReq req = storeQ.first();

    let req_data = req.data;
    let req_addr = req.addr;
    let req_offset = req_addr[5:2];

    line[req_offset] = req_data;

    cache_data.portA.request.put(BRAMRequest{write: True,   //write line back to cache
                        responseOnWrite: False,
                        address: req_idx,
                        datain: line});
    
    storeQ.deq();
    mshr <= Ready;
  endrule

  rule displayPercents;
    if (missCount == 100) begin
      $display("Misses: %d Hits: %d", missCount, hitCount);
    end
  endrule

  rule clearL1Lock; lockL1[1] <= False; endrule

  method Action putFromProc(ProcReq req) if (mshr == Ready);
    let req_store = req.write; //1 if store, 0 if load
    Word req_addr = req.addr;
    Word req_data = req.data;

    let req_offset = req_addr[5:2];
    let req_idx = req_addr[12:6];
    let req_tag = req_addr[31:13];
    let cur_tag = tagArray[req_idx];
    let cur_valid = validArray[req_idx];
    //$display("Load/Store: %d, Tag: %d, Idx: %d", req_store, req_tag, req_idx);

    if (req_store == 1) begin //store instruction
        storeQ.enq(req);
    end

    else begin     //load instruction
      lockL1[0] <= True;    //lock L1 so that store buffer does not access it
      ProcReq storeQ_req = storeQ.first();
      if (storeQ_req.addr == req_addr)  begin   //hit in store Q
          //$display("here");
          hitCount <= hitCount + 1;
          Word storeQ_data = storeQ_req.data;
          hitQ.enq(storeQ_data);
      end
      else if (cur_tag == req_tag && cur_valid) begin  //cache hit 
        //$display("Cache Hit");
        hitCount <= hitCount + 1;
        cache_data.portA.request.put(BRAMRequest{write: False,   //read corresponding line (512 bits) from cache
                         responseOnWrite: False,
                         address: req_idx,
                         datain: ?});

        loadOffsetQ.enq(req_offset);   //store the line offset so that it can be used later

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
