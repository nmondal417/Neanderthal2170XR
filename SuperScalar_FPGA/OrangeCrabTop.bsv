import UsbByte::*;
import GetPut::*;
// import Orange::*;
// import AXI4_Lite_Master::*;
// import AXI4_Lite_Slave::*;
// import AXI4_Lite_Types::*;
import Connectable::*;
import DefaultValue::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import BRAM::*;
import RVUtil::*;
import pipelined::*;
import FIFO::*;
import Cache::*;
import TwoWordCache::*;
import MainMem::*;
import MemTypes::*;
import DelayLine::*;


interface OrangeCrab;
    (* prefix = "" *)
    interface Usb usb;
    
    (* always_ready, prefix = "" *)
    method Bit#(1) pin_pu;

    (* always_ready, prefix = "" *)
    method Bit#(1) rgb_led0_r();
    (* always_ready, prefix = "" *)
    method Bit#(1) rgb_led0_g();
    (* always_ready, prefix = "" *)
    method Bit#(1) rgb_led0_b();

    // (* prefix = "" *)
    // interface DramPins dram;
endinterface

// Import BVI for the usb_bridge_top
(* synthesize, default_clock_osc = "pin_clk", default_reset = "usr_btn" *)
module top(OrangeCrab ifc);
    Reg#(Bit#(8)) cnt <- mkReg(0);
    Reg#(Bit#(16)) rcnt <- mkReg(0);
    // USB Core
    UsbCore usb_core <- mkUsbCore();
    Inout#(Bit#(1)) usbp = usb_core.usb_d_p; 
    Inout#(Bit#(1)) usbn = usb_core.usb_d_n; 

    /* BRAM_Configure cfg = defaultValue(); */
    /* BRAM2PortBE#(Bit#(8), Bit#(32), 4) bram <- mkBRAM2ServerBE(cfg); */

    Reg#(Bit#(8)) r <- mkReg(0);
    Reg#(Bit#(8)) g <- mkReg(0);
    Reg#(Bit#(8)) b <- mkReg(0);

    //Reg#(Bit#(8)) req <- mkReg(0);

    FIFO#(Bit#(8)) to_host <- mkFIFO;
    FIFOF#(Bit#(8)) from_host <- mkSizedFIFOF(8);

    /* Modifiable */

       // Instantiate the dual ported memory
    MainMem2 dram <- mkMainMem2;
    //I-Cache
    Cache2 i_cache <- mkCache2;
    //D-Cache
    Cache1 d_cache <- mkCache1;

    RVIfc rv_core <- mkpipelined;
    Reg#(Mem2) ireq <- mkRegU;
    Reg#(Mem) dreq <- mkRegU;
    FIFO#(Mem) mmioreq <- mkFIFO;
    Reg#(Bit#(32)) cycle_count <- mkReg(0);

    rule tic;
	    cycle_count <= cycle_count + 1;
        if (cycle_count[3] == 1  ) begin
            r <= r + 1;
        end 
    endrule

    rule connectICacheDram;
        let lineReq <- i_cache.getToMem();
        dram.put1(lineReq);
    endrule
    rule connectDramICache;
        let resp <- dram.get1();
        i_cache.putFromMem(resp);
    endrule

    rule connectDCacheDram;
        let lineReq <- d_cache.getToMem();
        dram.put2(lineReq);
    endrule
    rule connectDramDCache;
        let resp <- dram.get2();
        d_cache.putFromMem(resp);
    endrule

    rule requestI;
        let req <- rv_core.getIReq();
        ireq <= req;
        //TODO check req struct
        i_cache.putFromProc(ProcReq2{write: req.byte_en[0], addr: req.addr, data: req.data});

    endrule
    
    rule responseI;
        let x <- i_cache.getToProc();
        let req = ireq;
     
        req.data = x;
        rv_core.getIResp(req);
    endrule

    rule requestD;
        let req <- rv_core.getDReq();
        dreq <= req;
       
        d_cache.putFromProc(ProcReq{write: req.byte_en[0], addr: req.addr, data: req.data});
    endrule

    rule responseD;
        let x <- d_cache.getToProc();
        let req = dreq;
        
        req.data = x;
        rv_core.getDResp(req);
    endrule
  
    rule requestMMIO;
        let req <- rv_core.getMMIOReq;
        
        if (req.byte_en == 'hf) begin
            if (req.addr == 'hf000_fff4) begin
                // Write integer to STDERR
                        $fwrite(stderr, "%0d", req.data);
                        $fflush(stderr);
            end
        end
        if (req.addr ==  'hf000_fff0) begin
                // Writing to STDERR
                $fwrite(stderr, "%c", req.data[7:0]);
                $fflush(stderr);
        end else
            if (req.addr == 'hf000_fff8) begin
            // Exiting Simulation
                if (req.data == 0) begin
                        $fdisplay(stderr, "  [0;32mPASS[0m");
                end
                else
                    begin
                        $fdisplay(stderr, "  [0;31mFAIL[0m (%0d)", req.data);
                    end
                $fflush(stderr);
                $finish;
            end

        mmioreq.enq(req);
    endrule

    rule responseMMIO;
        let req = mmioreq.first();
        mmioreq.deq();
        rv_core.getMMIOResp(req);
    endrule




    /* Do not Touch Section */
    //dont touch
    rule reset_setup;
        cnt <= cnt + 1;
        let reset = ~rcnt[5];
        rcnt <= rcnt + zeroExtend(reset);
        usb_core.reset(reset);
    endrule

    //dont touch
    //recieving from host
    rule get_k ;
        Bit#(8) addr = 0;
        if (from_host.notFull) begin
            addr <- usb_core.uart_out();
            if (usb_core.uart_out_ready() == 1) begin 
                from_host.enq(addr);
                // r <= ~r;
            end
        end
    endrule
    
    // dont touch 
    // sending to host
    rule output_uart;
        usb_core.uart_in(to_host.first());
        if (usb_core.uart_in_ready() == 1) 
            to_host.deq();
    endrule

    method Bit#(1) rgb_led0_r();
        return ~pack(cnt < r);
    endmethod

    method Bit#(1) rgb_led0_g();
        return ~pack(cnt < g);
    endmethod

    method Bit#(1) rgb_led0_b();
        return ~pack(cnt < b);
    endmethod

    method Bit#(1) pin_pu;
        return 1;
    endmethod
    
    interface usb = interface Usb;
        interface pin_usb_p = usbp;
        interface pin_usb_n = usbn;
    endinterface;

    // interface dram = system.dram;        

endmodule
