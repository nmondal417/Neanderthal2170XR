import FIFO::*;
import SpecialFIFOs::*;
import RegFile::*;
import RVUtil::*;
import Vector::*;
import KonataHelper::*;
import Printf::*;
import Ehr::*;
import Supfifo::*;

typedef struct { Bit#(4) byte_en; Bit#(32) addr; Bit#(65) data; } Mem deriving (Eq, FShow, Bits);

interface RVIfc;
    method ActionValue#(Mem) getIReq();
    method Action getIResp(Mem a);
    method ActionValue#(Mem) getDReq();
    method Action getDResp(Mem a);
    method ActionValue#(Mem) getMMIOReq();
    method Action getMMIOResp(Mem a);
endinterface


typedef struct { Bool isUnsigned; Bit#(2) size; Bit#(2) offset; Bool mmio; } MemBusiness deriving (Eq, FShow, Bits);

function Bool isMMIO(Bit#(32) addr);
    Bool x = case (addr) 
        32'hf000fff0: True;
        32'hf000fff4: True;
        32'hf000fff8: True;
        default: False;
    endcase;
    return x;
endfunction

typedef struct { Bit#(32) pc;
                 Bit#(32) ppc;
                 Bit#(1) epoch; 
                 KonataId k_id; // <- This is a unique identifier per instructions, for logging purposes
             } F2D deriving (Eq, FShow, Bits);

typedef struct { 
    DecodedInst dinst;
    Bit#(32) pc;
    Bit#(32) ppc;
    Bit#(1) epoch;
    Bit#(32) rv1; 
    Bit#(32) rv2; 
    Bit#(5) rd_idx;
    KonataId k_id; // <- This is a unique identifier per instructions, for logging purposes
    } D2E deriving (Eq, FShow, Bits);

typedef struct { 
    MemBusiness mem_business;
    Bit#(32) data;
    DecodedInst dinst;
    KonataId k_id; // <- This is a unique identifier per instructions, for logging purposes
} E2W deriving (Eq, FShow, Bits);

interface Scoreboard;
    method Action insert(Bit#(5) rd_idx);
    method Action remove1(Bit#(5) rd_idx);
    method Action remove2(Bit#(5) rd_idx);
    method Bool search1(Bit#(5) rs1_idx);
    method Bool search2(Bit#(5) rs2_idx);
    method Bool search3(Bit#(5) rd_idx);
endinterface

(* synthesize *)
module mkScoreboard(Scoreboard);
    Vector#(32, Ehr#(3, Bool)) sb <- replicateM(mkEhr(False)); 
    
    method Action insert(Bit#(5) rd_idx);
        sb[rd_idx][0] <= True;
    endmethod
    method Action remove1(Bit#(5) rd_idx);
        sb[rd_idx][1] <= False;
    endmethod
    method Action remove2(Bit#(5) rd_idx);
        sb[rd_idx][2] <= False;
    endmethod
    method Bool search1(Bit#(5) rsrc_idx1);
        return sb[rsrc_idx1][0];
    endmethod
    method Bool search2(Bit#(5) rsrc_idx2);
        return sb[rsrc_idx2][0];
    endmethod 
    method Bool search3(Bit#(5) rd_idx);
        return sb[rd_idx][0];
    endmethod 

    
endmodule
(* synthesize *)
module mkpipelined(RVIfc);
    // Interface with memory and devices
    FIFO#(Mem) toImem <- mkBypassFIFO;
    FIFO#(OneOrTwoWords) fromImem <- mkBypassFIFO;
    FIFO#(Mem) toDmem <- mkBypassFIFO;
    FIFO#(Mem) fromDmem <- mkBypassFIFO;
    FIFO#(Mem) toMMIO <- mkBypassFIFO;
    FIFO#(Mem) fromMMIO <- mkBypassFIFO;
    let debug = False;
    let mmio_debug = False;
    let konata_debug = False;

    //Program Counter
    Ehr#(2, Bit#(32)) program_counter <- mkEhr(32'h0000000);
    
    //Register File
    Vector#(32, Reg#(Bit#(32))) rf <- replicateM(mkReg(0));

    //Queues for Pipeling 
    SupFifo#(F2D) f2d <- mkSupFifo;
    SupFifo#(D2E) d2e <- mkSupFifo;
    SupFifo#(E2W) e2w <- mkSupFifo;

    //Epoch
    Reg#(Bit#(1)) mEpoch <- mkReg(0);

    //Scoreboard
    Vector#(32, Ehr#(3, Bool)) sb <- replicateM(mkEhr(False));

	// Code to support Konata visualization
    String dumpFile = "output.log" ;
    let lfh <- mkReg(InvalidFile);
	Reg#(KonataId) fresh_id <- mkReg(0);
	Reg#(KonataId) commit_id <- mkReg(0);
	FIFO#(KonataId) retired <- mkFIFO;
	FIFO#(KonataId) squashed <- mkFIFO;

    //Tics
    Reg#(Bool) starting <- mkReg(True);
    Bit#(32) maxCount = 600;
    Reg#(Bit#(32)) count <- mkReg(0);
    rule doTic;
        if (debug && count < maxCount) begin
            $display("Cycle %d", count);
        end
        count <= count + 1;
    endrule

	rule do_tic_logging;
        if (starting) begin
            let f <- $fopen(dumpFile, "w") ;
            lfh <= f;
            $fwrite(f, "Kanata\t0004\nC=\t1\n");
            starting <= False;
        end
		konataTic(lfh);
	endrule

	//Each state can run concurrently might not need guards
    rule fetch if (!starting);
        // You should put the pc that you fetch in pc_fetched
        // Below is the code to support Konata's visualization
        
		let iid <- fetch1Konata(lfh, fresh_id, 0);
        if (konata_debug) labelKonataLeft(lfh, iid, $format("PC %x",program_counter[0]));
        if(debug && count < maxCount) $display("Fetch %x", program_counter[0]);
        toImem.enq(Mem{byte_en: 0,  addr: program_counter[0], data: 0});
        f2d.enq1(F2D{pc: program_counter[0], ppc: program_counter[0] + 4, epoch: mEpoch, k_id: iid});
        if  (program_counter[5:2] != 15 ) begin 
            f2d.enq2(F2D{pc: program_counter[0] + 4, ppc: program_counter[0] + 8, epoch: mEpoch, k_id: iid});
            program_counter[0] <= program_counter[0] + 8;
        end else begin 
            program_counter[0] <= program_counter[0] + 4;
        end 
    endrule

    rule decode if (!starting);
        //First Instruction
        let imemInst1 = fromImem.first()[64:33];
        let isTwoWords = fromImem.first()[32];
        let imemInst2 = fromImem.first()[31:0];

        // F2D Data 1
        let f2d_data_1 = f2d.first1();
        let pc_1 = f2d_data_1.pc;
        let ppc_1 = f2d_data_1.ppc;
        let fEpoch_1 = f2d_data_1.epoch;
        let current_id_1 = f2d_data_1.k_id;

        //Decoding
        let rs1_idx_1 = getInstFields(imemInst1).rs1;
        let rs2_idx_1 = getInstFields(imemInst1).rs2;
        let rd_1 = getInstFields(imemInst1).rd;
        let rs1_1 = (rs1_idx_1 == 0 ? 0 : rf[rs1_idx_1]);
        let rs2_1 = (rs2_idx_1 == 0 ? 0 : rf[rs2_idx_1]);
        let dInst_1 = decodeInst(imemInst1);

        //F2D Data 2
        let f2d_data_2 = f2d.first2();
        let pc_2 = f2d_data_2.pc;
        let ppc_2 = f2d_data_2.ppc;
        let fEpoch_2 = f2d_data_2.epoch;
        let current_id_2 = f2d_data_2.k_id;
        
        //Decodeing
        let rs1_idx_2 = getInstFields(imemInst2).rs1;
        let rs2_idx_2 = getInstFields(imemInst2).rs2;
        let rd_2 = getInstFields(imemInst2).rd;
        let rs1_2 = (rs1_idx_2 == 0 ? 0 : rf[rs1_idx_2]);
        let rs2_2 = (rs2_idx_2 == 0 ? 0 : rf[rs2_idx_2]);
        let dInst_2 = decodeInst(imemInst2);
        
        //if(noDependency(sb,ins1) && noDependency(sb, ins2))
        if(!(sb[rs1_1][0] || sb[rs2_1][0] || sb[rd_1][0]) && !(sb[rs1_2][0] || sb[rs2_2][0] || sb[rd_2][0])) begin 
            
            //Debug 1
            if (debug && count < maxCount) $display(pc_1, " [Decode] ", fshow(dInst_1));
            if (konata_debug) decodeKonata(lfh, current_id_1);
            if (konata_debug) labelKonataLeft(lfh,current_id_1, $format("Instr bits: %x",dInst_1.inst));
            if (konata_debug) labelKonataLeft(lfh, current_id_1, $format(" Potential r1: %x, Potential r2: %x" , rs1_1, rs2_1)); 
            if(fEpoch_1 == mEpoch) begin
                if(dInst_1.valid_rd && rd_1 != 0) begin
                    sb[rd_1][0] <= True;
                end 
                d2e.enq1(D2E{dinst: dInst_1, pc: pc_1, ppc: ppc_1, epoch: fEpoch_1, rv1: rs1_1, rv2: rs2_1, rd_idx: rd_1, k_id: current_id_1});
            end 

            //Debug 2
            if (debug && count < maxCount) $display(pc_2, " [Decode] ", fshow(dInst_2));
            if (konata_debug) decodeKonata(lfh, current_id_2);
            if (konata_debug) labelKonataLeft(lfh,current_id_2, $format("Instr bits: %x",dInst_2.inst));
            if (konata_debug) labelKonataLeft(lfh, current_id_2, $format(" Potential r1: %x, Potential r2: %x" , rs1_2, rs2_2)); 
            if (fEpoch_2 == mEpoch) begin 
                if(dInst_2.valid_rd && rd_2 != 0) begin
                    sb[rd_2][0] <= True;
                end 
                d2e.enq2(D2E{dinst: dInst_2, pc: pc_2, ppc: ppc_2, epoch: fEpoch_2, rv1: rs1_2, rv2: rs2_2, rd_idx: rd_2, k_id: current_id_2});
            end 
            f2d.deq1();
            f2d.deq2();
            fromImem.deq();
        //if(noDependency(ins1, sb))
        end else if (!(sb[rs1_1][0] || sb[rs2_1][0] || sb[rd_1][0])) begin 
            //Debug 1
            if (debug && count < maxCount) $display(pc_1, " [Decode] ", fshow(dInst_1));
            if (konata_debug) decodeKonata(lfh, current_id_1);
            if (konata_debug) labelKonataLeft(lfh,current_id_1, $format("Instr bits: %x",dInst_1.inst));
            if (konata_debug) labelKonataLeft(lfh, current_id_1, $format(" Potential r1: %x, Potential r2: %x" , rs1_1, rs2_1));

            if(fEpoch_1 == mEpoch) begin
                if(dInst_1.valid_rd && rd_1 != 0) begin
                    sb[rd_1][0] <= True;
                end 
                d2e.enq1(D2E{dinst: dInst_1, pc: pc_1, ppc: ppc_1, epoch: fEpoch_1, rv1: rs1_1, rv2: rs2_1, rd_idx: rd_1, k_id: current_id_1});
            end 
            f2d.deq1();
            fromImem.deq();
        end 
    endrule

    rule execute if (!starting);
        let ins1 = d2e.first1();
        let ins2 = d2e.first2();
        d2e.deq1();

        //Debug 
        if (debug && count < maxCount) $display(ins1.pc, " [Execute] ", fshow(ins1.dinst));
		if (konata_debug) executeKonata(lfh, ins1.k_id);
        if (debug && count < maxCount) $display(ins2.pc, " [Execute] ", fshow(ins2.dinst));
		if (konata_debug) executeKonata(lfh, ins2.k_id);

        if(ins1.epoch != mEpoch && ins2.epoch != mEpoch ) begin 
            d2e.deq2();
            sb[ins1.rd_idx][1] <= False;
            sb[ins2.rd_idx][1] <= False;
        end else if (ins1.epoch != mEpoch) begin 
            sb[ins1.rd_idx][1] <= False;
        end else if (isALU(ins1.dinst) && isALU(ins2.dinst) && ins2.epoch == mEpoch) begin 
            //Execute Both
            d2e.deq2();

            // Instruction 1 
            let imm_1 = getImmediate(ins1.dinst);
            Bool mmio_1 = False;
            let data_1 = execALU32(ins1.dinst.inst, ins1.rv1, ins1.rv2, imm_1, ins1.pc);
            let isUnsigned_1 = 0;
            let funct3_1 = getInstFields(ins1.dinst.inst).funct3;
            let size_1 = funct3_1[1:0];
            let addr_1 = ins1.rv1 + imm_1;
            Bit#(2) offset_1 = addr_1[1:0];
            
            if (debug && count < maxCount) $display(ins1.pc, "Register Source 1: %d", ins1.rv1);
            if (debug && count < maxCount) $display(ins1.pc, "Register Source 2: %d", ins1.rv2);
            if (debug && count < maxCount) $display(ins1.pc, "Immediate: %d", imm_1);
            if (konata_debug) labelKonataLeft(lfh, ins1.k_id, $format(" ALU output: %x" , data_1));
            // not sure if this required
            let controlResult_1 = execControl32(ins1.dinst.inst, ins1.rv1, ins1.rv2, imm_1, ins1.pc);
            let nextPc_1 = controlResult_1.nextPC;

            if(ins1.ppc != nextPc_1) begin
                if(debug && count < maxCount) $display("New PC: ", fshow(nextPc_1));
                mEpoch <= mEpoch + 1;
                program_counter[1] <= nextPc_1;
            end 

            e2w.enq1(E2W{mem_business: MemBusiness { isUnsigned : unpack(isUnsigned_1), size : size_1, offset : offset_1, mmio: mmio_1}, data: data_1, dinst: ins1.dinst, k_id: ins1.k_id});

            // Instruction 2 
            let imm_2 = getImmediate(ins2.dinst);
            Bool mmio_2 = False;
            let data_2 = execALU32(ins2.dinst.inst, ins2.rv1, ins2.rv2, imm_2, ins2.pc);
            let isUnsigned_2 = 0;
            let funct3_2 = getInstFields(ins2.dinst.inst).funct3;
            let size_2 = funct3_2[1:0];
            let addr_2 = ins2.rv1 + imm_1;
            Bit#(2) offset_2 = addr_2[1:0];
            
            if (debug && count < maxCount) $display(ins1.pc, "Register Source 1: %d", ins2.rv1);
            if (debug && count < maxCount) $display(ins1.pc, "Register Source 2: %d", ins2.rv2);
            if (debug && count < maxCount) $display(ins1.pc, "Immediate: %d", imm_2);
            if (konata_debug) labelKonataLeft(lfh, ins2.k_id, $format(" ALU output: %x" , data_2));
            
            // not sure if this required
            let controlResult_2 = execControl32(ins2.dinst.inst, ins2.rv1, ins2.rv2, imm_2, ins2.pc);
            let nextPc_2 = controlResult_2.nextPC;
            
            if(ins2.ppc != nextPc_2) begin
                if(debug && count < maxCount) $display("New PC: ", fshow(nextPc_2));
                mEpoch <= mEpoch + 1;
                program_counter[1] <= nextPc_2;
            end 

            e2w.enq2(E2W{mem_business: MemBusiness { isUnsigned : unpack(isUnsigned_2), size : size_2, offset : offset_2, mmio: mmio_2}, data: data_2, dinst: ins2.dinst, k_id: ins2.k_id});
        end else begin 
            //Original
            let imm_1 = getImmediate(ins1.dinst);
            Bool mmio_1 = False;
            let data_1 = execALU32(ins1.dinst.inst, ins1.rv1, ins1.rv2, imm_1, ins1.pc);
            let isUnsigned_1 = 0;
            let funct3_1 = getInstFields(ins1.dinst.inst).funct3;
            let size_1 = funct3_1[1:0];
            let addr_1 = ins1.rv1 + imm_1;
            Bit#(2) offset_1 = addr_1[1:0];

            if (debug && count < maxCount) $display(ins1.pc, "Register Source 1: %d", ins1.rv1);
            if (debug && count < maxCount) $display(ins1.pc, "Register Source 2: %d", ins1.rv2);
            if (debug && count < maxCount) $display(ins1.pc, "Immediate: %d", imm_1);

            if (isMemoryInst(ins1.dinst)) begin
                // Technical details for load byte/halfword/word
                let shift_amount_1 = {offset_1, 3'b0};
                let byte_en_1 = 0;
                case (size_1) matches
                2'b00: byte_en_1 = 4'b0001 << offset;
                2'b01: byte_en_1 = 4'b0011 << offset;
                2'b10: byte_en_1 = 4'b1111 << offset;
                endcase

                data_1 = rv2 << shift_amount;
                addr_1 = {addr[31:2], 2'b0};
                isUnsigned_1 = funct3[2];

                let type_mem_1 = (ins1.dinst.inst[5] == 1) ? 1 : 0;
                let req_1 = Mem {byte_en : type_mem_1,
                        addr : addr_1,
                        data : data_1};
                
                //if (debug && count < maxCount) $display(pc, "Register Source 1: %d", rv1);
                if (debug && count < maxCount) $display(ins1.pc, "Memory address ", addr_1);
                //if (debug && count < maxCount) $display(pc, "Register Source 2: %d", rv2);
                //if (debug && count < maxCount) $display(pc, "Immediate: %d", imm);
                if (isMMIO(addr_1)) begin 
                    if (mmio_debug) $display(count, " [Execute] MMIO", fshow(req_1));
                    toMMIO.enq(req_1);
                    if (konata_debug) labelKonataLeft(lfh,ins1.k_id, $format(" MMIO ", fshow(req_1)));
                    mmio_1 = True;
                end else begin 
                    if (konata_debug) labelKonataLeft(lfh,ins1.k_id, $format(" MEM ", fshow(req_1)));
                    toDmem.enq(req_1);
                end
            end
            else if (isControlInst(ins1.dinst)) begin
                    if (konata_debug) labelKonataLeft(lfh,ins1.k_id, $format(" Ctrl instr "));
                    data_1 = ins1.pc + 4;
            end else begin 
                if (konata_debug) labelKonataLeft(lfh,ins1.k_id, $format(" Standard instr "));
            end
            let controlResult_1 = execControl32(ins1.dinst.inst, ins1.rv1, ins1.rv2, imm_1, ins1.pc);
            let nextPc_1 = controlResult_1.nextPC;
            if(ins1.ppc != nextPc_1) begin
                if(debug && count < maxCount) $display("New PC: ", fshow(nextPc_1));
                mEpoch <= mEpoch + 1;
                program_counter[1] <= nextPc_1;
            end 
            
            if (konata_debug) labelKonataLeft(lfh, ins1.k_id, $format(" ALU output: %x" , data_1));
            e2w.enq1(E2W{mem_business: MemBusiness { isUnsigned : unpack(isUnsigned_1), size : size_1, offset : offset_1, mmio: mmio_1}, data: data_1, dinst: ins1.dinst, k_id: ins1.k_id});
        end 
    endrule

    rule writeback if (!starting);
        // TODO
        let e2w_data = e2w.first();
        e2w.deq();
        let mem_business = e2w_data.mem_business;
        let data = e2w_data.data; 
        let dInst = e2w_data.dinst;
        let current_id = e2w_data.k_id;
       
        if (konata_debug) writebackKonata(lfh,current_id);
        if (konata_debug) retired.enq(current_id);
        let fields = getInstFields(dInst.inst);
        if (isMemoryInst(dInst) && (mem_business.mmio || dInst.valid_rd)) begin // (* // write_val *)
            
            let resp = ?;
		    if (mem_business.mmio) begin 
                resp = fromMMIO.first();
		        fromMMIO.deq();
		    end else begin 
                resp = fromDmem.first();
		        fromDmem.deq();
		    end

            if(debug && count < maxCount) $display("Mem Inst: ", fshow(resp));

            let mem_data = resp.data;
            mem_data = mem_data >> {mem_business.offset ,3'b0};
            case ({pack(mem_business.isUnsigned), mem_business.size}) matches
	     	3'b000 : data = signExtend(mem_data[7:0]);
	     	3'b001 : data = signExtend(mem_data[15:0]);
	     	3'b100 : data = zeroExtend(mem_data[7:0]);
	     	3'b101 : data = zeroExtend(mem_data[15:0]);
	     	3'b010 : data = mem_data;
             endcase
		end
		if(debug && count < maxCount) $display("[Writeback]", fshow(dInst));
        if (!dInst.legal) begin
			if(debug && count < maxCount) $display("[Writeback] Illegal Inst, Drop and fault: ", fshow(dInst));
			//program_counter <= 0;	// Fault
	    end
		if (dInst.valid_rd) begin
            let rd_idx = fields.rd;
            if (rd_idx != 0) begin 
                rf[rd_idx] <=data;
                scoreboard.remove2(rd_idx);
            end
		end
        

        // Similarly, to register an execute event for an instruction:
	   	//	writebackKonata(lfh,k_id);


	   	// In writeback is also the moment where an instruction retires (there are no more stages)
	   	// Konata requires us to register the event as well using the following: 
		//retired.enq(k_id);
	endrule
		

	// ADMINISTRATION:

    rule administrative_konata_commit;
		    retired.deq();
		    let f = retired.first();
		    commitKonata(lfh, f, commit_id);
	endrule
		
	rule administrative_konata_flush;
		    squashed.deq();
		    let f = squashed.first();
		    squashKonata(lfh, f);
	endrule
		
    method ActionValue#(Mem) getIReq();
		toImem.deq();
		return toImem.first();
    endmethod
    method Action getIResp(Mem a);
    	fromImem.enq(a);
    endmethod
    method ActionValue#(Mem) getDReq();
		toDmem.deq();
		return toDmem.first();
    endmethod
    method Action getDResp(Mem a);
		fromDmem.enq(a);
    endmethod
    method ActionValue#(Mem) getMMIOReq();
		toMMIO.deq();
		return toMMIO.first();
    endmethod
    method Action getMMIOResp(Mem a);
		fromMMIO.enq(a);
    endmethod
endmodule
