typedef Bit#(30) LineAddr;
typedef struct { Bit#(1) write; LineAddr addr; Bit#(512) data; } MainMemReq deriving (Eq, FShow, Bits, Bounded);
typedef Bit#(512) MainMemResp;

typedef Bit#(32) Word;
