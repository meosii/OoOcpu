`ifndef OOOCPU_ROB
`define OOOCPU_ROB
module rob(
    input wire                              clk,
    input wire                              rst_n,

    // allocate
    input wire                              allocate_en,
        // from decoder
    input wire  [`PC_WIDTH-1 : 0]           pc,
    input wire  [`GPR_ADDR_WIDTH-1 : 0]     dst_addr, // gpr addr
    input wire                              dst_wen,  
    input wire  [`GPR_ADDR_WIDTH-1 : 0]     rs1_addr, // gpr addr
    input wire  [`GPR_ADDR_WIDTH-1 : 0]     rs2_addr, // gpr addr
    input wire                              csr_rd_en,
    input wire [`CSR_ADDR_WIDTH - 1 :0]     csr_rd_addr,
    input wire                              csr_w_en,
    input wire [`CSR_ADDR_WIDTH - 1 :0]     csr_w_addr,
    input wire [`DATA_WIDTH_CSR_OP-1 : 0]   csr_op,
    input wire [4:0]                        csr_uimm,
    input wire                              ebreak_en,
    input wire                              ecall_en,
    input wire                              mret_en,
    input wire [`DATA_WIDTH_ISA_EXP - 1 : 0]exp_code_in_decoder,
    // from rat, search ROB send data to issue queue
    input wire                              rs1_rat_valid,  // when rat_valid==0, rs1_value_fromGPR valid.
    input wire [$clog2(`ROB_DEPTH)-1 : 0]   rs1_Paddr,
    input wire                              rs2_rat_valid,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]   rs2_Paddr,

    // writeback
        // from alu
    input wire  [`GPR_ADDR_WIDTH-1 : 0]     wb_alu_dst_Paddr,
    input wire  [`WORD_WIDTH-1 : 0]         wb_alu_out,
    input wire                              wb_alu_valid,
        // from div
    input wire  [`GPR_ADDR_WIDTH-1 : 0]     wb_div_dst_Paddr,
    input wire  [`WORD_WIDTH-1 : 0]         wb_div_out,
    input wire                              wb_div_valid,
        // from mul
    input wire  [`GPR_ADDR_WIDTH-1 : 0]     wb_mul_dst_Paddr,
    input wire  [`WORD_WIDTH-1 : 0]         wb_mul_out,
    input wire                              wb_mul_valid,
        // from mem
    input wire  [`GPR_ADDR_WIDTH-1 : 0]     wb_mem_dst_Paddr,
    input wire  [`WORD_WIDTH-1 : 0]         wb_load_data,
    input wire                              wb_load_valid,
    input wire [`DATA_WIDTH_ISA_EXP - 1 : 0]exp_code_in_mem_ctrl,
        // store
    input wire                              store_finish,

    // branch
    input wire                              wb_br_finish,   // branch instruction has judged whether jump and jump to where
    input wire                              wb_br_taken,    // whether jump or not
    input wire  [`PC_WIDTH-1 : 0]           wb_br_addr,     // jump to where
    input wire  [$clog2(`ROB_DEPTH)-1 : 0]  wb_br_rob,      // which instruction
    input wire [`WORD_WIDTH-1 : 0]          wb_br_out,      // jal and jalr: rd = pc+4
    input wire                              wb_br_out_valid,
    // from csr
    input wire [`WORD_WIDTH - 1 : 0]        csr_rd_data,
    // from csr_iq (from gpr[rs1])
    input wire                              csr_rs1_data_valid,
    input wire [`WORD_WIDTH - 1 : 0]        csr_rs1_data,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]   csr_Pdst,

    // outputs

    // rob allocate
        // To rat
    output wire [$clog2(`ROB_DEPTH)-1 : 0]  rob_alloc_tag_2rat,         // Paddr
    output wire [`GPR_ADDR_WIDTH-1 : 0]     rob_alloc_dst_addr_2rat,    // Aaddr
    output wire                             rob_alloc_dst_wen_2rat,     // whether write
    
    // rob commit
        // To rat
    output wire                             commit_dst_en,
    output wire [`GPR_ADDR_WIDTH-1 : 0]     rob_commit_dst_addr_2rat,   // Aaddr
    output wire [$clog2(`ROB_DEPTH)-1 : 0]  rob_commit_Paddr,           // Paddr
    output wire                             rob_commit_br_taken,        // branch -> flush the rat
    output wire [`PC_WIDTH-1 : 0]           rob_commit_br_addr,         // branch -> flush the rat
    output wire                             rob_commit_exp_en,          // exp -> flush the rat
    output wire [`DATA_WIDTH_ISA_EXP-1:0]   rob_commit_exp_op,
        // To gpr
    output wire [`WORD_WIDTH-1 : 0]         rob_commit_dst_value_2rat,  // data
        // To csr
    output reg                              rob_commit_csr_rd_en,
    output reg [`CSR_ADDR_WIDTH - 1 :0]     rob_commit_csr_rd_addr,
    output reg                              rob_commit_csr_w_en,
    output reg [`CSR_ADDR_WIDTH - 1 :0]     rob_commit_csr_w_addr,
    output reg [`DATA_WIDTH_CSR_OP-1 : 0]   rob_commit_csr_op,
    output reg [4:0]                        rob_commit_csr_uimm,
    output reg [`WORD_WIDTH - 1 : 0]        rob_commit_csr_rs1_data,
        // To cpu_ctrl
    output wire [`PC_WIDTH-1 : 0]           rob_commit_pc,
    output reg [2:0]                        rob_commit_ebreak_ecall_mret,
    // to id_reg to issue queue
    output wire [$clog2(`ROB_DEPTH)-1 : 0]  alloc_rob,                  // wptr, Paddr
    // stall the pipeline
    output wire                             rob_full,
    output wire                             rob_empty,
    
    // to issue queue
    output reg                              rs1_fromROB_valid,
    output reg                              rs2_fromROB_valid,
    output reg [`WORD_WIDTH-1 : 0]          rs1_value_fromROB,
    output reg [`WORD_WIDTH-1 : 0]          rs2_value_fromROB
);
genvar i;

reg [$clog2(`ROB_DEPTH) : 0]  wptr; // [5:0],the highest bit is used to determine empty or full
reg [$clog2(`ROB_DEPTH) : 0]  rptr;

wire [$clog2(`ROB_DEPTH) : 0] wptr_next;
wire [$clog2(`ROB_DEPTH) : 0] rptr_next;

wire    rob_flush;
wire    retire_en;
//-------------------------------rob reg------------------------------------------------------
    // from allocate
reg [`PC_WIDTH-1 : 0]           rob_pc              [0 : `ROB_DEPTH-1];
reg [`GPR_ADDR_WIDTH-1 : 0]     rob_dst_addr        [0 : `ROB_DEPTH-1];
reg                             rob_dst_wen         [0 : `ROB_DEPTH-1];
reg [`GPR_ADDR_WIDTH-1 : 0]     rob_rs1_addr        [0 : `ROB_DEPTH-1];
reg [`GPR_ADDR_WIDTH-1 : 0]     rob_rs2_addr        [0 : `ROB_DEPTH-1];
reg                             rob_csr_rd_en       [0 : `ROB_DEPTH-1];
reg [`CSR_ADDR_WIDTH - 1 :0]    rob_csr_rd_addr     [0 : `ROB_DEPTH-1];
reg                             rob_csr_w_en        [0 : `ROB_DEPTH-1];
reg [`CSR_ADDR_WIDTH - 1 :0]    rob_csr_w_addr      [0 : `ROB_DEPTH-1];
reg [`DATA_WIDTH_CSR_OP-1 : 0]  rob_csr_op          [0 : `ROB_DEPTH-1];
reg [4:0]                       rob_csr_uimm        [0 : `ROB_DEPTH-1];
reg [2:0]                       rob_ebreak_ecall_mret[0 : `ROB_DEPTH-1];
    // from writeback
reg [`WORD_WIDTH-1 : 0]         rob_dst_value       [0 : `ROB_DEPTH-1];
reg                             rob_dst_value_ready [0 : `ROB_DEPTH-1];
reg                             rob_store_ready     [0 : `ROB_DEPTH-1]; // instruction store finished
reg                             rob_branch_ready    [0 : `ROB_DEPTH-1]; // instruction judgement finished
reg                             rob_br_taken        [0 : `ROB_DEPTH-1]; // branch
reg [`PC_WIDTH-1 : 0]           rob_br_addr         [0 : `ROB_DEPTH-1]; // branch
reg [`WORD_WIDTH - 1 : 0]       rob_csr_rs1_data    [0 : `ROB_DEPTH-1]; // write to csr
reg                             rob_csr_ready       [0 : `ROB_DEPTH-1];
    // expection
reg                             rob_exp_en          [0 : `ROB_DEPTH-1];
reg [`DATA_WIDTH_ISA_EXP-1 : 0] rob_exp_op          [0 : `ROB_DEPTH-1];
//--------------------------------------------------------------------------------------------

always @(posedge clk or negedge rst_n) begin
    if (~rst_n) begin
        wptr <= 'b0;
        rptr <= 'b0;
    end else begin
        wptr <= wptr_next;
        rptr <= rptr_next;
    end
end

assign rob_flush        = rob_commit_br_taken || rob_commit_exp_en || rob_commit_ebreak_ecall_mret[1] || rob_commit_ebreak_ecall_mret[2];
assign alloc_rob        = wptr[$clog2(`ROB_DEPTH)-1 : 0];
assign rob_commit_Paddr = rptr[$clog2(`ROB_DEPTH)-1 : 0];

assign wptr_next =  (rob_flush  )?  rptr + 1    :
                    (allocate_en)?  wptr + 1    :   wptr;

assign rptr_next =  (retire_en)? rptr + 1   :   rptr;

assign rob_empty =  (wptr == rptr);
assign rob_full  =  (wptr[$clog2(`ROB_DEPTH)] == !rptr[$clog2(`ROB_DEPTH)]) &&
                    (wptr[$clog2(`ROB_DEPTH)-1 : 0] == rptr[$clog2(`ROB_DEPTH)-1 : 0]);

// allocate to rat
assign rob_alloc_tag_2rat = alloc_rob;
assign rob_alloc_dst_addr_2rat = dst_addr;
assign rob_alloc_dst_wen_2rat = dst_wen;

// ROB commit to rat and gpr
assign retire_en                    =  (rob_dst_value_ready [rob_commit_Paddr]  || 
                                        rob_store_ready     [rob_commit_Paddr]  ||
                                        rob_branch_ready    [rob_commit_Paddr]  ||
                                        rob_csr_ready       [rob_commit_Paddr]  ||
                                        rob_exp_en          [rob_commit_Paddr]  )   &&  !rob_empty;
assign rob_commit_pc                =   rob_pc              [rob_commit_Paddr]  ;
assign rob_commit_dst_addr_2rat     =   rob_dst_addr        [rob_commit_Paddr]  ;
assign rob_commit_br_taken          =   rob_br_taken        [rob_commit_Paddr]  ;
assign rob_commit_br_addr           =   rob_br_addr         [rob_commit_Paddr]  ;
assign rob_commit_exp_en            =   rob_exp_en          [rob_commit_Paddr]  ;
assign rob_commit_exp_op            =   rob_exp_op          [rob_commit_Paddr]  ;
assign rob_commit_csr_rd_en         =   rob_csr_rd_en       [rob_commit_Paddr]  ;
assign rob_commit_csr_rd_addr       =   rob_csr_rd_addr     [rob_commit_Paddr]  ;
assign rob_commit_csr_w_en          =   rob_csr_w_en        [rob_commit_Paddr]  ;
assign rob_commit_csr_w_addr        =   rob_csr_w_addr      [rob_commit_Paddr]  ;
assign rob_commit_csr_op            =   rob_csr_op          [rob_commit_Paddr]  ;
assign rob_commit_csr_uimm          =   rob_csr_uimm        [rob_commit_Paddr]  ;
assign rob_commit_ebreak_ecall_mret =   rob_ebreak_ecall_mret[rob_commit_Paddr] ;
assign rob_commit_csr_rs1_data      =   rob_csr_rs1_data    [rob_commit_Paddr]  ;
// ROB commit to Physical regfile
assign rob_commit_dst_value_2rat    =   (rob_commit_csr_rd_en)? csr_rd_data                     :
                                                                rob_dst_value[rob_commit_Paddr] ;
assign commit_dst_en                =   rob_dst_value_ready [rob_commit_Paddr] || rob_commit_csr_rd_en;
// write rob, from allocate
generate
    for (i=0; i<`ROB_DEPTH; i++) begin: GENERATE_ROB_INITAL
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_pc          [i]     <= `PC_WIDTH'b0;
                rob_dst_addr    [i]     <= 'b0;
                rob_dst_wen     [i]     <= 'b0;
                rob_rs1_addr    [i]     <= 'b0; 
                rob_rs2_addr    [i]     <= 'b0; 
                rob_csr_rd_en   [i]     <= 'b0; 
                rob_csr_rd_addr [i]     <= 'b0; 
                rob_csr_w_en    [i]     <= 'b0; 
                rob_csr_w_addr  [i]     <= 'b0; 
                rob_csr_op      [i]     <= `CSR_OP_NOP; 
                rob_csr_uimm    [i]     <= 'b0; 
                rob_ebreak_ecall_mret[i]<= 3'b000;
            end else if (rob_flush) begin
                rob_pc          [i]     <= `PC_WIDTH'b0;
                rob_dst_addr    [i]     <= 'b0;
                rob_dst_wen     [i]     <= 'b0;
                rob_rs1_addr    [i]     <= 'b0; 
                rob_rs2_addr    [i]     <= 'b0; 
                rob_csr_rd_en   [i]     <= 'b0; 
                rob_csr_rd_addr [i]     <= 'b0; 
                rob_csr_w_en    [i]     <= 'b0; 
                rob_csr_w_addr  [i]     <= 'b0; 
                rob_csr_op      [i]     <= `CSR_OP_NOP; 
                rob_csr_uimm    [i]     <= 'b0; 
                rob_ebreak_ecall_mret[i]<= 3'b000;
            end else if (allocate_en && (alloc_rob == i)) begin
                rob_pc          [alloc_rob]     <= pc; 
                rob_dst_addr    [alloc_rob]     <= dst_addr;
                rob_dst_wen     [alloc_rob]     <= dst_wen;
                rob_rs1_addr    [alloc_rob]     <= rs1_addr;
                rob_rs2_addr    [alloc_rob]     <= rs2_addr;
                rob_csr_rd_en   [alloc_rob]     <= csr_rd_en;
                rob_csr_rd_addr [alloc_rob]     <= csr_rd_addr;
                rob_csr_w_en    [alloc_rob]     <= csr_w_en;
                rob_csr_w_addr  [alloc_rob]     <= csr_w_addr;
                rob_csr_op      [alloc_rob]     <= csr_op;
                rob_csr_uimm    [alloc_rob]     <= csr_uimm;
                rob_ebreak_ecall_mret[alloc_rob]<= {ebreak_en, ecall_en, mret_en};
            end
        end
    end
endgenerate

// write rob, from writeback
generate
    for (i=0; i<`ROB_DEPTH; i++) begin: GENARATE_ROB_WB
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_branch_ready[i] <= 1'b0;
            end else if (rob_flush) begin
                rob_branch_ready[i] <= 1'b0;
            end else if (wb_br_finish && (wb_br_rob == i)) begin
                rob_branch_ready[i] <= 1'b1; 
            end else if (rob_commit_Paddr == i) begin
                rob_branch_ready[i] <= 1'b0;
            end
        end
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_br_taken[i] <= 1'b0;
                rob_br_addr [i] <= `PC_WIDTH'b0;
            end else if (rob_flush) begin
                rob_br_taken[i] <= 1'b0;
                rob_br_addr[i]  <= `PC_WIDTH'b0;
            end else if (wb_br_finish && (wb_br_rob == i) && wb_br_taken) begin
                rob_br_taken[i] <= 1'b1; 
                rob_br_addr [i] <= wb_br_addr;
            end else if (rob_commit_Paddr == i) begin
                rob_br_taken[i] <= 1'b0;
                rob_br_addr[i]  <= `PC_WIDTH'b0;
            end
        end
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_store_ready[i] <= 1'b0;
            end else if (rob_flush) begin
                rob_store_ready[i] <= 1'b0;
            end else if (store_finish && (wb_mem_dst_Paddr == i)) begin
                rob_store_ready[i] <= 1'b1; 
            end else if (rob_commit_Paddr == i) begin
                rob_store_ready[i] <= 1'b0;
            end
        end
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_dst_value_ready[i]  <= 1'b0;
            end else if (rob_flush) begin                                   // clear
                rob_dst_value_ready[i]  <= 1'b0;
            end else if (wb_alu_valid && (wb_alu_dst_Paddr == i)) begin     // alu
                rob_dst_value[i]        <= wb_alu_out;
                rob_dst_value_ready[i]  <= 1'b1;
            end else if (wb_div_valid && (wb_div_dst_Paddr == i)) begin     // div
                rob_dst_value[i]        <= wb_div_out;
                rob_dst_value_ready[i]  <= 1'b1;
            end else if (wb_mul_valid && (wb_mul_dst_Paddr == i)) begin     // mul
                rob_dst_value[i]        <= wb_mul_out;
                rob_dst_value_ready[i]  <= 1'b1;
            end else if (wb_load_valid && (wb_mem_dst_Paddr == i)) begin    // mem load
                rob_dst_value[i]        <= wb_load_data;
                rob_dst_value_ready[i]  <= 1'b1;
            end else if (wb_br_out_valid && (wb_br_rob == i)) begin         // jump
                rob_dst_value[i]        <= wb_br_out;
                rob_dst_value_ready[i]  <= 1'b1;
            end else if (rob_commit_Paddr == i) begin                       // clear
                rob_dst_value_ready[i]  <= 1'b0;
            end
        end
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_csr_rs1_data[i]     <=  `WORD_WIDTH'b0;
                rob_csr_ready[i]        <=  1'b0;
            end else if (rob_flush) begin
                rob_csr_ready[i]        <=  1'b0;
            end else if (csr_rs1_data_valid && (csr_Pdst == i)) begin
                rob_csr_rs1_data[i]     <=  csr_rs1_data;
                rob_csr_ready[i]        <=  1'b1;
            end else if (rob_commit_Paddr == i) begin
                rob_csr_ready[i]        <=  1'b0;
            end
        end
    end
endgenerate
// -------------------------rs1 and rs2 from ROB---------------------------------

wire rs1_wb_en;
wire rs2_wb_en;
wire [`WORD_WIDTH-1 : 0] rs1_wb_data;
wire [`WORD_WIDTH-1 : 0] rs2_wb_data;

assign rs1_wb_en =  (wb_alu_valid && (wb_alu_dst_Paddr == rs1_Paddr )   )   ||
                    (wb_div_valid && (wb_div_dst_Paddr == rs1_Paddr )   )   ||
                    (wb_mul_valid && (wb_mul_dst_Paddr == rs1_Paddr )   )   ||
                    (wb_load_valid&& (wb_mem_dst_Paddr == rs1_Paddr )   )   ||
                    (wb_br_out_valid && (wb_br_rob  == rs1_Paddr    )   )   ;
assign rs2_wb_en =  (wb_alu_valid && (wb_alu_dst_Paddr == rs2_Paddr )   )   ||
                    (wb_div_valid && (wb_div_dst_Paddr == rs2_Paddr )   )   ||
                    (wb_mul_valid && (wb_mul_dst_Paddr == rs2_Paddr )   )   ||
                    (wb_load_valid&& (wb_mem_dst_Paddr == rs2_Paddr )   )   ||
                    (wb_br_out_valid && (wb_br_rob  == rs2_Paddr    )   )   ;

assign rs1_wb_data =    (wb_alu_valid && (wb_alu_dst_Paddr == rs1_Paddr )   )? wb_alu_out   :
                        (wb_div_valid && (wb_div_dst_Paddr == rs1_Paddr )   )? wb_div_out   :
                        (wb_mul_valid && (wb_mul_dst_Paddr == rs1_Paddr )   )? wb_mul_out   :
                        (wb_load_valid&& (wb_mem_dst_Paddr == rs1_Paddr )   )? wb_load_data :
                        (wb_br_out_valid && (wb_br_rob  == rs1_Paddr    )   )? wb_br_out    : `WORD_WIDTH'b0;

assign rs2_wb_data =    (wb_alu_valid && (wb_alu_dst_Paddr == rs2_Paddr )   )? wb_alu_out   :
                        (wb_div_valid && (wb_div_dst_Paddr == rs2_Paddr )   )? wb_div_out   :
                        (wb_mul_valid && (wb_mul_dst_Paddr == rs2_Paddr )   )? wb_mul_out   :
                        (wb_load_valid&& (wb_mem_dst_Paddr == rs2_Paddr )   )? wb_load_data :
                        (wb_br_out_valid && (wb_br_rob  == rs2_Paddr    )   )? wb_br_out    : `WORD_WIDTH'b0;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        rs1_fromROB_valid <= 1'b0;
        rs1_value_fromROB <= `WORD_WIDTH'b0;
    end  else if (rs1_rat_valid && rs1_wb_en) begin
        rs1_fromROB_valid <= 1'b1;
        rs1_value_fromROB <= rs1_wb_data;
    end else if (rs1_rat_valid && rob_dst_value_ready[rs1_Paddr]) begin
        rs1_fromROB_valid <= 1'b1;
        rs1_value_fromROB <= rob_dst_value[rs1_Paddr];
    end else begin
        rs1_fromROB_valid <= 1'b0;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        rs2_fromROB_valid <= 1'b0;
        rs2_value_fromROB <= `WORD_WIDTH'b0;
    end else if (rs2_rat_valid && rs2_wb_en) begin
        rs2_fromROB_valid <= 1'b1;
        rs2_value_fromROB <= rs2_wb_data;
    end  else if (rs2_rat_valid && rob_dst_value_ready[rs2_Paddr]) begin
        rs2_fromROB_valid <= 1'b1;
        rs2_value_fromROB <= rob_dst_value[rs2_Paddr];
    end else begin
        rs2_fromROB_valid <= 1'b0;
    end
end
//--------------------------- exception ----------------------------------------
generate
    for (i=0; i<`ROB_DEPTH; i++) begin: GENERATE_ROB_EXP
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_exp_en[i] <= 1'b0;
                rob_exp_op[i] <= `ISA_EXP_NO_EXP;
            end else if (rob_flush) begin
                rob_exp_en[i] <= 1'b0;
                rob_exp_op[i] <= `ISA_EXP_NO_EXP;
            end else if ((exp_code_in_decoder != `ISA_EXP_NO_EXP) && allocate_en && !rob_flush && (alloc_rob==i)) begin
                rob_exp_en[i] <= 1'b1;
                rob_exp_op[i] <= exp_code_in_decoder;
            end else if ((exp_code_in_mem_ctrl != `ISA_EXP_NO_EXP)&& !rob_flush && (wb_mem_dst_Paddr==i)) begin
                rob_exp_en[i] <= 1'b1;
                rob_exp_op[i] <= exp_code_in_mem_ctrl;
            end else if (rob_commit_Paddr == i) begin
                rob_exp_en[i] <= 1'b0;
                rob_exp_op[i] <= `ISA_EXP_NO_EXP;
            end
        end
    end
endgenerate

endmodule
`endif
