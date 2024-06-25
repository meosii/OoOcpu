`ifndef OOOCPU_ROB
`define OOOCPU_ROB
`include "define.v"
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
            // store
    input wire                              store_finish,

    // branch
    input wire                              wb_br_finish,  // the branch instruction has judged whether jump and jump to where
    input wire                              wb_br_taken,   // whether jump or not
    input wire  [`PC_WIDTH-1 : 0]           wb_br_addr,    // jump to where
    input wire  [`ROB_DEPTH-1 : 0]          wb_br_rob,     // which instruction
    
    // exception
    input wire  [$clog2(`ROB_DEPTH)-1 : 0]  exp_rob,
    input wire                              exp_en,
    input wire  [`DATA_WIDTH_ISA_EXP-1 : 0] exp_op,

    // outputs

    // rob allocate
        // To rat
    output wire [$clog2(`ROB_DEPTH)-1 : 0]  rob_alloc_tag_2rat,         // Paddr
    output wire [`GPR_ADDR_WIDTH-1 : 0]     rob_alloc_dst_addr_2rat,    // Aaddr
    output wire                             rob_alloc_dst_wen_2rat,     // whether write
    
    // rob commit
        // To rat
    output wire                             commit_en,
    output wire [`GPR_ADDR_WIDTH-1 : 0]     rob_commit_dst_addr_2rat,   // Aaddr
    output wire                             rob_commit_br_taken,        // branch -> refresh the rat
    output wire [`PC_WIDTH-1 : 0]           rob_commit_br_addr,         // branch -> refresh the rat
    output wire                             rob_commit_exp_en,          // exp -> refresh the rat
        // To gpr
    output wire [`WORD_WIDTH-1 : 0]         rob_commit_dst_value_2rat,  // data

    // to id_reg to issue queue
    output wire [$clog2(`ROB_DEPTH)-1 : 0]  alloc_rob,                  // wptr, Paddr
    // stall the pipeline
    output wire                             rob_full
);

genvar i;

reg [$clog2(`ROB_DEPTH) : 0]  wptr; // [5:0],the highest bit is used to determine empty or full
reg [$clog2(`ROB_DEPTH) : 0]  rptr;

wire [$clog2(`ROB_DEPTH) : 0] wptr_next;
wire [$clog2(`ROB_DEPTH) : 0] rptr_next;

wire [$clog2(`ROB_DEPTH)-1 : 0]   commit_rob;

wire    rob_empty;

// rob reg
    // from allocate
reg [`PC_WIDTH-1 : 0]           rob_pc              [0 : `ROB_DEPTH-1];
reg [`GPR_ADDR_WIDTH-1 : 0]     rob_dst_addr        [0 : `ROB_DEPTH-1];
reg                             rob_dst_wen         [0 : `ROB_DEPTH-1];
reg [`GPR_ADDR_WIDTH-1 : 0]     rob_rs1_addr        [0 : `ROB_DEPTH-1];
reg [`GPR_ADDR_WIDTH-1 : 0]     rob_rs2_addr        [0 : `ROB_DEPTH-1];
    // from writeback
reg [`WORD_WIDTH-1 : 0]         rob_dst_value       [0 : `ROB_DEPTH-1];
reg                             rob_dst_value_ready [0 : `ROB_DEPTH-1];
reg                             rob_store_ready     [0 : `ROB_DEPTH-1]; // instruction store finished
reg                             rob_branch_ready    [0 : `ROB_DEPTH-1]; // instruction judgement finished
reg                             rob_br_taken        [0 : `ROB_DEPTH-1]; // branch
reg [`PC_WIDTH-1 : 0]           rob_br_addr         [0 : `ROB_DEPTH-1]; // branch
    // expection
reg                             rob_exp_en          [0 : `ROB_DEPTH-1];
reg [`DATA_WIDTH_ISA_EXP-1 : 0] rob_exp_op          [0 : `ROB_DEPTH-1];


always @(posedge clk or negedge rst_n) begin
    if (~rst_n) begin
        wptr <= 'b0;
        rptr <= 'b0;
    end else begin
        wptr <= wptr_next;
        rptr <= rptr_next;
    end
end

assign alloc_rob    = wptr[$clog2(`ROB_DEPTH)-1 : 0];
assign commit_rob   = rptr[$clog2(`ROB_DEPTH)-1 : 0];

assign wptr_next =  (rob_commit_br_taken)?  commit_rob+1:
                    (allocate_en        )?  wptr + 1    :   wptr;

assign rptr_next =  (commit_en)? rptr + 1   :   rptr;

assign rob_empty =  (wptr == rptr);
assign rob_full  =  (wptr[$clog2(`ROB_DEPTH)] == rptr[$clog2(`ROB_DEPTH)]) &&
                    (wptr[$clog2(`ROB_DEPTH)-1 : 0] == rptr[$clog2(`ROB_DEPTH)-1 : 0]);

// allocate to rat
assign rob_alloc_tag_2rat = alloc_rob;
assign rob_alloc_dst_addr_2rat = dst_addr;
assign rob_alloc_dst_wen_2rat = dst_wen;

// ROB commit to rat and gpr
assign commit_en                    =   rob_dst_value_ready [commit_rob]    || 
                                        rob_store_ready     [commit_rob]    ||
                                        rob_branch_ready    [commit_rob]    ;

assign rob_commit_dst_addr_2rat     =   rob_dst_addr        [commit_rob]    ;
assign rob_commit_br_taken          =   rob_br_taken        [commit_rob]    ;
assign rob_commit_br_addr           =   rob_br_addr         [commit_rob]    ;
assign rob_commit_exp_en            =   rob_exp_en          [commit_rob]    ;

// ROB commit to Physical regfile
assign rob_commit_dst_value_2rat    =   rob_dst_value       [commit_rob]    ;

// write rob, from allocate
always @(posedge clk) begin
    if (allocate_en) begin
        rob_pc          [alloc_rob]    <= pc; 
        rob_dst_addr    [alloc_rob]    <= dst_addr;
        rob_dst_wen     [alloc_rob]    <= dst_wen;
        rob_rs1_addr    [alloc_rob]    <= rs1_addr;
        rob_rs2_addr    [alloc_rob]    <= rs2_addr;
    end
end

// write rob, from writeback
generate
    for (i=0; i<`ROB_DEPTH; i++) begin: GENARATE_ROB_WB
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_branch_ready[i] <= 1'b0;
            end else if (wb_br_finish && (wb_br_rob == i)) begin
                rob_branch_ready[i] <= 1'b1; 
            end
        end
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_br_taken[i] <= 1'b0;
                rob_br_addr [i] <= `PC_WIDTH'b0;
            end else if (wb_br_finish && (wb_br_rob == i) && wb_br_taken) begin
                rob_br_taken[i] <= 1'b1; 
                rob_br_addr [i] <= wb_br_addr;
            end
        end
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_store_ready[i] <= 1'b0;
            end else if (store_finish && (wb_mem_dst_Paddr == i)) begin
                rob_store_ready[i] <= 1'b1; 
            end
        end
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                rob_dst_value_ready[i] <= 1'b0;
            end else if (wb_alu_valid && (wb_alu_dst_Paddr == i)) begin     // alu
                rob_dst_value[i]        <= wb_alu_out;
                rob_dst_value_ready[i]  <= 1'b1;
            end else if (wb_div_valid && (wb_div_dst_Paddr == i)) begin     // div
                rob_dst_value[i]        <= wb_div_out;
                rob_dst_value_ready[i]  <= 1'b1;
            end else if (wb_mul_valid && (wb_mul_dst_Paddr == i)) begin     // mul
                rob_dst_value[i]        <= wb_mul_out;
                rob_dst_value_ready[i]  <= 1'b1;
            end else if (wb_load_valid && (wb_mem_dst_Paddr == i)) begin   // mem load
                rob_dst_value[i]        <= wb_load_data;
                rob_dst_value_ready[i]  <= 1'b1;
            end
        end
    end
endgenerate

// exception
generate
    for (i=0; i<`ROB_DEPTH; i++) begin: GENERATE_ROB_EXP
        always @(posedge clk) begin
            if (!rst_n) begin
                rob_exp_en[i] <= 1'b0;
                rob_exp_op[i] <= 'b0;
            end else if (exp_rob == i && exp_en) begin
                rob_exp_en[i] <= 1'b1;
                rob_exp_op[i] <= exp_op;
            end
        end
    end
endgenerate


endmodule
`endif
