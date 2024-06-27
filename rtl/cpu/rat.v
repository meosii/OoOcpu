`ifndef OOOCPU_RAT
`define OOOCPU_RAT
// Register alias table
// maintains the mapping between architectural registers and physical registers.
// If there is a mapping in rat, either the value has not been computed,
// or the valued has been computed and stored in rob and has not been retired.
module rat(
    input wire                              clk,
    input wire                              rst_n,

    // rs1 and rs2 read rat
    input wire  [`GPR_ADDR_WIDTH-1 : 0]     id_rs1_addr, // gpr addr
    input wire  [`GPR_ADDR_WIDTH-1 : 0]     id_rs2_addr, // gpr addr
    input wire  [`GPR_ADDR_WIDTH-1 : 0]     jp_rs1_addr,
    
    // from rob allocate
    input wire                              allocate_en,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]   rob_alloc_tag_2rat,       // Paddr
    input wire [`GPR_ADDR_WIDTH-1 : 0]      rob_alloc_dst_addr_2rat,  // Aaddr
    input wire                              rob_alloc_dst_wen_2rat,   // whether write

    // from rob commit
    input wire                              commit_en,
    input wire [`GPR_ADDR_WIDTH-1 : 0]      rob_commit_dst_addr_2rat,  // Aaddr
    // branch
    input wire                              rob_commit_br_taken,   // refresh the rat
    // exception
    input wire                              rob_commit_exp_en,

    // output to issue_queue
    // rs1 and rs2
    output wire                             rs1_rat_valid,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]  rs1_Paddr,
    output wire                             rs2_rat_valid,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]  rs2_Paddr,
    // jump
    output wire                             jp_rs1_rat_valid,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]  jp_rs1_Paddr
);

reg [$clog2(`ROB_DEPTH)-1 : 0]  rat_Paddr   [31 : 0];
reg                             rat_valid   [31 : 0];

genvar i;
generate for (i=0; i<32; i++) begin: GENERATE_RAT
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            rat_Paddr[i] <= 'b0;
            rat_valid[i] <= 1'b0;
        end else if (rob_commit_br_taken || rob_commit_exp_en) begin
            rat_Paddr[i] <= 'b0;
            rat_valid[i] <= 1'b0;
        end else if (allocate_en && (rob_alloc_dst_addr_2rat==i) && rob_alloc_dst_wen_2rat) begin
            rat_Paddr[i] <=  rob_alloc_tag_2rat;
            rat_valid[i] <= 1'b1;    // now has mapping between A and P
        end else if (commit_en && (rob_commit_dst_addr_2rat==i)) begin
            rat_Paddr[i] <=  'b0;
            rat_valid[i] <= 1'b0;    // not mapping between A and P
        end
    end
end
endgenerate

// read rs1 and rs2
assign rs1_rat_valid    = (id_rs1_addr == 'b0)? 1'b0 : rat_valid[id_rs1_addr];
assign rs1_Paddr        = (id_rs1_addr == 'b0)? 'b0 : rat_Paddr[id_rs1_addr];
assign rs2_rat_valid    = (id_rs2_addr == 'b0)? 1'b0 : rat_valid[id_rs2_addr];
assign rs2_Paddr        = (id_rs2_addr == 'b0)? 'b0 : rat_Paddr[id_rs2_addr];

assign jp_rs1_rat_valid = (jp_rs1_addr == 'b0)? 1'b0 : rat_valid[jp_rs1_addr];
assign jp_rs1_Paddr     = (jp_rs1_addr == 'b0)? 'b0 : rat_Paddr[jp_rs1_addr];

endmodule
`endif

