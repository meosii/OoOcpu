`ifndef OOOCPU_GPR
`define OOOCPU_GPR
//General Purpose Registers

module gpr(
    input wire                                      clk,
    input wire                                      rst_n,
    input wire                                      commit_en,
    input wire  [`GPR_ADDR_WIDTH - 1 : 0]           rob_commit_dst_addr,
    input wire  [`WORD_WIDTH - 1:0]                 rob_commit_dst_value,
    input wire  [`GPR_ADDR_WIDTH - 1 : 0]           rs1_addr,
    input wire  [`GPR_ADDR_WIDTH - 1 : 0]           rs2_addr,
    input wire  [`GPR_ADDR_WIDTH - 1 : 0]           jp_rs1_addr,
    // outputs
    output wire [`WORD_WIDTH - 1 : 0]               rs1_value,
    output wire [`WORD_WIDTH - 1 : 0]               rs2_value,
    // jp
    output wire [`WORD_WIDTH - 1 : 0]               jp_rs1_data
);

reg [`WORD_WIDTH - 1 : 0] gpr [0 : `DATA_HIGH_GPR - 1];

assign rs1_value = (commit_en && (rob_commit_dst_addr == rs1_addr))? rob_commit_dst_value : gpr[rs1_addr];
assign rs2_value = (commit_en && (rob_commit_dst_addr == rs2_addr))? rob_commit_dst_value : gpr[rs2_addr];
assign jp_rs1_data  = gpr[jp_rs1_addr];

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        gpr[0]  <=  `WORD_WIDTH'b0;
    end
end

genvar i;
generate for (i=1; i<32; i++) begin: GENERATE_RAT
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            gpr[i]  <=  `WORD_WIDTH'b0;
        end else if (commit_en && (rob_commit_dst_addr==i)) begin
            gpr[i]  <=  rob_commit_dst_value;
        end
    end
end
endgenerate

endmodule
`endif
