`ifndef OOOCPU_PC
`define OOOCPU_PC
`include "define.v"
module pc (
    input wire                              clk,
    input wire                              rst_n,
    input wire                              cpu_en,
    input wire                              pc_stall,
    // jump and branch
    input wire                              br_taken,
    input wire [`PC_WIDTH-1 : 0]            br_addr,
    input wire [`PC_WIDTH-1 : 0]            jp_addr,
    input wire                              jp_taken,
    // cpu_ctrl
    input wire [`PC_WIDTH-1 : 0]            ctrl_pc,
    input wire                              trap_happened,
    input wire                              mret_en, // from decoder
    output reg [`PC_WIDTH-1 : 0]            pc
);
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        pc <= `PC_WIDTH'b0;
    end else if (cpu_en) begin
        if (trap_happened || mret_en) begin
            pc <= ctrl_pc;
        end else if (br_taken && !pc_stall) begin
            pc <= br_addr;
        end else if (jp_taken && !pc_stall) begin
            pc <= jp_addr;
        end else if (!pc_stall) begin
            pc <= pc + 4;
        end
    end
end

endmodule
`endif
