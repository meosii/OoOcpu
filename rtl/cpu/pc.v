`ifndef OOOCPU_PC
`define OOOCPU_PC
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
    input wire [2:0]                        rob_commit_ebreak_ecall_mret,
    output reg [`PC_WIDTH-1 : 0]            pc
);
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        pc <= `PC_WIDTH'b0;
    end else if (cpu_en) begin
        if (trap_happened || rob_commit_ebreak_ecall_mret[0]) begin
            pc <= ctrl_pc;
        end else if (br_taken) begin
            pc <= br_addr;
        end else if (jp_taken) begin
            pc <= jp_addr;
        end else if (!pc_stall) begin
            pc <= pc + 4;
        end
    end
end

endmodule
`endif
