`ifndef OOOCPU_IF_REG
`define OOOCPU_IF_REG

`include "define.v"

module if_reg (
    input wire                          clk,
    input wire                          rst_n,
    input wire                          cpu_en,
    input wire                          if_stall,
    input wire                          if_flush,
    input wire [`PC_WIDTH-1 : 0]        pc,
    input wire [`WORD_WIDTH - 1 : 0]    insn,
    // outputs
    output reg [`PC_WIDTH-1 : 0]        if_pc,
    output wire [`WORD_WIDTH - 1 : 0]   if_insn,
    output reg                          if_en
);

reg                         if_flush_r1;
reg                         if_stall_r1;
reg [`WORD_WIDTH - 1 : 0]   insn_stall;

assign if_insn =    (if_flush_r1)? `WORD_WIDTH'b0   :
                    (if_stall_r1)? insn_stall       :   insn;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        insn_stall <= `WORD_WIDTH'b0;
    end else if (if_stall) begin
        insn_stall <= if_insn;
    end
end                    

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        if_flush_r1 <= 1'b0;
        if_stall_r1 <= 1'b0;
    end else begin
        if_flush_r1 <= if_flush;
        if_stall_r1 <= if_stall;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        if_en               <= 1'b0;
        if_pc               <= `PC_WIDTH'b0;
    end else if (cpu_en) begin
        if (if_flush) begin
            if_en               <= 1'b0;
            if_pc               <= `PC_WIDTH'b0;
        end else if (!if_stall) begin
            if_en               <= 1'b1;
            if_pc               <= pc;
        end
    end
end

endmodule
`endif
