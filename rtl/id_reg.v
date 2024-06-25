`ifndef OOOCPU_ID_REG
`define OOOCPU_ID_REG
`include "define.v"
module id_reg(
    input wire                                  clk,
    input wire                                  rst_n,
    input wire                                  cpu_en,
    input wire                                  id_stall,
    input wire                                  id_flush,
    // from if_reg
    input wire [`PC_WIDTH-1 : 0]                if_pc,
    // from decoder
    input wire  [`GPR_ADDR_WIDTH-1 : 0]         rs1_addr,
    input wire  [`GPR_ADDR_WIDTH-1 : 0]         rs2_addr,
    input wire [`DATA_WIDTH_ALU_OP - 1 : 0]     alu_op, 
    input wire [`DATA_WIDTH_MEM_OP-1 :0]        mem_op,
    input wire [`DATA_WIDTH_BR_OP-1 :0]         br_op,
    input wire [`WORD_WIDTH-1 : 0]              imm,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       alloc_rob,
    // outputs
    output reg [`PC_WIDTH-1 : 0]                id_pc,
    output reg  [`GPR_ADDR_WIDTH-1 : 0]         id_rs1_addr,
    output reg  [`GPR_ADDR_WIDTH-1 : 0]         id_rs2_addr,
    output reg [`DATA_WIDTH_ALU_OP - 1 : 0]     id_alu_op, 
    output reg [`DATA_WIDTH_MEM_OP-1 :0]        id_mem_op,
    output reg [`DATA_WIDTH_BR_OP-1 :0]         id_br_op,
    output reg [`WORD_WIDTH-1 : 0]              id_imm,
    output reg [$clog2(`ROB_DEPTH)-1 : 0]       id_alloc_rob,
    output reg                                  id_en
);

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        id_en               <=  1'b0;
        id_pc               <=  `PC_WIDTH'b0;
        id_rs1_addr         <=  `GPR_ADDR_WIDTH'b0;
        id_rs2_addr         <=  `GPR_ADDR_WIDTH'b0;
        id_alu_op           <=  `DATA_WIDTH_ALU_OP'b0;
        id_mem_op           <=  `DATA_WIDTH_MEM_OP'b0;
        id_br_op            <=  `DATA_WIDTH_BR_OP'b0;
        id_imm              <=  `WORD_WIDTH'b0;
        id_alloc_rob        <=  {$clog2(`ROB_DEPTH){1'b0}};
    end else if (cpu_en) begin
        if (id_flush) begin
            id_en               <= 1'b0;
            id_pc               <= `PC_WIDTH'b0;
            id_rs1_addr         <=  `GPR_ADDR_WIDTH'b0;
            id_rs2_addr         <=  `GPR_ADDR_WIDTH'b0;
            id_alu_op           <=  `DATA_WIDTH_ALU_OP'b0;
            id_mem_op           <=  `DATA_WIDTH_MEM_OP'b0;
            id_br_op            <=  `DATA_WIDTH_BR_OP'b0;
            id_imm              <=  `WORD_WIDTH'b0;
            id_alloc_rob        <=  {$clog2(`ROB_DEPTH){1'b0}};
        end else if (!id_stall) begin
            id_en               <= 1'b1;
            id_pc               <= if_pc;
            id_rs1_addr         <= rs1_addr; 
            id_rs2_addr         <=  rs2_addr;
            id_alu_op           <=  alu_op;
            id_mem_op           <=  mem_op;
            id_br_op            <=  br_op;
            id_imm              <=  imm;
            id_alloc_rob        <=  alloc_rob;
        end
    end
end
endmodule
`endif
