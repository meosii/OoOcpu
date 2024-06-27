`ifndef OOOCPU_ID_REG
`define OOOCPU_ID_REG
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
    // from rat
    input wire                                  rs1_rat_valid, 
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       rs1_Paddr,
    input wire                                  rs2_rat_valid,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       rs2_Paddr,
    input wire [`WORD_WIDTH-1 : 0]              rs1_value_fromGPR,
    input wire [`WORD_WIDTH-1 : 0]              rs2_value_fromGPR,
    // outputs
    output reg [`PC_WIDTH-1 : 0]                id_pc,
    output reg  [`GPR_ADDR_WIDTH-1 : 0]         id_rs1_addr,
    output reg  [`GPR_ADDR_WIDTH-1 : 0]         id_rs2_addr,
    output reg [`DATA_WIDTH_ALU_OP - 1 : 0]     id_alu_op, 
    output reg [`DATA_WIDTH_MEM_OP-1 :0]        id_mem_op,
    output reg [`DATA_WIDTH_BR_OP-1 :0]         id_br_op,
    output reg [`WORD_WIDTH-1 : 0]              id_imm,
    output reg [$clog2(`ROB_DEPTH)-1 : 0]       id_alloc_rob,
    output reg                                  id_en,
    output reg                                  id_rs1_rat_valid,
    output reg [$clog2(`ROB_DEPTH)-1 : 0]       id_rs1_Paddr,
    output reg                                  id_rs2_rat_valid,
    output reg [$clog2(`ROB_DEPTH)-1 : 0]       id_rs2_Paddr,
    output reg [`WORD_WIDTH-1 : 0]              id_rs1_value_fromGPR,
    output reg [`WORD_WIDTH-1 : 0]              id_rs2_value_fromGPR
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
        id_rs1_rat_valid    <=  1'b0;
        id_rs1_Paddr        <=  {$clog2(`ROB_DEPTH){1'b0}};
        id_rs2_rat_valid    <=  1'b0;
        id_rs2_Paddr        <=  {$clog2(`ROB_DEPTH){1'b0}};
        id_rs1_value_fromGPR<=  `WORD_WIDTH'b0;
        id_rs2_value_fromGPR<=  `WORD_WIDTH'b0;
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
            id_rs1_rat_valid    <=  1'b0;
            id_rs1_Paddr        <=  {$clog2(`ROB_DEPTH){1'b0}};
            id_rs2_rat_valid    <=  1'b0;
            id_rs2_Paddr        <=  {$clog2(`ROB_DEPTH){1'b0}};
            id_rs1_value_fromGPR<=  `WORD_WIDTH'b0;
            id_rs2_value_fromGPR<=  `WORD_WIDTH'b0;
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
            id_rs1_rat_valid    <=  rs1_rat_valid;
            id_rs1_Paddr        <=  rs1_Paddr;
            id_rs2_rat_valid    <=  rs2_rat_valid;
            id_rs2_Paddr        <=  rs2_Paddr;
            id_rs1_value_fromGPR<=  rs1_value_fromGPR;
            id_rs2_value_fromGPR<=  rs2_value_fromGPR;
        end
    end
end
endmodule
`endif
