`ifndef OOOCPU_ALU
`define OOOCPU_ALU
module alu(
    // inputs
    input wire                                  alu_issue_en,
    input wire [`DATA_WIDTH_ALU_OP-1 : 0]       alu_issue_queue_op,
    input wire [`PC_WIDTH-1 : 0]                alu_issue_queue_pc,
    input wire [`WORD_WIDTH-1 : 0]              alu_issue_queue_imm,
    input wire [`WORD_WIDTH-1 : 0]              alu_issue_queue_rs1_value,
    input wire [`WORD_WIDTH-1 : 0]              alu_issue_queue_rs2_value,
    // output
    output reg [`WORD_WIDTH - 1:0]              alu_out,
    output wire                                 alu_out_valid
);

assign alu_out_valid = alu_issue_en;

always @(*) begin
   case (alu_issue_queue_op)
    `ALU_OP_ADDI: begin
        alu_out = alu_issue_queue_imm + alu_issue_queue_rs1_value;
    end
    `ALU_OP_SLTI: begin
        alu_out = ($signed(alu_issue_queue_rs1_value) < $signed(alu_issue_queue_imm))? 1'b1:1'b0;
    end
    `ALU_OP_SLTIU: begin
        alu_out = (alu_issue_queue_rs1_value < alu_issue_queue_imm)? 1'b1:1'b0;
    end
    `ALU_OP_ANDI: begin
        alu_out = alu_issue_queue_imm & alu_issue_queue_rs1_value;
    end
    `ALU_OP_ORI: begin
        alu_out = alu_issue_queue_imm | alu_issue_queue_rs1_value;
    end
    `ALU_OP_XORI: begin
        alu_out = alu_issue_queue_imm ^ alu_issue_queue_rs1_value;
    end
    `ALU_OP_SLLI: begin
        alu_out = alu_issue_queue_rs1_value << alu_issue_queue_imm[4:0];
    end
    `ALU_OP_SRLI: begin
        alu_out = alu_issue_queue_rs1_value >> alu_issue_queue_imm[4:0];
    end
    `ALU_OP_SRAI: begin
        alu_out = alu_issue_queue_rs1_value >>> alu_issue_queue_imm[4:0];
    end
    `ALU_OP_LUI: begin  // imm<<12
        alu_out = alu_issue_queue_imm; // imm has already shift, << 12
    end
    `ALU_OP_AUIPC: begin
        alu_out = alu_issue_queue_pc + (alu_issue_queue_imm << 12);
    end
    `ALU_OP_ADD: begin
        alu_out = alu_issue_queue_rs1_value + alu_issue_queue_rs2_value;
    end
    `ALU_OP_SLT: begin
        alu_out = ($signed(alu_issue_queue_rs1_value) < $signed(alu_issue_queue_rs2_value))? 1:0;
    end
    `ALU_OP_SLTU: begin
        alu_out = (alu_issue_queue_rs1_value < alu_issue_queue_rs2_value)? 1:0;
    end
    `ALU_OP_AND: begin
        alu_out = alu_issue_queue_rs1_value & alu_issue_queue_rs2_value;
    end
    `ALU_OP_OR: begin
        alu_out = alu_issue_queue_rs1_value | alu_issue_queue_rs2_value;
    end
    `ALU_OP_XOR: begin
        alu_out = alu_issue_queue_rs1_value ^ alu_issue_queue_rs2_value;
    end
    `ALU_OP_SLL: begin
        alu_out = alu_issue_queue_rs1_value << alu_issue_queue_rs2_value[4:0];
    end
    `ALU_OP_SRL: begin
        alu_out = alu_issue_queue_rs1_value >> alu_issue_queue_rs2_value[4:0];
    end
    `ALU_OP_SUB: begin
        alu_out = alu_issue_queue_rs1_value - alu_issue_queue_rs2_value;
    end
    `ALU_OP_SRA: begin
        alu_out = alu_issue_queue_rs1_value >>> alu_issue_queue_rs2_value[4:0];
    end
    default: alu_out = `WORD_WIDTH'b0;
   endcase 
end

endmodule
`endif
