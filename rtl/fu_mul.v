`ifndef FU_MUL
`define FU_MUL
`include "mul.v"
`include "define.v"
module fu_mul#(
    parameter   SIGNED_WORD_WIDTH,
    parameter   PARTIAL_PRODUCT_WIDTH
)(
    input wire                                  clk,
    input wire                                  rst_n,
    input wire                                  mul_div_issue_en,
    input wire [`DATA_WIDTH_ALU_OP-1 : 0]       mul_div_issue_queue_op,
    input wire [`WORD_WIDTH-1 : 0]              mul_div_issue_queue_rs1_value,
    input wire [`WORD_WIDTH-1 : 0]              mul_div_issue_queue_rs2_value,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       mul_div_issue_queue_Pdst,
    // mul writeback
    output reg [`WORD_WIDTH-1 : 0]              mul_out,
    output wire                                 mul_out_valid,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      mul_dst_Paddr,
    output wire                                 mul_ready
);
// mul
wire                                mul_start;
wire [2:0]                          mul_opcode;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_result;

assign mul_opcode = (mul_div_issue_queue_op == `ALU_OP_MUL      )? `MUL_OP_MUL      : 
                    (mul_div_issue_queue_op == `ALU_OP_MULH     )? `MUL_OP_MULH     :
                    (mul_div_issue_queue_op == `ALU_OP_MULHU    )? `MUL_OP_MULHU    :
                    (mul_div_issue_queue_op == `ALU_OP_MULHSU   )? `MUL_OP_MULHSU   : `MUL_OP_NOP;

assign mul_start  = (mul_opcode != `MUL_OP_NOP) && mul_div_issue_en;

mul #(
    .SIGNED_WORD_WIDTH          (SIGNED_WORD_WIDTH              ),
    .PARTIAL_PRODUCT_WIDTH      (PARTIAL_PRODUCT_WIDTH          )
) u_alu_mul(
    .clk                        (clk                            ),
    .rst_n                      (rst_n                          ),
    .mul_en                     (mul_start                      ),
    .mul_opcode                 (mul_opcode                     ),
    .mul_data1                  (mul_div_issue_queue_rs1_value  ),
    .mul_data2                  (mul_div_issue_queue_rs2_value  ),
    .mul_result                 (mul_result                     ),
    .mul_result_valid           (mul_out_valid                  )
);
reg [`DATA_WIDTH_ALU_OP-1 : 0]  mul_op_r;
reg [$clog2(`ROB_DEPTH)-1 : 0]  mul_Pdst_r;

assign mul_dst_Paddr = (mul_out_valid)? mul_Pdst_r : 'b0;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        mul_Pdst_r  <= 'b0;
        mul_op_r    <= `MUL_OP_NOP;
    end else if (mul_start) begin
        mul_Pdst_r  <= mul_div_issue_queue_Pdst;
        mul_op_r    <= mul_div_issue_queue_op;
    end else if (mul_out_valid) begin
        mul_Pdst_r <= 'b0;
        mul_op_r    <= `MUL_OP_NOP;
    end
end


always @(*) begin
   case (mul_op_r)
    `ALU_OP_MUL:begin
        mul_out = mul_result;
    end
    `ALU_OP_MULH, `ALU_OP_MULHSU, `ALU_OP_MULHU:begin
        mul_out = mul_result[`WORD_WIDTH+`WORD_WIDTH-1 : `WORD_WIDTH];
    end
    default: begin
        mul_out = `WORD_WIDTH'b0;
    end
   endcase 
end

// --------------------------------mul could issue ?----------------------------
reg mul_ready_tmp;
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        mul_ready_tmp <= 1'b1;
    end else if (mul_start) begin
        mul_ready_tmp <= 1'b0;
    end else if (mul_out_valid) begin
        mul_ready_tmp <= 1'b1;
    end
end

assign mul_ready = mul_ready_tmp || mul_out_valid;

endmodule
`endif
