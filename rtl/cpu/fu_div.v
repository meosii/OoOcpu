`ifndef FU_DIV
`define FU_DIV
module fu_div(

    input wire                                  clk,
    input wire                                  rst_n,
    // to mul or div
    input wire                                  mul_div_issue_en,
    input wire [`DATA_WIDTH_ALU_OP-1 : 0]       mul_div_issue_queue_op,
    input wire [`WORD_WIDTH-1 : 0]              mul_div_issue_queue_rs1_value,
    input wire [`WORD_WIDTH-1 : 0]              mul_div_issue_queue_rs2_value,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       mul_div_issue_queue_Pdst,

    // div writeback
    output reg [`WORD_WIDTH-1 : 0]              div_out,
    output wire                                 div_out_valid,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      div_dst_Paddr,
    output wire                                 div_ready
);

// div
wire                                div_start;
wire [2:0]                          div_opcode;
wire [`WORD_WIDTH-1 : 0]            div_quotient;   // from div
wire [`WORD_WIDTH-1 : 0]            div_remainder;  // from div

assign div_opcode = ((mul_div_issue_queue_op == `ALU_OP_DIV) || (mul_div_issue_queue_op == `ALU_OP_REM)     )? `DIV_OP_DIV  : 
                    ((mul_div_issue_queue_op == `ALU_OP_DIVU) || (mul_div_issue_queue_op == `ALU_OP_REMU)   )? `DIV_OP_DIVU : `DIV_OP_NOP;

assign div_start = ((div_opcode == `DIV_OP_DIV) || (div_opcode == `DIV_OP_DIVU)) && mul_div_issue_en;


div u_alu_div(
    .clk                        (clk                            ),
    .rst_n                      (rst_n                          ),
    .div_start                  (div_start                      ),
    .div_opcode                 (div_opcode                     ),
    .div_divident               (mul_div_issue_queue_rs1_value  ),
    .div_divisor                (mul_div_issue_queue_rs2_value  ),
    .div_quotient               (div_quotient                   ),
    .div_remainder              (div_remainder                  ),
    .div_finish                 (div_out_valid                  )
);

reg [`DATA_WIDTH_ALU_OP-1 : 0]  div_op_r;
reg [$clog2(`ROB_DEPTH)-1 : 0]  div_Pdst_r;

assign div_dst_Paddr = (div_out_valid)? div_Pdst_r : 'b0;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        div_Pdst_r  <= 'b0;
        div_op_r    <= `DIV_OP_NOP;
    end else if (div_start) begin
        div_Pdst_r  <= mul_div_issue_queue_Pdst;
        div_op_r    <= mul_div_issue_queue_op;
    end else if (div_out_valid) begin
        div_Pdst_r  <= 'b0;
        div_op_r    <= `DIV_OP_NOP;
    end
end

always @(*) begin
   case (div_op_r)
    `ALU_OP_DIV, `ALU_OP_DIVU: begin
        div_out = div_quotient;
    end
    `ALU_OP_REM, `ALU_OP_REMU: begin
        div_out = div_remainder;
    end
    default: begin
        div_out = `WORD_WIDTH'b0;
    end
   endcase 
end

//------------------------- div could issue ? -----------------------
reg div_ready_tmp;
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        div_ready_tmp <= 1'b1;
    end else if (div_start) begin
        div_ready_tmp <= 1'b0;
    end else if (div_out_valid) begin
        div_ready_tmp <= 1'b1;
    end
end

assign div_ready = div_ready_tmp || div_out_valid;

endmodule
`endif
