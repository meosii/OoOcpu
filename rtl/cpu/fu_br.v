`ifndef FU_BR
`define FU_BR
module fu_br(

    // to br
    input wire                                  br_issue_en,
    input wire [`DATA_WIDTH_BR_OP-1 : 0]        br_issue_queue_op,
    input wire [`PC_WIDTH-1 : 0]                br_issue_queue_pc,
    input wire [`WORD_WIDTH-1 : 0]              br_issue_queue_imm,
    input wire [`WORD_WIDTH-1 : 0]              br_issue_queue_rs1_value,
    input wire [`WORD_WIDTH-1 : 0]              br_issue_queue_rs2_value,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       br_issue_queue_Pdst,
    // outputs
    output wire                                 br_finish,
    output wire                                 br_taken,
    output wire [`PC_WIDTH-1 : 0]               br_addr,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      br_rob,
    output wire [`WORD_WIDTH-1 : 0]             br_out, // jal and jalr: rd = pc+4
    output wire                                 br_out_valid
);

assign br_finish = br_issue_en;

assign br_taken =   br_issue_en &&
                (   ((br_issue_queue_op == `BR_OP_BEQ)  && (br_issue_queue_rs1_value == br_issue_queue_rs2_value))                      ||
                    ((br_issue_queue_op == `BR_OP_BNE)  && (br_issue_queue_rs1_value != br_issue_queue_rs2_value))                      ||
                    ((br_issue_queue_op == `BR_OP_BLT)  && ($signed(br_issue_queue_rs1_value)  < $signed(br_issue_queue_rs2_value)))    ||
                    ((br_issue_queue_op == `BR_OP_BLTU) && (br_issue_queue_rs1_value  < br_issue_queue_rs2_value))                      ||
                    ((br_issue_queue_op == `BR_OP_BGE)  && ($signed(br_issue_queue_rs1_value) >= $signed(br_issue_queue_rs2_value)))    ||
                    ((br_issue_queue_op == `BR_OP_BGEU) && (br_issue_queue_rs1_value >= br_issue_queue_rs2_value))                          );

assign br_addr      =   br_issue_queue_pc + br_issue_queue_imm;
assign br_rob       =   br_issue_queue_Pdst;
assign br_out       =   ((br_issue_queue_op == `BR_OP_JAL) || (br_issue_queue_op == `BR_OP_JALR))?  br_issue_queue_pc + 4   : `WORD_WIDTH'b0;
assign br_out_valid =   (br_issue_queue_op == `BR_OP_JAL) || (br_issue_queue_op == `BR_OP_JALR);

endmodule
`endif
