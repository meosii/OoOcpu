`ifndef OOOCPU_CTRL
`define OOOCPU_CTRL
`include "define.v"
module cpu_ctrl(
    input wire  rob_full,           // rob full
    input wire  stall_in_issue,     // issue_queue full
    input wire  stall_in_decoder,   // jp_rs1 data not got
    input wire  jp_taken,           // jump
    input wire  rob_commit_br_taken,// branch

    // outputs
    output wire pc_stall,
    output wire if_stall,
    output wire id_stall,
    output wire if_flush,
    output wire id_flush

);

assign pc_stall = rob_full || stall_in_issue || stall_in_decoder;
assign if_stall = rob_full || stall_in_issue || stall_in_decoder;
assign id_stall = rob_full || stall_in_issue;

assign if_flush = jp_taken || rob_commit_br_taken;
assign id_flush = jp_taken || rob_commit_br_taken || rob_full || stall_in_issue || stall_in_decoder;

endmodule
`endif
