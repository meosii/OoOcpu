`ifndef OOOCPU_CTRL
`define OOOCPU_CTRL
module cpu_ctrl(
    input wire  if_en,
    input wire  rob_full,           // rob full
    input wire  stall_in_issue,     // issue_queue full
    input wire  stall_in_decoder,   // jp_rs1 data not get
    input wire  jp_taken,           // jump
    input wire  rob_commit_br_taken,// branch

    // outputs
    output wire allocate_en,
    output wire pc_stall,
    output wire if_stall,
    output wire id_stall,
    output wire if_flush,
    output wire id_flush

);

assign allocate_en = if_en && !if_stall;

// stall and flush
assign pc_stall = rob_full || stall_in_issue || stall_in_decoder;
assign if_stall = rob_full || stall_in_issue || stall_in_decoder;
assign id_stall = rob_full || stall_in_issue;

assign if_flush = jp_taken || rob_commit_br_taken;
assign id_flush = rob_commit_br_taken;

endmodule
`endif
