`ifndef OOOCPU_CSR_CTRL
`define OOOCPU_CSR_CTRL
module csr_ctrl(
    // from rob
    input reg [`DATA_WIDTH_CSR_OP-1 : 0]    rob_commit_csr_op,
    input reg [4:0]                         rob_commit_csr_uimm,
    input reg [`WORD_WIDTH - 1 : 0]         rob_commit_csr_rs1_data,
    // from csr
    input wire [`WORD_WIDTH - 1 : 0]        csr_rd_data,
    // writeback
    input wire [$clog2(`ROB_DEPTH)-1 : 0]   rob_commit_Paddr,           // Paddr
    input wire [`WORD_WIDTH-1 : 0]          rob_commit_dst_value_2rat,  // csr wb_data
    // to csr
    output wire [`WORD_WIDTH - 1 : 0]       commit_csr_w_data,
    // to issue queue
    output wire [`WORD_WIDTH - 1 : 0]       wb_csr_data,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]  wb_csr_dst_Paddr,
    output wire                             wb_csr_valid
);

assign commit_csr_w_data = (rob_commit_csr_op == `CSR_OP_CSRRW )?  rob_commit_csr_rs1_data              ://t = CSRs[csr]; CSRs[csr] = x[rs1]; x[rd] = t
                    (rob_commit_csr_op == `CSR_OP_CSRRS )?  csr_rd_data | rob_commit_csr_rs1_data       ://t = CSRs[csr]; CSRs[csr] = t | x[rs1]; x[rd] = t
                    (rob_commit_csr_op == `CSR_OP_CSRRC )?  csr_rd_data & ~rob_commit_csr_rs1_data      ://t = CSRs[csr]; CSRs[csr] = t &~x[rs1]; x[rd] = t
                    (rob_commit_csr_op == `CSR_OP_CSRRWI)?  {27'b0,rob_commit_csr_uimm}                 ://x[rd] = CSRs[csr]; CSRs[csr] = zimm
                    (rob_commit_csr_op == `CSR_OP_CSRRSI)?  csr_rd_data | {27'b0,rob_commit_csr_uimm}   ://t = CSRs[csr]; CSRs[csr] = t | zimm; x[rd] = t
                    (rob_commit_csr_op == `CSR_OP_CSRRCI)?  csr_rd_data & ~{27'b0,rob_commit_csr_uimm}  ://t = CSRs[csr]; CSRs[csr] = t &~zimm; x[rd] = t
                                                            `WORD_WIDTH'b0;


//-----------------------writeback-------------------------------------------

assign wb_csr_data = rob_commit_dst_value_2rat;

assign wb_csr_dst_Paddr = rob_commit_Paddr;

assign wb_csr_valid = !((rob_commit_csr_op == `CSR_OP_NOP) || (rob_commit_csr_op == `CSR_OP_ECALL_EBREAK));

endmodule
`endif
