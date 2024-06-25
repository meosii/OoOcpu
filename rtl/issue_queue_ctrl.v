`ifndef OOOCPU_ISSUE_QUEUE_CTRL
`define OOOCPU_ISSUE_QUEUE_CTRL
`include "define.v"
`include "issue_queue_OoO.v"
`include "issue_queue_InO.v"
`include "issue_queue_OoO_MD.v"
module issue_queue_ctrl (
    input wire                                  clk,
    input wire                                  rst_n,

    // from id_reg
    input wire [`DATA_WIDTH_ALU_OP - 1 : 0]     id_alu_op,          // include div and mul
    input wire [`DATA_WIDTH_MEM_OP - 1 : 0]     id_mem_op,
    input wire [`DATA_WIDTH_BR_OP - 1 : 0]      id_br_op,
    input wire [`PC_WIDTH-1 : 0]                id_pc,
    input wire [`WORD_WIDTH-1 : 0]              id_imm,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       id_alloc_rob,       // from rob -> if_reg (Pdst)
    // from rat
    input wire                                  rs1_rat_valid,      // when rat_valid==0, rs1_value_fromGPR is valid.
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       rs1_Paddr,
    input wire                                  rs2_rat_valid,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       rs2_Paddr,
    
    // from gpr
    input wire [`WORD_WIDTH-1 : 0]              rs1_value_fromGPR,  // rs1_r -> gpr -> rs1_value
    input wire [`WORD_WIDTH-1 : 0]              rs2_value_fromGPR,

    // from writeback, when rat_valid==1
        // from alu
    input wire  [`GPR_ADDR_WIDTH-1 : 0]         wb_alu_dst_Paddr,
    input wire  [`WORD_WIDTH-1 : 0]             wb_alu_out,
    input wire                                  wb_alu_valid,
        // from div
    input wire  [`GPR_ADDR_WIDTH-1 : 0]         wb_div_dst_Paddr,
    input wire  [`WORD_WIDTH-1 : 0]             wb_div_out,
    input wire                                  wb_div_valid,
        // from mul
    input wire  [`GPR_ADDR_WIDTH-1 : 0]         wb_mul_dst_Paddr,
    input wire  [`WORD_WIDTH-1 : 0]             wb_mul_out,
    input wire                                  wb_mul_valid,
        // from mem
    input wire  [`GPR_ADDR_WIDTH-1 : 0]         wb_load_dst_Paddr,
    input wire  [`WORD_WIDTH-1 : 0]             wb_load_data,
    input wire                                  wb_load_valid,
        // from jal and jalr
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       wb_br_rob,
    input wire [`WORD_WIDTH-1 : 0]              wb_br_out,                  // jal and jalr: rd = pc+4
    input wire                                  wb_br_out_valid,

    // branch and exception
    input wire                                  rob_commit_br_taken,    // branch -> refresh the issue_queue
    input wire                                  rob_commit_exp_en,          // exp -> refresh the issue_queue

    // DIV and MEM ready
    input wire                                  mul_ready,
    input wire                                  div_ready,
    input wire                                  mem_ready,

    // outputs
    // to alu
    output wire                                 alu_issue_en_out,
    output wire [`DATA_WIDTH_ALU_OP-1 : 0]      alu_issue_queue_op_out,
    output wire [`PC_WIDTH-1 : 0]               alu_issue_queue_pc_out,
    output wire [`WORD_WIDTH-1 : 0]             alu_issue_queue_imm_out,
    output wire [`WORD_WIDTH-1 : 0]             alu_issue_queue_rs1_value_out,
    output wire [`WORD_WIDTH-1 : 0]             alu_issue_queue_rs2_value_out,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      alu_issue_queue_Pdst_out,
    // to mul or div
    output wire                                 mul_div_issue_en_out,
    output wire [`DATA_WIDTH_ALU_OP-1 : 0]      mul_div_issue_queue_op_out,
    output wire [`PC_WIDTH-1 : 0]               mul_div_issue_queue_pc_out,
    output wire [`WORD_WIDTH-1 : 0]             mul_div_issue_queue_imm_out,
    output wire [`WORD_WIDTH-1 : 0]             mul_div_issue_queue_rs1_value_out,
    output wire [`WORD_WIDTH-1 : 0]             mul_div_issue_queue_rs2_value_out,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      mul_div_issue_queue_Pdst_out,
    // to mem
    output wire                                 mem_issue_en_out,
    output wire [`DATA_WIDTH_MEM_OP-1 : 0]      mem_issue_queue_op_out,
    output wire [`PC_WIDTH-1 : 0]               mem_issue_queue_pc_out,
    output wire [`WORD_WIDTH-1 : 0]             mem_issue_queue_imm_out,
    output wire [`WORD_WIDTH-1 : 0]             mem_issue_queue_rs1_value_out,
    output wire [`WORD_WIDTH-1 : 0]             mem_issue_queue_rs2_value_out,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      mem_issue_queue_Pdst_out,
    // to br
    output wire                                 br_issue_en_out,
    output wire [`DATA_WIDTH_BR_OP-1 : 0]       br_issue_queue_op_out,
    output wire [`PC_WIDTH-1 : 0]               br_issue_queue_pc_out,
    output wire [`WORD_WIDTH-1 : 0]             br_issue_queue_imm_out,
    output wire [`WORD_WIDTH-1 : 0]             br_issue_queue_rs1_value_out,
    output wire [`WORD_WIDTH-1 : 0]             br_issue_queue_rs2_value_out,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      br_issue_queue_Pdst_out,
    // issue queue full
    output wire                                 stall_in_issue
);

wire    alu_insn_en;
wire    mul_div_insn_en;
wire    mem_insn_en;
wire    br_insn_en;
// from issue queue: dispatch_en?
wire    alu_issue_queue_full;
wire    mul_div_issue_queue_full;
wire    mem_issue_queue_full;
wire    br_issue_queue_full;
// to issue queue
wire    issue2alu_en;
wire    issue2mul_div_en;
wire    issue2mem_en;
wire    issue2br_en;

assign  alu_insn_en     =  (id_alu_op == `ALU_OP_ADDI   ||
                            id_alu_op == `ALU_OP_SLTI   ||
                            id_alu_op == `ALU_OP_SLTIU  ||
                            id_alu_op == `ALU_OP_ANDI   ||
                            id_alu_op == `ALU_OP_ORI    ||
                            id_alu_op == `ALU_OP_XORI   ||
                            id_alu_op == `ALU_OP_SLLI   ||
                            id_alu_op == `ALU_OP_SRLI   ||
                            id_alu_op == `ALU_OP_SRAI   ||
                            id_alu_op == `ALU_OP_LUI    ||
                            id_alu_op == `ALU_OP_AUIPC  ||
                            id_alu_op == `ALU_OP_ADD    ||
                            id_alu_op == `ALU_OP_SLT    ||
                            id_alu_op == `ALU_OP_SLTU   ||
                            id_alu_op == `ALU_OP_AND    ||
                            id_alu_op == `ALU_OP_OR     ||
                            id_alu_op == `ALU_OP_XOR    ||
                            id_alu_op == `ALU_OP_SLL    ||
                            id_alu_op == `ALU_OP_SRL    ||
                            id_alu_op == `ALU_OP_SUB    ||
                            id_alu_op == `ALU_OP_SRA    );

assign  mul_div_insn_en =   (id_alu_op == `ALU_OP_MUL   || 
                            id_alu_op == `ALU_OP_MULH   || 
                            id_alu_op == `ALU_OP_MULHSU || 
                            id_alu_op == `ALU_OP_MULHU  || 
                            id_alu_op == `ALU_OP_DIV    || 
                            id_alu_op == `ALU_OP_DIVU   || 
                            id_alu_op == `ALU_OP_REM    || 
                            id_alu_op == `ALU_OP_REMU   );

assign  mem_insn_en     =   !(id_mem_op == `MEM_OP_NOP);
assign  br_insn_en      =   !(id_br_op == `BR_OP_NOP);


assign  issue2alu_en        =   !alu_issue_queue_full       && alu_insn_en;
assign  issue2mul_div_en    =   !mul_div_issue_queue_full   && mul_div_insn_en;
assign  issue2mem_en        =   !mem_issue_queue_full       && mem_insn_en;
assign  issue2br_en         =   !br_issue_queue_full        && br_insn_en;

assign stall_in_issue   =   (alu_issue_queue_full       && alu_insn_en      ) ||
                            (mul_div_issue_queue_full   && mul_div_insn_en  ) ||
                            (mem_issue_queue_full       && mem_insn_en      ) ||
                            (br_issue_queue_full        && br_insn_en       )  ;

issue_queue_OoO #(
    .OP_WIDTH                   (`DATA_WIDTH_ALU_OP             )
)u_alu_issue_queue_OoO(
    .clk                        (clk                            ),
    .rst_n                      (rst_n                          ),
    // inputs
    .issue2queue_en             (issue2alu_en                   ),
    .id_op                      (id_alu_op                      ),

    .id_pc                      (id_pc                          ),
    .id_imm                     (id_imm                         ),
    .id_alloc_rob               (id_alloc_rob                   ),
    .rs1_rat_valid              (rs1_rat_valid                  ),
    .rs1_Paddr                  (rs1_Paddr                      ),
    .rs2_rat_valid              (rs2_rat_valid                  ),
    .rs2_Paddr                  (rs2_Paddr                      ),
    .rs1_value_fromGPR          (rs1_value_fromGPR              ),
    .rs2_value_fromGPR          (rs2_value_fromGPR              ),
    .wb_alu_dst_Paddr           (wb_alu_dst_Paddr               ),
    .wb_alu_out                 (wb_alu_out                     ),
    .wb_alu_valid               (wb_alu_valid                   ),
    .wb_div_dst_Paddr           (wb_div_dst_Paddr               ),
    .wb_div_out                 (wb_div_out                     ),
    .wb_div_valid               (wb_div_valid                   ),
    .wb_mul_dst_Paddr           (wb_mul_dst_Paddr               ),
    .wb_mul_out                 (wb_mul_out                     ),
    .wb_mul_valid               (wb_mul_valid                   ),
    .wb_load_dst_Paddr          (wb_load_dst_Paddr              ),
    .wb_load_data               (wb_load_data                   ),
    .wb_load_valid              (wb_load_valid                  ),
    .wb_br_rob                  (wb_br_rob                      ),
    .wb_br_out                  (wb_br_out                      ),
    .wb_br_out_valid            (wb_br_out_valid                ),
    .rob_commit_br_taken        (rob_commit_br_taken            ),
    .rob_commit_exp_en          (rob_commit_exp_en              ),
    .fu_ready                   (1'b1                           ),
    // outputs
    .issue_en_out               (alu_issue_en_out               ),
    .issue_queue_op_out         (alu_issue_queue_op_out         ),
    .issue_queue_pc_out         (alu_issue_queue_pc_out         ),
    .issue_queue_imm_out        (alu_issue_queue_imm_out        ),
    .issue_queue_rs1_value_out  (alu_issue_queue_rs1_value_out  ),
    .issue_queue_rs2_value_out  (alu_issue_queue_rs2_value_out  ),
    .issue_queue_Pdst_out       (alu_issue_queue_Pdst_out       ),
    .issue_queue_full           (alu_issue_queue_full           )
);

issue_queue_OoO_MD #(
    .OP_WIDTH                   (`DATA_WIDTH_ALU_OP                 )
)u_mul_div_issue_queue_OoO(
    .clk                        (clk                                ),
    .rst_n                      (rst_n                              ),
    // inputs
    .issue2queue_en             (issue2mul_div_en                   ),
    .id_op                      (id_alu_op                          ),

    .id_pc                      (id_pc                              ),
    .id_imm                     (id_imm                             ),
    .id_alloc_rob               (id_alloc_rob                       ),
    .rs1_rat_valid              (rs1_rat_valid                      ),
    .rs1_Paddr                  (rs1_Paddr                          ),
    .rs2_rat_valid              (rs2_rat_valid                      ),
    .rs2_Paddr                  (rs2_Paddr                          ),
    .rs1_value_fromGPR          (rs1_value_fromGPR                  ),
    .rs2_value_fromGPR          (rs2_value_fromGPR                  ),
    .wb_alu_dst_Paddr           (wb_alu_dst_Paddr                   ),
    .wb_alu_out                 (wb_alu_out                         ),
    .wb_alu_valid               (wb_alu_valid                       ),
    .wb_div_dst_Paddr           (wb_div_dst_Paddr                   ),
    .wb_div_out                 (wb_div_out                         ),
    .wb_div_valid               (wb_div_valid                       ),
    .wb_mul_dst_Paddr           (wb_mul_dst_Paddr                   ),
    .wb_mul_out                 (wb_mul_out                         ),
    .wb_mul_valid               (wb_mul_valid                       ),
    .wb_load_dst_Paddr          (wb_load_dst_Paddr                  ),
    .wb_load_data               (wb_load_data                       ),
    .wb_load_valid              (wb_load_valid                      ),
    .wb_br_rob                  (wb_br_rob                          ),
    .wb_br_out                  (wb_br_out                          ),
    .wb_br_out_valid            (wb_br_out_valid                    ),
    .rob_commit_br_taken        (rob_commit_br_taken                ),
    .rob_commit_exp_en          (rob_commit_exp_en                  ),
    .mul_ready                  (mul_ready                          ),
    .div_ready                  (div_ready                          ),
    // outputs
    .issue_en_out               (mul_div_issue_en_out               ),
    .issue_queue_op_out         (mul_div_issue_queue_op_out         ),
    .issue_queue_pc_out         (mul_div_issue_queue_pc_out         ),
    .issue_queue_imm_out        (mul_div_issue_queue_imm_out        ),
    .issue_queue_rs1_value_out  (mul_div_issue_queue_rs1_value_out  ),
    .issue_queue_rs2_value_out  (mul_div_issue_queue_rs2_value_out  ),
    .issue_queue_Pdst_out       (mul_div_issue_queue_Pdst_out       ),
    .issue_queue_full           (mul_div_issue_queue_full           )
);

issue_queue_InO #(
    .OP_WIDTH                   (`DATA_WIDTH_MEM_OP             )
)u_mem_issue_queue_InO(
    .clk                        (clk                            ),
    .rst_n                      (rst_n                          ),
    // inputs
    .issue2queue_en             (issue2mem_en                   ),
    .id_op                      (id_mem_op                      ),

    .id_pc                      (id_pc                          ),
    .id_imm                     (id_imm                         ),
    .id_alloc_rob               (id_alloc_rob                   ),
    .rs1_rat_valid              (rs1_rat_valid                  ),
    .rs1_Paddr                  (rs1_Paddr                      ),
    .rs2_rat_valid              (rs2_rat_valid                  ),
    .rs2_Paddr                  (rs2_Paddr                      ),
    .rs1_value_fromGPR          (rs1_value_fromGPR              ),
    .rs2_value_fromGPR          (rs2_value_fromGPR              ),
    .wb_alu_dst_Paddr           (wb_alu_dst_Paddr               ),
    .wb_alu_out                 (wb_alu_out                     ),
    .wb_alu_valid               (wb_alu_valid                   ),
    .wb_div_dst_Paddr           (wb_div_dst_Paddr               ),
    .wb_div_out                 (wb_div_out                     ),
    .wb_div_valid               (wb_div_valid                   ),
    .wb_mul_dst_Paddr           (wb_mul_dst_Paddr               ),
    .wb_mul_out                 (wb_mul_out                     ),
    .wb_mul_valid               (wb_mul_valid                   ),
    .wb_load_dst_Paddr          (wb_load_dst_Paddr              ),
    .wb_load_data               (wb_load_data                   ),
    .wb_load_valid              (wb_load_valid                  ),
    .wb_br_rob                  (wb_br_rob                      ),
    .wb_br_out                  (wb_br_out                      ),
    .wb_br_out_valid            (wb_br_out_valid                ),
    .rob_commit_br_taken        (rob_commit_br_taken            ),
    .rob_commit_exp_en          (rob_commit_exp_en              ),
    .fu_ready                   (mem_ready                      ),
    // outputs
    .issue_en_out               (mem_issue_en_out               ),
    .issue_queue_op_out         (mem_issue_queue_op_out         ),
    .issue_queue_pc_out         (mem_issue_queue_pc_out         ),
    .issue_queue_imm_out        (mem_issue_queue_imm_out        ),
    .issue_queue_rs1_value_out  (mem_issue_queue_rs1_value_out  ),
    .issue_queue_rs2_value_out  (mem_issue_queue_rs2_value_out  ),
    .issue_queue_Pdst_out       (mem_issue_queue_Pdst_out       ),
    .issue_queue_full           (mem_issue_queue_full           )
);

issue_queue_InO #(
    .OP_WIDTH                   (`DATA_WIDTH_BR_OP              )
)u_br_issue_queue_InO(
    .clk                        (clk                            ),
    .rst_n                      (rst_n                          ),
    // inputs
    .issue2queue_en             (issue2br_en                    ),
    .id_op                      (id_br_op                       ),
    .id_pc                      (id_pc                          ),
    .id_imm                     (id_imm                         ),
    .id_alloc_rob               (id_alloc_rob                   ),
    .rs1_rat_valid              (rs1_rat_valid                  ),
    .rs1_Paddr                  (rs1_Paddr                      ),
    .rs2_rat_valid              (rs2_rat_valid                  ),
    .rs2_Paddr                  (rs2_Paddr                      ),
    .rs1_value_fromGPR          (rs1_value_fromGPR              ),
    .rs2_value_fromGPR          (rs2_value_fromGPR              ),
    .wb_alu_dst_Paddr           (wb_alu_dst_Paddr               ),
    .wb_alu_out                 (wb_alu_out                     ),
    .wb_alu_valid               (wb_alu_valid                   ),
    .wb_div_dst_Paddr           (wb_div_dst_Paddr               ),
    .wb_div_out                 (wb_div_out                     ),
    .wb_div_valid               (wb_div_valid                   ),
    .wb_mul_dst_Paddr           (wb_mul_dst_Paddr               ),
    .wb_mul_out                 (wb_mul_out                     ),
    .wb_mul_valid               (wb_mul_valid                   ),
    .wb_load_dst_Paddr          (wb_load_dst_Paddr              ),
    .wb_load_data               (wb_load_data                   ),
    .wb_load_valid              (wb_load_valid                  ),
    .wb_br_rob                  (wb_br_rob                      ),
    .wb_br_out                  (wb_br_out                      ),
    .wb_br_out_valid            (wb_br_out_valid                ),
    .rob_commit_br_taken        (rob_commit_br_taken            ),
    .rob_commit_exp_en          (rob_commit_exp_en              ),
    .fu_ready                   (1'b1                           ),
    // outputs
    .issue_en_out               (br_issue_en_out                ),
    .issue_queue_op_out         (br_issue_queue_op_out          ),
    .issue_queue_pc_out         (br_issue_queue_pc_out          ),
    .issue_queue_imm_out        (br_issue_queue_imm_out         ),
    .issue_queue_rs1_value_out  (br_issue_queue_rs1_value_out   ),
    .issue_queue_rs2_value_out  (br_issue_queue_rs2_value_out   ),
    .issue_queue_Pdst_out       (br_issue_queue_Pdst_out        ),
    .issue_queue_full           (br_issue_queue_full            )
);

endmodule
`endif
