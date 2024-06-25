`include "define.v"
`include "issue_queue_4.v"
module tb_alu_issue_queue();
    reg                                  clk;
    reg                                  rst_n;
    
    // from issue queue ctrl
    reg                                  issue2alu_en;
    reg [`DATA_WIDTH_ALU_OP - 1 : 0]     id_alu_op;      // include div and mul
    reg [`WORD_WIDTH-1 : 0]              id_insn;
    reg [`PC_WIDTH-1 : 0]                id_pc;
    reg [`WORD_WIDTH-1 : 0]              id_imm;
    reg [$clog2(`ROB_DEPTH)-1 : 0]       id_alloc_rob; // from rob -> if_reg

    reg                                  rs1_rat_valid;  // when rat_valid==0; rs1_value_fromGPR is valid.
    reg [$clog2(`ROB_DEPTH)-1 : 0]       rs1_Paddr;
    reg                                  rs2_rat_valid;
    reg [$clog2(`ROB_DEPTH)-1 : 0]       rs2_Paddr;
    reg [`WORD_WIDTH-1 : 0]              rs1_value_fromGPR;
    reg [`WORD_WIDTH-1 : 0]              rs2_value_fromGPR;
    
    // from writeback; when rat_valid==1
        // from alu
    reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_alu_dst_Paddr;
    reg  [`WORD_WIDTH-1 : 0]             wb_alu_out;
    reg                                  wb_alu_valid;
        // from div
    reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_div_dst_Paddr;
    reg  [`WORD_WIDTH-1 : 0]             wb_div_out;
    reg                                  wb_div_valid;
        // from mul
    reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_mul_dst_Paddr;
    reg  [`WORD_WIDTH-1 : 0]             wb_mul_out;
    reg                                  wb_mul_valid;
        // from mem
    reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_load_dst_Paddr;
    reg  [`WORD_WIDTH-1 : 0]             wb_load_data;
    reg                                  wb_load_valid;

    // branch and exception
    reg                                  rob_commit_branch_taken;    // branch -> refresh the issue_queue
    reg                                  rob_commit_exp_en;          // exp -> refresh the issue_queue

    // outputs
    wire                                 alu_issue_en;
    wire [`DATA_WIDTH_ALU_OP-1 : 0]      issue_queue_alu_op_out;
    wire [`PC_WIDTH-1 : 0]               issue_queue_pc_out;
    wire [`WORD_WIDTH-1 : 0]             issue_queue_imm_out;
    wire [`WORD_WIDTH-1 : 0]             issue_queue_rs1_value_out;
    wire [`WORD_WIDTH-1 : 0]             issue_queue_rs2_value_out;

    wire                                 issue_queue_full;

issue_queue_4 #(
    .OP_WIDTH                   (`DATA_WIDTH_ALU_OP         )
)u_issue_queue_4(
    .clk                        (clk                        ),
    .rst_n                      (rst_n                      ),
    .issue2queue_en             (issue2alu_en               ),
    .id_op                      (id_alu_op                  ),
    .id_pc                      (id_pc                      ),
    .id_imm                     (id_imm                     ),
    .id_alloc_rob               (id_alloc_rob               ),
    .rs1_rat_valid              (rs1_rat_valid              ),
    .rs1_Paddr                  (rs1_Paddr                  ),
    .rs2_rat_valid              (rs2_rat_valid              ),
    .rs2_Paddr                  (rs2_Paddr                  ),
    .rs1_value_fromGPR          (rs1_value_fromGPR          ),
    .rs2_value_fromGPR          (rs2_value_fromGPR          ),
    .wb_alu_dst_Paddr           (wb_alu_dst_Paddr           ),
    .wb_alu_out                 (wb_alu_out                 ),
    .wb_alu_valid               (wb_alu_valid               ),
    .wb_div_dst_Paddr           (wb_div_dst_Paddr           ),
    .wb_div_out                 (wb_div_out                 ),
    .wb_div_valid               (wb_div_valid               ),
    .wb_mul_dst_Paddr           (wb_mul_dst_Paddr           ),
    .wb_mul_out                 (wb_mul_out                 ),
    .wb_mul_valid               (wb_mul_valid               ),
    .wb_load_dst_Paddr          (wb_load_dst_Paddr          ),
    .wb_load_data               (wb_load_data               ),
    .wb_load_valid              (wb_load_valid              ),
    .rob_commit_branch_taken    (rob_commit_branch_taken    ),
    .rob_commit_exp_en          (rob_commit_exp_en          ),
    .issue_en_out               (alu_issue_en               ),
    .issue_queue_op_out         (issue_queue_alu_op_out     ),
    .issue_queue_pc_out         (issue_queue_pc_out         ),
    .issue_queue_imm_out        (issue_queue_imm_out        ),
    .issue_queue_rs1_value_out  (issue_queue_rs1_value_out  ),
    .issue_queue_rs2_value_out  (issue_queue_rs2_value_out  ),
    .issue_queue_full           (issue_queue_full           )
);

always #5 clk = ~clk;

initial begin
    #0 begin
        clk = 0;
        rst_n = 0;
    end
    #3 begin
        rst_n = 1;
    end
    #10
    @(posedge clk)
    #1 begin    // add x3, x2, x1   (x2=10, x1=20)
       issue2alu_en          = 1'b1;
       id_alu_op             = `ALU_OP_ADD;
       id_insn[`ALL_TYPE_OPCODE] = `OP;
       id_pc                 = 4;
       id_imm                = 0;
       id_alloc_rob          = 0;
       rs1_rat_valid         = 0;
       rs1_Paddr             = 0;
       rs2_rat_valid         = 0;
       rs2_Paddr             = 0;
       rs1_value_fromGPR     = 10;
       rs2_value_fromGPR     = 20;
       wb_alu_dst_Paddr      = 0;
       wb_alu_out            = 0;
       wb_alu_valid          = 0;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    @(posedge clk)
    #1 begin    // and x4, x3, x1
       issue2alu_en          = 1'b1;
       id_alu_op             = `ALU_OP_AND;
       id_insn[`ALL_TYPE_OPCODE] = `OP;
       id_pc                 = 8;
       id_imm                = 0;
       id_alloc_rob          = 1;
       rs1_rat_valid         = 1;
       rs1_Paddr             = 0;
       rs2_rat_valid         = 0;
       rs2_Paddr             = 0;
       rs1_value_fromGPR     = 0;
       rs2_value_fromGPR     = 20;
       wb_alu_dst_Paddr      = 0;
       wb_alu_out            = 30;
       wb_alu_valid          = 1;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    @(posedge clk)
    #1 begin    // addi x5, x4, 16
       issue2alu_en          = 1'b1;
       id_alu_op             = `ALU_OP_ADDI;
       id_insn[`ALL_TYPE_OPCODE] = `OP_IMM;
       id_pc                 = 12;
       id_imm                = 16;
       id_alloc_rob          = 2;
       rs1_rat_valid         = 1;
       rs1_Paddr             = 1;
       rs2_rat_valid         = 0;
       rs2_Paddr             = 0;
       rs1_value_fromGPR     = 0;
       rs2_value_fromGPR     = 0;
       wb_alu_dst_Paddr      = 0;
       wb_alu_out            = 0;
       wb_alu_valid          = 0;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    @(posedge clk)
    #1 begin    // sub x3, x5, x4
       issue2alu_en          = 1'b1;
       id_alu_op             = `ALU_OP_SUB;
       id_insn[`ALL_TYPE_OPCODE] = `OP;
       id_pc                 = 16;
       id_imm                = 0;
       id_alloc_rob          = 3;
       rs1_rat_valid         = 1;
       rs1_Paddr             = 2;
       rs2_rat_valid         = 0;
       rs2_Paddr             = 0;
       rs1_value_fromGPR     = 0;
       rs2_value_fromGPR     = 20;
       wb_alu_dst_Paddr      = 0;
       wb_alu_out            = 0;
       wb_alu_valid          = 0;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    @(posedge clk)
    #1 begin    // lui x10 8 (x10=8<<12)
       issue2alu_en          = 1'b1;
       id_alu_op             = `ALU_OP_LUI;
       id_insn[`ALL_TYPE_OPCODE] = `OP_LUI;
       id_pc                 = 20;
       id_imm                = 8;
       id_alloc_rob          = 4;
       rs1_rat_valid         = 0;
       rs1_Paddr             = 0;
       rs2_rat_valid         = 0;
       rs2_Paddr             = 0;
       rs1_value_fromGPR     = 0;
       rs2_value_fromGPR     = 0;
       wb_alu_dst_Paddr      = 0;
       wb_alu_out            = 0;
       wb_alu_valid          = 0;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
// branch
   @(posedge clk)
    #1 begin    // lui x10 2 (x10=2<<12)
       issue2alu_en          = 1'b1;
       id_alu_op             = `ALU_OP_LUI;
       id_insn[`ALL_TYPE_OPCODE] = `OP_LUI;
       id_pc                 = 20;
       id_imm                = 2;
       id_alloc_rob          = 4;
       rs1_rat_valid         = 0;
       rs1_Paddr             = 0;
       rs2_rat_valid         = 0;
       rs2_Paddr             = 0;
       rs1_value_fromGPR     = 0;
       rs2_value_fromGPR     = 0;
       wb_alu_dst_Paddr      = 0;
       wb_alu_out            = 0;
       wb_alu_valid          = 0;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 1;
       rob_commit_exp_en     = 0;
    end



    @(posedge clk)
    #1 begin    //auipc x10, 4 (x10=pc+imm<<12)
       issue2alu_en          = 1'b1;
       id_alu_op             = `ALU_OP_AUIPC;
       id_insn[`ALL_TYPE_OPCODE] = `OP_AUIPC;
       id_pc                 = 24;
       id_imm                = 4;
       id_alloc_rob          = 5;
       rs1_rat_valid         = 0;
       rs1_Paddr             = 0;
       rs2_rat_valid         = 0;
       rs2_Paddr             = 0;
       rs1_value_fromGPR     = 0;
       rs2_value_fromGPR     = 0;
       wb_alu_dst_Paddr      = 1;
       wb_alu_out            = 20;
       wb_alu_valid          = 1;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    @(posedge clk)
    #1 begin    // add x1, x10,x5
       issue2alu_en          = 1'b1;
       id_alu_op             = `ALU_OP_ADD;
       id_insn[`ALL_TYPE_OPCODE] = `OP;
       id_pc                 = 28;
       id_imm                = 0;
       id_alloc_rob          = 6;
       rs1_rat_valid         = 1;
       rs1_Paddr             = 5;
       rs2_rat_valid         = 0;
       rs2_Paddr             = 0;
       rs1_value_fromGPR     = 0;
       rs2_value_fromGPR     = 36;
       wb_alu_dst_Paddr      = 2;
       wb_alu_out            = 36;
       wb_alu_valid          = 1;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    @(posedge clk)
    #1 begin
       issue2alu_en          = 1'b0;
       id_alu_op             = `ALU_OP_NOP;
       wb_alu_dst_Paddr      = 3;
       wb_alu_out            = 16;
       wb_alu_valid          = 1;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    #10
    @(posedge clk)
    #1 begin
       wb_alu_dst_Paddr      = 0;
       wb_alu_out            = 0;
       wb_alu_valid          = 0;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    #10
    @(posedge clk)
    #1 begin
       wb_alu_dst_Paddr      = 4;
       wb_alu_out            = 32768;
       wb_alu_valid          = 1;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    @(posedge clk)
    #1 begin
       wb_alu_dst_Paddr      = 0;
       wb_alu_out            = 0;
       wb_alu_valid          = 0;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    
@(posedge clk)
    #1 begin
       wb_alu_dst_Paddr      = 5;
       wb_alu_out            = 16408;
       wb_alu_valid          = 1;
       wb_div_dst_Paddr      = 0;
       wb_div_out            = 0;
       wb_div_valid          = 0;
       wb_mul_dst_Paddr      = 0;
       wb_mul_out            = 0;
       wb_mul_valid          = 0;
       wb_load_dst_Paddr     = 0;
       wb_load_data          = 0;
       wb_load_valid         = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
    end
    #20 $finish;
end

initial begin
    $fsdbDumpfile("wave_alu_issue_queue.fsdb");
    $fsdbDumpvars();
    $fsdbDumpMDA();
end

endmodule
