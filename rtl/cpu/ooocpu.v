`ifndef OOOCPU
`define OOOCPU
module ooocpu(
    input wire                          clk,
    input wire                          rst_n,
    input wire                          cpu_en,
    // int req
    input wire                          irq_external,
    input wire                          irq_timer,
    input wire                          irq_software,
    // insn and pc
    input  wire [`WORD_WIDTH - 1 : 0]   insn,
    output wire                         rd_insn_en,
    output wire [`PC_WIDTH - 1 : 0]     pc,
    // from AHB
    input wire [`WORD_WIDTH - 1 : 0]    CPU_HRDATA,
    input wire                          CPU_HREADY,
    input wire [1 : 0]                  CPU_HRESP,
    // from spm
    input wire [`WORD_WIDTH - 1 : 0]    spm_rd_data,
    // to AHB
    output wire [`WORD_WIDTH - 1 : 0]   CPU_HADDR,
    output wire                         CPU_HWRITE,
    output wire [2 : 0]                 CPU_HSIZE,
    output wire [2 : 0]                 CPU_HBURST,
    output wire [1 : 0]                 CPU_HTRANS,
    output wire                         CPU_HMASTLOCK,
    output wire [`WORD_WIDTH - 1 : 0]   CPU_HWDATA,
    // to spm
    output wire [3 : 0]                 spm_store_byteena,
    output wire [`WORD_WIDTH - 1 : 0]   spm_write_data,
    output wire [`WORD_WIDTH - 1 : 0]   spm_rdaddress,
    output wire                         spm_rden,
    output wire [`WORD_WIDTH - 1 : 0]   spm_wraddress,
    output wire                         spm_wren,
    // int clear
    output wire                         external_int_clear,
    output wire                         software_int_clear,
    output wire                         timer_int_clear
);

assign rd_insn_en = (cpu_en)? 1'b1:1'b0;

    wire                                pc_stall;
    wire                                if_stall;
    wire                                id_stall;
    wire                                if_flush;
    wire                                id_flush;
    wire [`PC_WIDTH-1 : 0]              if_pc;
    wire [`WORD_WIDTH - 1 : 0]          if_insn;
    wire                                if_en;

    wire  [`GPR_ADDR_WIDTH-1 : 0]       id_rs1_addr; // gpr addr
    wire  [`GPR_ADDR_WIDTH-1 : 0]       id_rs2_addr; // gpr addr
    wire [`DATA_WIDTH_ALU_OP - 1 : 0]   id_alu_op;      // include div and mul
    wire [`DATA_WIDTH_MEM_OP-1 :0]      id_mem_op;
    wire [`DATA_WIDTH_BR_OP-1 :0]       id_br_op;
    wire [`PC_WIDTH-1 : 0]              id_pc;
    wire [`WORD_WIDTH-1 : 0]            id_imm;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     id_alloc_rob; // from rob -> if_wire
    wire                                id_rs1_rat_valid;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     id_rs1_Paddr;
    wire                                id_rs2_rat_valid;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     id_rs2_Paddr;
    wire [`WORD_WIDTH-1 : 0]            id_rs1_value_fromGPR;  // rs1 -> gpr -> rs1_value
    wire [`WORD_WIDTH-1 : 0]            id_rs2_value_fromGPR;
    // cpu_ctrl
    wire [`PC_WIDTH-1 : 0]              ctrl_pc;
    wire                                trap_happened;
    // decoder
    wire                                jp_rs1_rat_valid;   // from rat
    wire [$clog2(`ROB_DEPTH)-1 : 0]     jp_rs1_Paddr;       // from rat
    wire [`WORD_WIDTH - 1 : 0]          jp_rs1_data_fromGPR;// from gpr
    wire [`GPR_ADDR_WIDTH - 1 : 0]      rs1_addr;
    wire [`GPR_ADDR_WIDTH - 1 : 0]      rs2_addr;
    wire [`GPR_ADDR_WIDTH - 1 : 0]      dst_addr;
    wire                                dst_wen;
    wire [`DATA_WIDTH_ALU_OP - 1 : 0]   alu_op;
    wire [`DATA_WIDTH_MEM_OP - 1 : 0]   mem_op;
    wire [`DATA_WIDTH_BR_OP - 1 : 0]    br_op;
    wire [`WORD_WIDTH-1 : 0]            imm;
    wire                                jp_taken;
    wire [`PC_WIDTH-1 : 0]              jp_addr;
    wire [`GPR_ADDR_WIDTH-1 : 0]        jp_rs1_addr;    // to rat; judge if the data can be got from gpr
    wire                                stall_in_decoder;   // wait get the rs1 value
    wire                                ebreak_en;
    wire                                ecall_en;
    wire                                mret_en;
    wire [`DATA_WIDTH_ISA_EXP - 1 : 0]  exp_code_in_decoder;

    wire                                allocate_en;

    // writeback
        // from alu
    wire  [`GPR_ADDR_WIDTH-1 : 0]       wb_alu_dst_Paddr;
    wire  [`WORD_WIDTH-1 : 0]           wb_alu_out;
    wire                                wb_alu_valid;
        // from div
    wire  [`GPR_ADDR_WIDTH-1 : 0]       wb_div_dst_Paddr;
    wire  [`WORD_WIDTH-1 : 0]           wb_div_out;
    wire                                wb_div_valid;
        // from mul
    wire  [`GPR_ADDR_WIDTH-1 : 0]       wb_mul_dst_Paddr;
    wire  [`WORD_WIDTH-1 : 0]           wb_mul_out;
    wire                                wb_mul_valid;
        // from mem
    wire  [`GPR_ADDR_WIDTH-1 : 0]       wb_mem_dst_Paddr;
    wire  [`WORD_WIDTH-1 : 0]           wb_load_data;
    wire                                wb_load_valid;
        // jump and branch
    wire                                wb_br_finish;  // the branch instruction has judged whether jump and jump to where
    wire                                wb_br_taken;
    wire [`PC_WIDTH-1 : 0]              wb_br_addr;
    wire  [`ROB_DEPTH-1 : 0]            wb_br_rob;      // which instruction
    wire [`WORD_WIDTH-1 : 0]            wb_br_out;      // jal and jalr: rd = pc+4
    wire                                wb_br_out_valid;
    // store
    wire                                store_finish;
    wire  [`ROB_DEPTH-1 : 0]            store_rob;      // which instruction

    // exception
    wire  [$clog2(`ROB_DEPTH)-1 : 0]    exp_rob;
    wire                                exp_en;
    wire  [`DATA_WIDTH_ISA_EXP-1 : 0]   exp_op;
    // rob allocate
        // To rat
    wire [$clog2(`ROB_DEPTH)-1 : 0]     rob_alloc_tag_2rat;         // Paddr
    wire [`GPR_ADDR_WIDTH-1 : 0]        rob_alloc_dst_addr_2rat;    // Aaddr
    wire                                rob_alloc_dst_wen_2rat;     // whether write
    
    // rob commit
        // To rat
    wire                                commit_en;
    wire [`GPR_ADDR_WIDTH-1 : 0]        rob_commit_dst_addr_2rat;   // Aaddr
    wire                                rob_commit_br_taken;        // branch -> refresh the rat
    wire [`PC_WIDTH-1 : 0]              rob_commit_br_addr;
    wire                                rob_commit_exp_en;          // exp -> refresh the rat
        // To gpr
    wire [`WORD_WIDTH-1 : 0]            rob_commit_dst_value_2rat;  // data
    // to id_wire to issue queue
    wire [$clog2(`ROB_DEPTH)-1 : 0]     alloc_rob;                  // wptr; Paddr
    wire                                rob_full;
    wire                                rs1_fromROB_valid;
    wire                                rs2_fromROB_valid;
    wire [`WORD_WIDTH-1 : 0]            rs1_value_fromROB;
    wire [`WORD_WIDTH-1 : 0]            rs2_value_fromROB;
    // rs1 and rs2
    wire                                rs1_rat_valid;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     rs1_Paddr;
    wire                                rs2_rat_valid;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     rs2_Paddr;

    wire [`WORD_WIDTH-1 : 0]            rs1_value_fromGPR;
    wire [`WORD_WIDTH-1 : 0]            rs2_value_fromGPR;
    wire                                mul_ready;
    wire                                div_ready;
    wire                                mem_ready;
    // to alu
    wire                                alu_issue_en;
    wire [`DATA_WIDTH_ALU_OP-1 : 0]     alu_issue_queue_op;
    wire [`PC_WIDTH-1 : 0]              alu_issue_queue_pc;
    wire [`WORD_WIDTH-1 : 0]            alu_issue_queue_imm;
    wire [`WORD_WIDTH-1 : 0]            alu_issue_queue_rs1_value;
    wire [`WORD_WIDTH-1 : 0]            alu_issue_queue_rs2_value;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     alu_issue_queue_Pdst;
    // to mul or div
    wire                                mul_div_issue_en;
    wire [`DATA_WIDTH_ALU_OP-1 : 0]     mul_div_issue_queue_op;
    wire [`PC_WIDTH-1 : 0]              mul_div_issue_queue_pc;
    wire [`WORD_WIDTH-1 : 0]            mul_div_issue_queue_imm;
    wire [`WORD_WIDTH-1 : 0]            mul_div_issue_queue_rs1_value;
    wire [`WORD_WIDTH-1 : 0]            mul_div_issue_queue_rs2_value;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     mul_div_issue_queue_Pdst;
    // to mem
    wire                                mem_issue_en;
    wire [`DATA_WIDTH_MEM_OP-1 : 0]     mem_issue_queue_op;
    wire [`PC_WIDTH-1 : 0]              mem_issue_queue_pc;
    wire [`WORD_WIDTH-1 : 0]            mem_issue_queue_imm;
    wire [`WORD_WIDTH-1 : 0]            mem_issue_queue_rs1_value;
    wire [`WORD_WIDTH-1 : 0]            mem_issue_queue_rs2_value;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     mem_issue_queue_Pdst;
    // to br
    wire                                br_issue_en;
    wire [`DATA_WIDTH_BR_OP-1 : 0]      br_issue_queue_op;
    wire [`PC_WIDTH-1 : 0]              br_issue_queue_pc;
    wire [`WORD_WIDTH-1 : 0]            br_issue_queue_imm;
    wire [`WORD_WIDTH-1 : 0]            br_issue_queue_rs1_value;
    wire [`WORD_WIDTH-1 : 0]            br_issue_queue_rs2_value;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     br_issue_queue_Pdst;

    // exp_code
    wire [`DATA_WIDTH_ISA_EXP - 1 : 0]  exp_code_in_mem_ctrl;

    wire                                stall_in_issue;

pc u_pc(
    .clk            (clk                ),
    .rst_n          (rst_n              ),
    .cpu_en         (cpu_en             ),
    .pc_stall       (pc_stall           ),
    .br_taken       (rob_commit_br_taken),
    .br_addr        (rob_commit_br_addr ),  
    .jp_taken       (jp_taken           ),
    .jp_addr        (jp_addr            ),  
    .ctrl_pc        (ctrl_pc            ),
    .trap_happened  (trap_happened      ),
    .mret_en        (mret_en            ),
    //output
    .pc             (pc                 )
);

if_reg u_if_reg(
    .clk                    (clk                    ),
    .rst_n                  (rst_n                  ),
    .cpu_en                 (cpu_en                 ),
    .if_stall               (if_stall               ),
    .if_flush               (if_flush               ),
    .pc                     (pc                     ),
    .insn                   (insn                   ),
    // outputs
    .if_pc                  (if_pc                  ),
    .if_insn                (if_insn                ),
    .if_en                  (if_en                  )
);

decoder u_decoder(

    .if_en                  (if_en                  ),
    .if_pc                  (if_pc                  ),
    .if_insn                (if_insn                ),
    .jp_rs1_rat_valid       (jp_rs1_rat_valid       ),
    .jp_rs1_data_fromGPR    (jp_rs1_data_fromGPR    ),
    // outputs
    .rs1_addr               (rs1_addr               ),
    .rs2_addr               (rs2_addr               ),
    .dst_addr               (dst_addr               ),
    .dst_wen                (dst_wen                ),
    .alu_op                 (alu_op                 ),
    .mem_op                 (mem_op                 ),
    .br_op                  (br_op                  ),
    .imm                    (imm                    ),
    .jp_taken               (jp_taken               ),
    .jp_addr                (jp_addr                ),
    .jp_rs1_addr            (jp_rs1_addr            ),
    .stall_in_decoder       (stall_in_decoder       ),
    .ebreak_en              (ebreak_en              ),
    .ecall_en               (ecall_en               ),
	.mret_en                (mret_en                ),
    .exp_code               (exp_code_in_decoder    )
);

rob u_rob(
    .clk                        (clk                        ),
    .rst_n                      (rst_n                      ),
    .allocate_en                (allocate_en                ),
    .pc                         (if_pc                      ),
    .dst_addr                   (dst_addr                   ),
    .dst_wen                    (dst_wen                    ),  
    .rs1_addr                   (rs1_addr                   ),
    .rs2_addr                   (rs2_addr                   ),
    .rs1_rat_valid              (rs1_rat_valid              ),
    .rs1_Paddr                  (rs1_Paddr                  ),
    .rs2_rat_valid              (rs2_rat_valid              ),
    .rs2_Paddr                  (rs2_Paddr                  ),
    .wb_alu_dst_Paddr           (wb_alu_dst_Paddr           ),
    .wb_alu_out                 (wb_alu_out                 ),
    .wb_alu_valid               (wb_alu_valid               ),
    .wb_div_dst_Paddr           (wb_div_dst_Paddr           ),
    .wb_div_out                 (wb_div_out                 ),
    .wb_div_valid               (wb_div_valid               ),
    .wb_mul_dst_Paddr           (wb_mul_dst_Paddr           ),
    .wb_mul_out                 (wb_mul_out                 ),
    .wb_mul_valid               (wb_mul_valid               ),
    .wb_mem_dst_Paddr           (wb_mem_dst_Paddr           ),
    .wb_load_data               (wb_load_data               ),
    .wb_load_valid              (wb_load_valid              ),
    .wb_br_finish               (wb_br_finish               ),
    .wb_br_taken                (wb_br_taken                ),
    .wb_br_addr                 (wb_br_addr                 ),
    .wb_br_rob                  (wb_br_rob                  ),
    .wb_br_out                  (wb_br_out                  ),
    .wb_br_out_valid            (wb_br_out_valid            ),
    .store_finish               (store_finish               ),
    .exp_rob                    (exp_rob                    ),
    .exp_en                     (exp_en                     ),
    .exp_op                     (exp_op                     ),
    // outputs
    .rob_alloc_tag_2rat         (rob_alloc_tag_2rat         ),
    .rob_alloc_dst_addr_2rat    (rob_alloc_dst_addr_2rat    ),
    .rob_alloc_dst_wen_2rat     (rob_alloc_dst_wen_2rat     ),
    .commit_en                  (commit_en                  ),
    .rob_commit_dst_addr_2rat   (rob_commit_dst_addr_2rat   ),
    .rob_commit_br_taken        (rob_commit_br_taken        ),
    .rob_commit_br_addr         (rob_commit_br_addr         ),
    .rob_commit_exp_en          (rob_commit_exp_en          ),
    .rob_commit_dst_value_2rat  (rob_commit_dst_value_2rat  ),
    .alloc_rob                  (alloc_rob                  ),
    .rob_full                   (rob_full                   ),
    .rs1_fromROB_valid          (rs1_fromROB_valid          ),
    .rs2_fromROB_valid          (rs2_fromROB_valid          ),
    .rs1_value_fromROB          (rs1_value_fromROB          ),
    .rs2_value_fromROB          (rs2_value_fromROB          )

);

rat u_rat(
    .clk                        (clk                        ),
    .rst_n                      (rst_n                      ),
    .id_rs1_addr                (rs1_addr                ),
    .id_rs2_addr                (rs2_addr                ),
    .jp_rs1_addr                (jp_rs1_addr                ),
    .allocate_en                (allocate_en                ),
    .rob_alloc_tag_2rat         (rob_alloc_tag_2rat         ),
    .rob_alloc_dst_addr_2rat    (rob_alloc_dst_addr_2rat    ),
    .rob_alloc_dst_wen_2rat     (rob_alloc_dst_wen_2rat     ), 
    .commit_en                  (commit_en                  ),
    .rob_commit_dst_addr_2rat   (rob_commit_dst_addr_2rat   ),
    .rob_commit_br_taken        (rob_commit_br_taken        ),
    .rob_commit_exp_en          (rob_commit_exp_en          ),
    // output to issue_queue
    .rs1_rat_valid              (rs1_rat_valid              ),
    .rs1_Paddr                  (rs1_Paddr                  ),
    .rs2_rat_valid              (rs2_rat_valid              ),
    .rs2_Paddr                  (rs2_Paddr                  ),
    .jp_rs1_rat_valid           (jp_rs1_rat_valid           ),
    .jp_rs1_Paddr               (jp_rs1_Paddr               )
);

gpr u_gpr(
    .clk                    (clk                        ),
    .rst_n                  (rst_n                      ),
    .commit_en              (commit_en                  ),
    .rob_commit_dst_addr    (rob_commit_dst_addr_2rat   ),
    .rob_commit_dst_value   (rob_commit_dst_value_2rat  ),
    .rs1_addr               (rs1_addr                ),
    .rs2_addr               (rs2_addr                ),
    .jp_rs1_addr            (jp_rs1_addr                ),
    // outputs to issue_queue and decoder(jump)
    .rs1_value              (rs1_value_fromGPR          ),
    .rs2_value              (rs2_value_fromGPR          ),
    .jp_rs1_data            (jp_rs1_data_fromGPR        )
);

id_reg u_id_reg(
    .clk                    (clk                    ),
    .rst_n                  (rst_n                  ),
    .cpu_en                 (cpu_en                 ),
    .id_stall               (id_stall               ),
    .id_flush               (id_flush               ),
    .if_pc                  (if_pc                  ),
    .rs1_addr               (rs1_addr               ),
    .rs2_addr               (rs2_addr               ),
    .alu_op                 (alu_op                 ), 
    .mem_op                 (mem_op                 ),
    .br_op                  (br_op                  ),
    .imm                    (imm                    ),
    .alloc_rob              (alloc_rob              ),
    .rs1_rat_valid          (rs1_rat_valid          ),
    .rs1_Paddr              (rs1_Paddr              ),
    .rs2_rat_valid          (rs2_rat_valid          ),
    .rs2_Paddr              (rs2_Paddr              ),
    .rs1_value_fromGPR      (rs1_value_fromGPR      ),
    .rs2_value_fromGPR      (rs2_value_fromGPR      ),
    // outputs
    .id_pc                  (id_pc                  ),
    .id_rs1_addr            (id_rs1_addr            ),
    .id_rs2_addr            (id_rs2_addr            ),
    .id_alu_op              (id_alu_op              ), 
    .id_mem_op              (id_mem_op              ),
    .id_br_op               (id_br_op               ),
    .id_imm                 (id_imm                 ),
    .id_alloc_rob           (id_alloc_rob           ),
    .id_en                  (allocate_en            ),
    .id_rs1_rat_valid       (id_rs1_rat_valid       ),
    .id_rs1_Paddr           (id_rs1_Paddr           ),
    .id_rs2_rat_valid       (id_rs2_rat_valid       ),
    .id_rs2_Paddr           (id_rs2_Paddr           ),
    .id_rs1_value_fromGPR   (id_rs1_value_fromGPR   ),
    .id_rs2_value_fromGPR   (id_rs2_value_fromGPR   )
);

issue_queue_ctrl u_issue_queue_ctrl(
    .clk                                (clk                            ),
    .rst_n                              (rst_n                          ),
    .id_alu_op                          (id_alu_op                      ),
    .id_mem_op                          (id_mem_op                      ),
    .id_br_op                           (id_br_op                       ),
    .id_pc                              (id_pc                          ),
    .id_imm                             (id_imm                         ),
    .id_alloc_rob                       (id_alloc_rob                   ),
    .id_rs1_rat_valid                   (id_rs1_rat_valid               ),
    .id_rs1_Paddr                       (id_rs1_Paddr                   ),
    .id_rs2_rat_valid                   (id_rs2_rat_valid               ),
    .id_rs2_Paddr                       (id_rs2_Paddr                   ),
    .id_rs1_value_fromGPR               (id_rs1_value_fromGPR           ),
    .id_rs2_value_fromGPR               (id_rs2_value_fromGPR           ),
    .rs1_fromROB_valid                  (rs1_fromROB_valid              ),
    .rs2_fromROB_valid                  (rs2_fromROB_valid              ),
    .rs1_value_fromROB                  (rs1_value_fromROB              ),
    .rs2_value_fromROB                  (rs2_value_fromROB              ),
    .wb_alu_dst_Paddr                   (wb_alu_dst_Paddr               ),
    .wb_alu_out                         (wb_alu_out                     ),
    .wb_alu_valid                       (wb_alu_valid                   ),
    .wb_div_dst_Paddr                   (wb_div_dst_Paddr               ),
    .wb_div_out                         (wb_div_out                     ),
    .wb_div_valid                       (wb_div_valid                   ),
    .wb_mul_dst_Paddr                   (wb_mul_dst_Paddr               ),
    .wb_mul_out                         (wb_mul_out                     ),
    .wb_mul_valid                       (wb_mul_valid                   ),
    .wb_load_dst_Paddr                  (wb_mem_dst_Paddr               ),
    .wb_load_data                       (wb_load_data                   ),
    .wb_load_valid                      (wb_load_valid                  ),
    .wb_br_rob                          (wb_br_rob                      ),
    .wb_br_out                          (wb_br_out                      ),
    .wb_br_out_valid                    (wb_br_out_valid                ),
    .rob_commit_br_taken                (rob_commit_br_taken            ),
    .rob_commit_exp_en                  (rob_commit_exp_en              ),
    .mul_ready                          (mul_ready                      ),
    .div_ready                          (div_ready                      ),
    .mem_ready                          (mem_ready                      ),
    // outputs
    // to alu
    .alu_issue_en_out                   (alu_issue_en                   ),
    .alu_issue_queue_op_out             (alu_issue_queue_op             ),
    .alu_issue_queue_pc_out             (alu_issue_queue_pc             ),
    .alu_issue_queue_imm_out            (alu_issue_queue_imm            ),
    .alu_issue_queue_rs1_value_out      (alu_issue_queue_rs1_value      ),
    .alu_issue_queue_rs2_value_out      (alu_issue_queue_rs2_value      ),
    .alu_issue_queue_Pdst_out           (alu_issue_queue_Pdst           ),
    // to div and mul
    .mul_div_issue_en_out               (mul_div_issue_en               ),
    .mul_div_issue_queue_op_out         (mul_div_issue_queue_op         ),
    .mul_div_issue_queue_pc_out         (mul_div_issue_queue_pc         ),
    .mul_div_issue_queue_imm_out        (mul_div_issue_queue_imm        ),
    .mul_div_issue_queue_rs1_value_out  (mul_div_issue_queue_rs1_value  ),
    .mul_div_issue_queue_rs2_value_out  (mul_div_issue_queue_rs2_value  ),
    .mul_div_issue_queue_Pdst_out       (mul_div_issue_queue_Pdst       ),
    // to mem
    .mem_issue_en_out                   (mem_issue_en                   ),
    .mem_issue_queue_op_out             (mem_issue_queue_op             ),
    .mem_issue_queue_pc_out             (mem_issue_queue_pc             ),
    .mem_issue_queue_imm_out            (mem_issue_queue_imm            ),
    .mem_issue_queue_rs1_value_out      (mem_issue_queue_rs1_value      ),
    .mem_issue_queue_rs2_value_out      (mem_issue_queue_rs2_value      ),
    .mem_issue_queue_Pdst_out           (mem_issue_queue_Pdst           ),
    // to br
    .br_issue_en_out                    (br_issue_en                    ),
    .br_issue_queue_op_out              (br_issue_queue_op              ),
    .br_issue_queue_pc_out              (br_issue_queue_pc              ),
    .br_issue_queue_imm_out             (br_issue_queue_imm             ),
    .br_issue_queue_rs1_value_out       (br_issue_queue_rs1_value       ),
    .br_issue_queue_rs2_value_out       (br_issue_queue_rs2_value       ),
    // stall in issue
    .stall_in_issue                     (stall_in_issue                 )
);

fu_top u_fu_top(
    .clk                            (clk                            ),
    .rst_n                          (rst_n                          ),
    // from issue_queue
    .alu_issue_en                   (alu_issue_en                   ),
    .alu_issue_queue_op             (alu_issue_queue_op             ),
    .alu_issue_queue_pc             (alu_issue_queue_pc             ),
    .alu_issue_queue_imm            (alu_issue_queue_imm            ),
    .alu_issue_queue_rs1_value      (alu_issue_queue_rs1_value      ),
    .alu_issue_queue_rs2_value      (alu_issue_queue_rs2_value      ),
    .alu_issue_queue_Pdst           (alu_issue_queue_Pdst           ),
    .mul_div_issue_en               (mul_div_issue_en               ),
    .mul_div_issue_queue_op         (mul_div_issue_queue_op         ),
    .mul_div_issue_queue_rs1_value  (mul_div_issue_queue_rs1_value  ),
    .mul_div_issue_queue_rs2_value  (mul_div_issue_queue_rs2_value  ),
    .mul_div_issue_queue_Pdst       (mul_div_issue_queue_Pdst       ),
    .mem_issue_en                   (mem_issue_en                   ),
    .mem_issue_queue_op             (mem_issue_queue_op             ),
    .mem_issue_queue_imm            (mem_issue_queue_imm            ),
    .mem_issue_queue_rs1_value      (mem_issue_queue_rs1_value      ),
    .mem_issue_queue_rs2_value      (mem_issue_queue_rs2_value      ),
    .mem_issue_queue_Pdst           (mem_issue_queue_Pdst           ),
    .br_issue_en                    (br_issue_en                    ),
    .br_issue_queue_op              (br_issue_queue_op              ),
    .br_issue_queue_pc              (br_issue_queue_pc              ),
    .br_issue_queue_imm             (br_issue_queue_imm             ),
    .br_issue_queue_rs1_value       (br_issue_queue_rs1_value       ),
    .br_issue_queue_rs2_value       (br_issue_queue_rs2_value       ),
    .CPU_HRDATA                     (CPU_HRDATA                     ),
    .CPU_HREADY                     (CPU_HREADY                     ),
    .CPU_HRESP                      (CPU_HRESP                      ),
    .spm_rd_data                    (spm_rd_data                    ),
    // outputs
    .alu_out                        (wb_alu_out                     ),
    .alu_out_valid                  (wb_alu_valid                   ),
    .alu_dst_Paddr                  (wb_alu_dst_Paddr               ),
    .mul_out                        (wb_mul_out                     ),
    .mul_out_valid                  (wb_mul_valid                   ),
    .mul_dst_Paddr                  (wb_mul_dst_Paddr               ),
    .mul_ready                      (mul_ready                      ),
    .div_out                        (wb_div_out                     ),
    .div_out_valid                  (wb_div_valid                   ),
    .div_dst_Paddr                  (wb_div_dst_Paddr               ),
    .div_ready                      (div_ready                      ),
    .load_data                      (wb_load_data                   ),
    .load_data_valid                (wb_load_valid                  ),
    .mem_dst_Paddr                  (wb_mem_dst_Paddr               ),
    .store_finish                   (store_finish                   ),
    .mem_ready                      (mem_ready                      ),
    .br_finish                      (wb_br_finish                   ),
    .br_taken                       (wb_br_taken                    ),
    .br_addr                        (wb_br_addr                     ),
    .br_rob                         (wb_br_rob                      ),
    .br_out                         (wb_br_out                      ),
    .br_out_valid                   (wb_br_out_valid                ),
    .CPU_HADDR                      (CPU_HADDR                      ),
    .CPU_HWRITE                     (CPU_HWRITE                     ),
    .CPU_HSIZE                      (CPU_HSIZE                      ),
    .CPU_HBURST                     (CPU_HBURST                     ),
    .CPU_HTRANS                     (CPU_HTRANS                     ),
    .CPU_HMASTLOCK                  (CPU_HMASTLOCK                  ),
    .CPU_HWDATA                     (CPU_HWDATA                     ),
    .spm_store_byteena              (spm_store_byteena              ),
    .spm_write_data                 (spm_write_data                 ),
    .spm_rdaddress                  (spm_rdaddress                  ),
    .spm_rden                       (spm_rden                       ),
    .spm_wraddress                  (spm_wraddress                  ),
    .spm_wren                       (spm_wren                       ),
    .exp_code_in_mem_ctrl           (exp_code_in_mem_ctrl           )
);

cpu_ctrl u_cpu_ctrl(
    .rob_full           (rob_full           ),
    .stall_in_decoder   (stall_in_decoder   ),
    .stall_in_issue     (stall_in_issue     ),
    .jp_taken           (jp_taken           ),
    .rob_commit_br_taken(rob_commit_br_taken),
    // outputs
    .pc_stall           (pc_stall           ),
    .if_stall           (if_stall           ),
    .id_stall           (id_stall           ),
    .if_flush           (if_flush           ),
    .id_flush           (id_flush           )
);

endmodule
`endif
