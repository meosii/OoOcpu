`ifndef OOOCPU_FU_TOP
`define OOOCPU_FU_TOP
`include "alu.v"
`include "fu_mul.v"
`include "fu_div.v"
`include "mem_ctrl.v"
`include "fu_br.v"
`include "define.v"
module fu_top (
    input wire                                  clk,
    input wire                                  rst_n,
    // inputs from issue queue
    // to alu
    input wire                                  alu_issue_en,
    input wire [`DATA_WIDTH_ALU_OP-1 : 0]       alu_issue_queue_op,
    input wire [`PC_WIDTH-1 : 0]                alu_issue_queue_pc,
    input wire [`WORD_WIDTH-1 : 0]              alu_issue_queue_imm,
    input wire [`WORD_WIDTH-1 : 0]              alu_issue_queue_rs1_value,
    input wire [`WORD_WIDTH-1 : 0]              alu_issue_queue_rs2_value,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       alu_issue_queue_Pdst,
    // to mul or div
    input wire                                  mul_div_issue_en,
    input wire [`DATA_WIDTH_ALU_OP-1 : 0]       mul_div_issue_queue_op,
    input wire [`WORD_WIDTH-1 : 0]              mul_div_issue_queue_rs1_value,
    input wire [`WORD_WIDTH-1 : 0]              mul_div_issue_queue_rs2_value,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       mul_div_issue_queue_Pdst,
    // to mem
    input wire                                  mem_issue_en,
    input wire [`DATA_WIDTH_MEM_OP-1 : 0]       mem_issue_queue_op,
    input wire [`WORD_WIDTH-1 : 0]              mem_issue_queue_imm,
    input wire [`WORD_WIDTH-1 : 0]              mem_issue_queue_rs1_value,
    input wire [`WORD_WIDTH-1 : 0]              mem_issue_queue_rs2_value,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       mem_issue_queue_Pdst,
    // to br
    input wire                                  br_issue_en,
    input wire [`DATA_WIDTH_BR_OP-1 : 0]        br_issue_queue_op,
    input wire [`PC_WIDTH-1 : 0]                br_issue_queue_pc,
    input wire [`WORD_WIDTH-1 : 0]              br_issue_queue_imm,
    input wire [`WORD_WIDTH-1 : 0]              br_issue_queue_rs1_value,
    input wire [`WORD_WIDTH-1 : 0]              br_issue_queue_rs2_value,
    // input from AHB
    input wire [`WORD_WIDTH - 1 : 0]            CPU_HRDATA,
    input wire                                  CPU_HREADY,
    input wire [1 : 0]                          CPU_HRESP,
    // from spm
    input wire [`WORD_WIDTH - 1 : 0]            spm_rd_data,

    // outputs
    // alu writeback
    output wire [`WORD_WIDTH-1 : 0]             alu_out,
    output wire                                 alu_out_valid,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      alu_dst_Paddr,
    // mul writeback
    output reg [`WORD_WIDTH-1 : 0]              mul_out,
    output wire                                 mul_out_valid,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      mul_dst_Paddr,
    output wire                                 mul_ready,
    // div writeback
    output reg [`WORD_WIDTH-1 : 0]              div_out,
    output wire                                 div_out_valid,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      div_dst_Paddr,
    output wire                                 div_ready,
    // mem writeback
    output wire [`WORD_WIDTH - 1 : 0]           load_data,
    output wire                                 load_data_valid,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      mem_dst_Paddr,
    output wire                                 store_finish,
    output wire                                 mem_ready,
    // br writeback
    output wire                                 br_finish,
    output wire                                 br_taken,
    output wire [`PC_WIDTH-1 : 0]               br_addr,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      br_rob,
    output wire [`WORD_WIDTH-1 : 0]             br_out, // jal and jalr: rd = pc+4
    output wire                                 br_out_valid,
    // to AHB
    output wire [`WORD_WIDTH - 1 : 0]           CPU_HADDR,
    output wire                                 CPU_HWRITE,
    output wire [2 : 0]                         CPU_HSIZE,
    output wire [2 : 0]                         CPU_HBURST,
    output wire [1 : 0]                         CPU_HTRANS,
    output wire                                 CPU_HMASTLOCK,
    output wire [`WORD_WIDTH - 1 : 0]           CPU_HWDATA,
    // to spm
    output wire [3 : 0]                         spm_store_byteena,
    output wire [`WORD_WIDTH - 1 : 0]           spm_write_data,
    output wire [`WORD_WIDTH - 1 : 0]           spm_rdaddress,
    output wire                                 spm_rden,
    output wire [`WORD_WIDTH - 1 : 0]           spm_wraddress,
    output wire                                 spm_wren,
    // exp_code
    output wire [`DATA_WIDTH_ISA_EXP - 1 : 0]   exp_code_in_mem_ctrl

);

parameter SIGNED_WORD_WIDTH = `WORD_WIDTH + 1;
parameter PARTIAL_PRODUCT_WIDTH = SIGNED_WORD_WIDTH + SIGNED_WORD_WIDTH;

// -------------------------------------------------- alu -----------------------------------------------------
assign alu_dst_Paddr = alu_issue_queue_Pdst;

alu u_alu(
    .alu_issue_en               (alu_issue_en               ),
    .alu_issue_queue_op         (alu_issue_queue_op         ),
    .alu_issue_queue_pc         (alu_issue_queue_pc         ),
    .alu_issue_queue_imm        (alu_issue_queue_imm        ),
    .alu_issue_queue_rs1_value  (alu_issue_queue_rs1_value  ),
    .alu_issue_queue_rs2_value  (alu_issue_queue_rs2_value  ),
    // outputs
    .alu_out                    (alu_out                    ),
    .alu_out_valid              (alu_out_valid              )
);


// -----------------------------------------------mul and div---------------------------------------------------

fu_mul  #(
    .SIGNED_WORD_WIDTH              (SIGNED_WORD_WIDTH              ),
    .PARTIAL_PRODUCT_WIDTH          (PARTIAL_PRODUCT_WIDTH          )
)u_fu_mul(
    .clk                            (clk                            ),
    .rst_n                          (rst_n                          ),
    .mul_div_issue_en               (mul_div_issue_en               ),
    .mul_div_issue_queue_op         (mul_div_issue_queue_op         ),
    .mul_div_issue_queue_rs1_value  (mul_div_issue_queue_rs1_value  ),
    .mul_div_issue_queue_rs2_value  (mul_div_issue_queue_rs2_value  ),
    .mul_div_issue_queue_Pdst       (mul_div_issue_queue_Pdst       ),
    .mul_out                        (mul_out                        ),
    .mul_out_valid                  (mul_out_valid                  ),
    .mul_dst_Paddr                  (mul_dst_Paddr                  ),
    .mul_ready                      (mul_ready                      )
);

fu_div u_fu_div(
    .clk                            (clk                            ),
    .rst_n                          (rst_n                          ),
    .mul_div_issue_en               (mul_div_issue_en               ),
    .mul_div_issue_queue_op         (mul_div_issue_queue_op         ),
    .mul_div_issue_queue_rs1_value  (mul_div_issue_queue_rs1_value  ),
    .mul_div_issue_queue_rs2_value  (mul_div_issue_queue_rs2_value  ),
    .mul_div_issue_queue_Pdst       (mul_div_issue_queue_Pdst       ),
    .div_out                        (div_out                        ),
    .div_out_valid                  (div_out_valid                  ),
    .div_dst_Paddr                  (div_dst_Paddr                  ),
    .div_ready                      (div_ready                      )
);

//--------------------------------------------------- mem ctrl -----------------------------------------------
mem_ctrl u_mem_ctrl(
    .clk                        (clk                        ),
    .rst_n                      (rst_n                      ),
    .mem_issue_en               (mem_issue_en               ),
    .mem_issue_queue_op         (mem_issue_queue_op         ),
    .mem_issue_queue_imm        (mem_issue_queue_imm        ),
    .mem_issue_queue_rs1_value  (mem_issue_queue_rs1_value  ),
    .mem_issue_queue_rs2_value  (mem_issue_queue_rs2_value  ),
    .mem_issue_queue_Pdst       (mem_issue_queue_Pdst       ),
    .CPU_HRDATA                 (CPU_HRDATA                 ),
    .CPU_HREADY                 (CPU_HREADY                 ),
    .CPU_HRESP                  (CPU_HRESP                  ),
    .spm_rd_data                (spm_rd_data                ),
    // outputs
    // to AHB
    .CPU_HADDR                  (CPU_HADDR                  ),
    .CPU_HWRITE                 (CPU_HWRITE                 ),
    .CPU_HSIZE                  (CPU_HSIZE                  ),
    .CPU_HBURST                 (CPU_HBURST                 ),
    .CPU_HTRANS                 (CPU_HTRANS                 ),
    .CPU_HMASTLOCK              (CPU_HMASTLOCK              ),
    .CPU_HWDATA                 (CPU_HWDATA                 ),
    // to spm
    .spm_store_byteena          (spm_store_byteena          ),
    .spm_write_data             (spm_write_data             ),
    .spm_rdaddress              (spm_rdaddress              ),
    .spm_rden                   (spm_rden                   ),
    .spm_wraddress              (spm_wraddress              ),
    .spm_wren                   (spm_wren                   ),
    // writeback
    .load_data                  (load_data                  ),
    .load_data_valid            (load_data_valid            ),
    .mem_ready                  (mem_ready                  ),
    .mem_dst_Paddr              (mem_dst_Paddr              ),
    .store_finish               (store_finish               ),
    // exception
    .exp_code_in_mem_ctrl       (exp_code_in_mem_ctrl       )
);
// ----------------------- branch ---------------------------------------------

fu_br u_fu_br(
    .br_issue_en                (br_issue_en                ),
    .br_issue_queue_op          (br_issue_queue_op          ),
    .br_issue_queue_pc          (br_issue_queue_pc          ),
    .br_issue_queue_imm         (br_issue_queue_imm         ),
    .br_issue_queue_rs1_value   (br_issue_queue_rs1_value   ),
    .br_issue_queue_rs2_value   (br_issue_queue_rs2_value   ),
    // outputs
    .br_finish                  (br_finish                  ),
    .br_taken                   (br_taken                   ),
    .br_addr                    (br_addr                    ),
    .br_rob                     (br_rob                     ),
    .br_out                     (br_out                     ),
    .br_out_valid               (br_out_valid               )
);

endmodule
`endif 
