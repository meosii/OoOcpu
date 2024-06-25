`include "fu_top.v"
`include "define.v"
module tb_fu_top();

    reg                                 clk;
    reg                                 rst_n;
    // inputs from issue queue
    // to alu
    reg                                 alu_issue_en;
    reg [`DATA_WIDTH_ALU_OP-1 : 0]      alu_issue_queue_op;
    reg [`PC_WIDTH-1 : 0]               alu_issue_queue_pc;
    reg [`WORD_WIDTH-1 : 0]             alu_issue_queue_imm;
    reg [`WORD_WIDTH-1 : 0]             alu_issue_queue_rs1_value;
    reg [`WORD_WIDTH-1 : 0]             alu_issue_queue_rs2_value;
    reg [$clog2(`ROB_DEPTH)-1 : 0]      alu_issue_queue_Pdst;
    // to mul or div
    reg                                 mul_div_issue_en;
    reg [`DATA_WIDTH_ALU_OP-1 : 0]      mul_div_issue_queue_op;
    reg [`WORD_WIDTH-1 : 0]             mul_div_issue_queue_rs1_value;
    reg [`WORD_WIDTH-1 : 0]             mul_div_issue_queue_rs2_value;
    reg [$clog2(`ROB_DEPTH)-1 : 0]      mul_div_issue_queue_Pdst;
    // to mem
    reg                                 mem_issue_en;
    reg [`DATA_WIDTH_MEM_OP-1 : 0]      mem_issue_queue_op;
    reg [`WORD_WIDTH-1 : 0]             mem_issue_queue_imm;
    reg [`WORD_WIDTH-1 : 0]             mem_issue_queue_rs1_value;
    reg [`WORD_WIDTH-1 : 0]             mem_issue_queue_rs2_value;
    reg [$clog2(`ROB_DEPTH)-1 : 0]      mem_issue_queue_Pdst;
    // to br
    reg                                 br_issue_en;
    reg [`DATA_WIDTH_BR_OP-1 : 0]       br_issue_queue_op;
    reg [`PC_WIDTH-1 : 0]               br_issue_queue_pc;
    reg [`WORD_WIDTH-1 : 0]             br_issue_queue_imm;
    reg [`WORD_WIDTH-1 : 0]             br_issue_queue_rs1_value;
    reg [`WORD_WIDTH-1 : 0]             br_issue_queue_rs2_value;
    // input from AHB
    reg [`WORD_WIDTH - 1 : 0]           CPU_HRDATA;
    reg                                 CPU_HREADY;
    reg [1 : 0]                         CPU_HRESP;
    // from spm
    reg [`WORD_WIDTH - 1 : 0]           spm_rd_data;

    // outputs
    // alu writeback
    wire [`WORD_WIDTH-1 : 0]            alu_out;
    wire                                alu_out_valid;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     alu_dst_Paddr;
    // mul writeback
    wire [`WORD_WIDTH-1 : 0]            mul_out;
    wire                                mul_out_valid;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     mul_dst_Paddr;
    wire                                mul_ready;
    // div writeback
    wire [`WORD_WIDTH-1 : 0]            div_out;
    wire                                div_out_valid;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     div_dst_Paddr;
    wire                                div_ready;
    // mem writeback
    wire [`WORD_WIDTH - 1 : 0]          load_data;
    wire                                load_data_valid;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     mem_dst_Paddr;
    wire                                mem_ready;
    // br writeback
    wire                                wb_br_finish;  // the branch instruction has judged whether jump and jump to where
    wire                                wb_br_taken;
    wire [`PC_WIDTH-1 : 0]              wb_br_addr;
    wire  [`ROB_DEPTH-1 : 0]            wb_br_rob;     // which instruction
    wire [`WORD_WIDTH-1 : 0]            wb_br_out; // jal and jalr: rd = pc+4
    wire                                wb_br_out_valid;
    // to AHB
    wire [`WORD_WIDTH - 1 : 0]          CPU_HADDR;
    wire                                CPU_HWRITE;
    wire [2 : 0]                        CPU_HSIZE;
    wire [2 : 0]                        CPU_HBURST;
    wire [1 : 0]                        CPU_HTRANS;
    wire                                CPU_HMASTLOCK;
    wire [`WORD_WIDTH - 1 : 0]          CPU_HWDATA;
    // to spm
    wire [3 : 0]                        spm_store_byteena;
    wire [`WORD_WIDTH - 1 : 0]          spm_write_data;
    wire [`WORD_WIDTH - 1 : 0]          spm_rdaddress;
    wire                                spm_rden;
    wire [`WORD_WIDTH - 1 : 0]          spm_wraddress;
    wire                                spm_wren;
    // exp_code
    wire [`DATA_WIDTH_ISA_EXP - 1 : 0]  exp_code_in_mem_ctrl;

fu_top u_fu_top(

    .clk                            (clk                            ),
    .rst_n                          (rst_n                          ),
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
    .alu_out                        (alu_out                        ),
    .alu_out_valid                  (alu_out_valid                  ),
    .alu_dst_Paddr                  (alu_dst_Paddr                  ),
    .mul_out                        (mul_out                        ),
    .mul_out_valid                  (mul_out_valid                  ),
    .mul_dst_Paddr                  (mul_dst_Paddr                  ),
    .mul_ready                      (mul_ready                      ),
    .div_out                        (div_out                        ),
    .div_out_valid                  (div_out_valid                  ),
    .div_dst_Paddr                  (div_dst_Paddr                  ),
    .div_ready                      (div_ready                      ),
    .load_data                      (load_data                      ),
    .load_data_valid                (load_data_valid                ),
    .mem_dst_Paddr                  (mem_dst_Paddr                  ),
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

always #5 clk = ~clk;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        alu_issue_en                  <= 0; 
        alu_issue_queue_op            <= 0; 
        alu_issue_queue_pc            <= 0; 
        alu_issue_queue_imm           <= 0; 
        alu_issue_queue_rs1_value     <= 0; 
        alu_issue_queue_rs2_value     <= 0; 
        alu_issue_queue_Pdst          <= 0; 
    end else begin
        alu_issue_en                  <= $random(); 
        alu_issue_queue_op            <= $random(); 
        alu_issue_queue_pc            <= $random(); 
        alu_issue_queue_imm           <= $random(); 
        alu_issue_queue_rs1_value     <= $random(); 
        alu_issue_queue_rs2_value     <= $random(); 
        alu_issue_queue_Pdst          <= $random(); 
    end
end
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        mul_div_issue_en                  <= 0; 
        mul_div_issue_queue_op            <= 0; 
        mul_div_issue_queue_rs1_value     <= 0; 
        mul_div_issue_queue_rs2_value     <= 0; 
        mul_div_issue_queue_Pdst          <= 0; 
    end else if (mul_ready && div_ready) begin
        mul_div_issue_en                  <= $random(); 
        mul_div_issue_queue_op            <= $random(); 
        mul_div_issue_queue_rs1_value     <= $random(); 
        mul_div_issue_queue_rs2_value     <= $random(); 
        mul_div_issue_queue_Pdst          <= $random(); 
    end else begin
        mul_div_issue_en                  <= 0; 
        mul_div_issue_queue_op            <= 0; 
        mul_div_issue_queue_rs1_value     <= 0; 
        mul_div_issue_queue_rs2_value     <= 0; 
        mul_div_issue_queue_Pdst          <= 0; 
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        mem_issue_en                  <= 0; 
        mem_issue_queue_op            <= 0; 
        mem_issue_queue_imm           <= 0; 
        mem_issue_queue_rs1_value     <= 0; 
        mem_issue_queue_rs2_value     <= 0; 
        mem_issue_queue_Pdst          <= 0; 
    end else begin
        mem_issue_en                  <= $random(); 
        mem_issue_queue_op            <= $random(); 
        mem_issue_queue_imm           <= $random(); 
        mem_issue_queue_rs1_value     <= $random(); 
        mem_issue_queue_rs2_value     <= $random(); 
        mem_issue_queue_Pdst          <= $random(); 
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        br_issue_en                  <= 0; 
        br_issue_queue_op            <= 0; 
        br_issue_queue_pc            <= 0; 
        br_issue_queue_imm           <= 0; 
        br_issue_queue_rs1_value     <= 0; 
        br_issue_queue_rs2_value     <= 0; 
    end else begin
        br_issue_en                  <= $random(); 
        br_issue_queue_op            <= $random(); 
        br_issue_queue_pc            <= $random(); 
        br_issue_queue_imm           <= $random(); 
        br_issue_queue_rs1_value     <= $random(); 
        br_issue_queue_rs2_value     <= $random(); 
    end
end

reg [31:0] div_result;
reg [31:0] div_remainder;
wire div_unsign_en;
wire div_sign_en;

assign div_unsign_en = (mul_div_issue_queue_op == `ALU_OP_DIV) || (mul_div_issue_queue_op == `ALU_OP_REM); 
assign div_sign_en = (mul_div_issue_queue_op == `ALU_OP_DIVU) || (mul_div_issue_queue_op == `ALU_OP_REMU);

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        div_result <= 'b0;
        div_remainder <= 'b0;
    end else if (div_ready && mul_div_issue_en && div_unsign_en) begin
        div_result <= $unsigned(mul_div_issue_queue_rs1_value)/$unsigned(mul_div_issue_queue_rs2_value);
        div_remainder <= $unsigned(mul_div_issue_queue_rs1_value)%$unsigned(mul_div_issue_queue_rs2_value);
    end else if (div_ready && mul_div_issue_en && div_sign_en) begin
        div_result <= $signed(mul_div_issue_queue_rs1_value)/$signed(mul_div_issue_queue_rs2_value);
        div_remainder <= $signed(mul_div_issue_queue_rs1_value)%$signed(mul_div_issue_queue_rs2_value);
    end
end

initial begin
    #0 begin
        clk = 0;
        rst_n = 0;
    end
    #2 begin
        clk = 0;
        rst_n = 1;
    end
    #2000 $finish;
end

initial begin
    $fsdbDumpfile("wave_fu_top.fsdb");
    $fsdbDumpvars();
    $fsdbDumpMDA();
end


endmodule
