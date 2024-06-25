`include "define.v"
`include "issue_queue_ctrl.v"
module tb_issue_queue_ctrl();
    reg                                  clk;
    reg                                  rst_n;
    
    // from issue queue ctrl
    reg [`DATA_WIDTH_ALU_OP - 1 : 0]    id_alu_op;      // include div and mul
    reg [`DATA_WIDTH_MEM_OP-1 :0]       id_mem_op;
    reg [`DATA_WIDTH_BR_OP-1 :0]        id_br_op;
    reg [`PC_WIDTH-1 : 0]               id_pc;
    reg [`WORD_WIDTH-1 : 0]             id_imm;
    reg [$clog2(`ROB_DEPTH)-1 : 0]      id_alloc_rob; // from rob -> if_reg

    reg                                 rs1_rat_valid;  // when rat_valid==0; rs1_value_fromGPR is valid.
    reg [$clog2(`ROB_DEPTH)-1 : 0]      rs1_Paddr;
    reg                                 rs2_rat_valid;
    reg [$clog2(`ROB_DEPTH)-1 : 0]      rs2_Paddr;
    reg [`WORD_WIDTH-1 : 0]             rs1_value_fromGPR;
    reg [`WORD_WIDTH-1 : 0]             rs2_value_fromGPR;
    
    // from writeback; when rat_valid==1
        // from alu
    reg  [$clog2(`ROB_DEPTH)-1 : 0]     wb_alu_dst_Paddr;
    reg  [`WORD_WIDTH-1 : 0]            wb_alu_out;
    reg                                 wb_alu_valid;
        // from div
    wire [$clog2(`ROB_DEPTH)-1 : 0]     wb_div_dst_Paddr;
    wire [`WORD_WIDTH-1 : 0]            wb_div_out;
    wire                                wb_div_valid;
        // from mul
    wire [$clog2(`ROB_DEPTH)-1 : 0]     wb_mul_dst_Paddr;
    wire [`WORD_WIDTH-1 : 0]            wb_mul_out;
    wire                                wb_mul_valid;
        // from mem
    reg  [$clog2(`ROB_DEPTH)-1 : 0]     wb_load_dst_Paddr;
    reg  [`WORD_WIDTH-1 : 0]            wb_load_data;
    reg                                 wb_load_valid;

    // branch and exception
    reg                                 rob_commit_branch_taken;    // branch -> refresh the issue_queue
    reg                                 rob_commit_exp_en;          // exp -> refresh the issue_queue

    wire                                mul_ready;
    wire                                div_ready;
    reg                                 mem_ready;

    // outputs
    // to alu
    wire                                alu_issue_en_out;
    wire [`DATA_WIDTH_ALU_OP-1 : 0]     alu_issue_queue_op_out;
    wire [`PC_WIDTH-1 : 0]              alu_issue_queue_pc_out;
    wire [`WORD_WIDTH-1 : 0]            alu_issue_queue_imm_out;
    wire [`WORD_WIDTH-1 : 0]            alu_issue_queue_rs1_value_out;
    wire [`WORD_WIDTH-1 : 0]            alu_issue_queue_rs2_value_out;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     alu_issue_queue_Pdst_out;
    // to mul or div
    wire                                mul_div_issue_en_out;
    wire [`DATA_WIDTH_ALU_OP-1 : 0]     mul_div_issue_queue_op_out;
    wire [`PC_WIDTH-1 : 0]              mul_div_issue_queue_pc_out;
    wire [`WORD_WIDTH-1 : 0]            mul_div_issue_queue_imm_out;
    wire [`WORD_WIDTH-1 : 0]            mul_div_issue_queue_rs1_value_out;
    wire [`WORD_WIDTH-1 : 0]            mul_div_issue_queue_rs2_value_out;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     mul_div_issue_queue_Pdst_out;
    // to mem
    wire                                mem_issue_en_out;
    wire [`DATA_WIDTH_MEM_OP-1 : 0]     mem_issue_queue_op_out;
    wire [`PC_WIDTH-1 : 0]              mem_issue_queue_pc_out;
    wire [`WORD_WIDTH-1 : 0]            mem_issue_queue_imm_out;
    wire [`WORD_WIDTH-1 : 0]            mem_issue_queue_rs1_value_out;
    wire [`WORD_WIDTH-1 : 0]            mem_issue_queue_rs2_value_out;
    wire [$clog2(`ROB_DEPTH)-1 : 0]     mem_issue_queue_Pdst_out;
    // to br
    wire                                br_issue_en_out;
    wire [`DATA_WIDTH_BR_OP-1 : 0]      br_issue_queue_op_out;
    wire [`PC_WIDTH-1 : 0]              br_issue_queue_pc_out;
    wire [`WORD_WIDTH-1 : 0]            br_issue_queue_imm_out;
    wire [`WORD_WIDTH-1 : 0]            br_issue_queue_rs1_value_out;
    wire [`WORD_WIDTH-1 : 0]            br_issue_queue_rs2_value_out;
    // to judge dispatch_en?
    wire                                alu_issue_queue_full;
    wire                                mul_div_issue_queue_full;
    wire                                mem_issue_queue_full;
    wire                                br_issue_queue_full;


issue_queue_ctrl u_issue_queue_ctrl(
    .clk                                (clk                                ),
    .rst_n                              (rst_n                              ),
    .id_alu_op                          (id_alu_op                          ),
    .id_mem_op                          (id_mem_op                          ),
    .id_br_op                           (id_br_op                           ),
    .id_pc                              (id_pc                              ),
    .id_imm                             (id_imm                             ),
    .id_alloc_rob                       (id_alloc_rob                       ),
    .rs1_rat_valid                      (rs1_rat_valid                      ),
    .rs1_Paddr                          (rs1_Paddr                          ),
    .rs2_rat_valid                      (rs2_rat_valid                      ),
    .rs2_Paddr                          (rs2_Paddr                          ),
    .rs1_value_fromGPR                  (rs1_value_fromGPR                  ),
    .rs2_value_fromGPR                  (rs2_value_fromGPR                  ),
    .wb_alu_dst_Paddr                   (wb_alu_dst_Paddr                   ),
    .wb_alu_out                         (wb_alu_out                         ),
    .wb_alu_valid                       (wb_alu_valid                       ),
    .wb_div_dst_Paddr                   (wb_div_dst_Paddr                   ),
    .wb_div_out                         (wb_div_out                         ),
    .wb_div_valid                       (wb_div_valid                       ),
    .wb_mul_dst_Paddr                   (wb_mul_dst_Paddr                   ),
    .wb_mul_out                         (wb_mul_out                         ),
    .wb_mul_valid                       (wb_mul_valid                       ),
    .wb_load_dst_Paddr                  (wb_load_dst_Paddr                  ),
    .wb_load_data                       (wb_load_data                       ),
    .wb_load_valid                      (wb_load_valid                      ),
    .rob_commit_branch_taken            (rob_commit_branch_taken            ),
    .rob_commit_exp_en                  (rob_commit_exp_en                  ),
    .mul_ready                          (mul_ready                          ),
    .div_ready                          (div_ready                          ),
    .mem_ready                          (mem_ready                          ),
    // outputs
    // to alu
    .alu_issue_en_out                   (alu_issue_en_out                   ),
    .alu_issue_queue_op_out             (alu_issue_queue_op_out             ),
    .alu_issue_queue_pc_out             (alu_issue_queue_pc_out             ),
    .alu_issue_queue_imm_out            (alu_issue_queue_imm_out            ),
    .alu_issue_queue_rs1_value_out      (alu_issue_queue_rs1_value_out      ),
    .alu_issue_queue_rs2_value_out      (alu_issue_queue_rs2_value_out      ),
    .alu_issue_queue_Pdst_out           (alu_issue_queue_Pdst_out           ),
    .alu_issue_queue_full               (alu_issue_queue_full               ),
    // to div and mul
    .mul_div_issue_en_out               (mul_div_issue_en_out               ),
    .mul_div_issue_queue_op_out         (mul_div_issue_queue_op_out         ),
    .mul_div_issue_queue_pc_out         (mul_div_issue_queue_pc_out         ),
    .mul_div_issue_queue_imm_out        (mul_div_issue_queue_imm_out        ),
    .mul_div_issue_queue_rs1_value_out  (mul_div_issue_queue_rs1_value_out  ),
    .mul_div_issue_queue_rs2_value_out  (mul_div_issue_queue_rs2_value_out  ),
    .mul_div_issue_queue_Pdst_out       (mul_div_issue_queue_Pdst_out       ),
    .mul_div_issue_queue_full           (mul_div_issue_queue_full           ),
    // to mem
    .mem_issue_en_out                   (mem_issue_en_out                   ),
    .mem_issue_queue_op_out             (mem_issue_queue_op_out             ),
    .mem_issue_queue_pc_out             (mem_issue_queue_pc_out             ),
    .mem_issue_queue_imm_out            (mem_issue_queue_imm_out            ),
    .mem_issue_queue_rs1_value_out      (mem_issue_queue_rs1_value_out      ),
    .mem_issue_queue_rs2_value_out      (mem_issue_queue_rs2_value_out      ),
    .mem_issue_queue_Pdst_out           (mem_issue_queue_Pdst_out           ),
    .mem_issue_queue_full               (mem_issue_queue_full               ),
    
        // to br
    .br_issue_en_out                    (br_issue_en_out                    ),
    .br_issue_queue_op_out              (br_issue_queue_op_out              ),
    .br_issue_queue_pc_out              (br_issue_queue_pc_out              ),
    .br_issue_queue_imm_out             (br_issue_queue_imm_out             ),
    .br_issue_queue_rs1_value_out       (br_issue_queue_rs1_value_out       ),
    .br_issue_queue_rs2_value_out       (br_issue_queue_rs2_value_out       ),
    .br_issue_queue_full                (br_issue_queue_full                )
);

always #5 clk = ~clk;

reg [31:0]  alu_out;
wire [63:0] md_out;
reg [31:0]  mul_div_out;
reg         mul_en;
reg         div_en;
wire [31:0] div_quotient;
wire [31:0] div_remainder;
//  div
reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_div_dst_Paddr_tmp;
reg  [`WORD_WIDTH-1 : 0]             wb_div_out_tmp;
reg                                  wb_div_valid_tmp;
reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_div_dst_Paddr_tmp_r1;
reg  [`WORD_WIDTH-1 : 0]             wb_div_out_tmp_r1;
reg                                  wb_div_valid_tmp_r1;
reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_div_dst_Paddr_tmp_r2;
reg  [`WORD_WIDTH-1 : 0]             wb_div_out_tmp_r2;
reg                                  wb_div_valid_tmp_r2;
reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_div_dst_Paddr_tmp_r3;
reg  [`WORD_WIDTH-1 : 0]             wb_div_out_tmp_r3;
reg                                  wb_div_valid_tmp_r3;
//  mul
reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_mul_dst_Paddr_tmp;
reg  [`WORD_WIDTH-1 : 0]             wb_mul_out_tmp;
reg                                  wb_mul_valid_tmp;
reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_mul_dst_Paddr_tmp_r1;
reg  [`WORD_WIDTH-1 : 0]             wb_mul_out_tmp_r1;
reg                                  wb_mul_valid_tmp_r1;
reg  [`GPR_ADDR_WIDTH-1 : 0]         wb_mul_dst_Paddr_tmp_r2;
reg  [`WORD_WIDTH-1 : 0]             wb_mul_out_tmp_r2;
reg                                  wb_mul_valid_tmp_r2;

always @(*) begin
   case (alu_issue_queue_op_out)
    `ALU_OP_ADDI: begin
        alu_out = alu_issue_queue_imm_out + alu_issue_queue_rs1_value_out;
    end
    `ALU_OP_SLTI: begin
        alu_out = ($signed(alu_issue_queue_rs1_value_out) < $signed(alu_issue_queue_imm_out))? 1'b1:1'b0;
    end
    `ALU_OP_SLTIU: begin
        alu_out = (alu_issue_queue_rs1_value_out < alu_issue_queue_imm_out)? 1'b1:1'b0;
    end
    `ALU_OP_ANDI: begin
        alu_out = alu_issue_queue_imm_out & alu_issue_queue_rs1_value_out;
    end
    `ALU_OP_ORI: begin
        alu_out = alu_issue_queue_imm_out | alu_issue_queue_rs1_value_out;
    end
    `ALU_OP_XORI: begin
        alu_out = alu_issue_queue_imm_out ^ alu_issue_queue_rs1_value_out;
    end
    `ALU_OP_SLLI: begin
        alu_out = alu_issue_queue_rs1_value_out << alu_issue_queue_imm_out[4:0];
    end
    `ALU_OP_SRLI: begin
        alu_out = alu_issue_queue_rs1_value_out >> alu_issue_queue_imm_out[4:0];
    end
    `ALU_OP_SRAI: begin
        alu_out = alu_issue_queue_rs1_value_out >>> alu_issue_queue_imm_out[4:0];
    end
    `ALU_OP_LUI: begin  // imm<<12
        alu_out = alu_issue_queue_imm_out; // imm has already shift, << 12
    end
    `ALU_OP_AUIPC: begin
        alu_out = alu_issue_queue_pc_out + (alu_issue_queue_imm_out << 12);
    end
    `ALU_OP_ADD: begin
        alu_out = alu_issue_queue_rs1_value_out + alu_issue_queue_rs2_value_out;
    end
    `ALU_OP_SLT: begin
        alu_out = ($signed(alu_issue_queue_rs1_value_out) < $signed(alu_issue_queue_rs2_value_out))? 1:0;
    end
    `ALU_OP_SLTU: begin
        alu_out = (alu_issue_queue_rs1_value_out < alu_issue_queue_rs2_value_out)? 1:0;
    end
    `ALU_OP_AND: begin
        alu_out = alu_issue_queue_rs1_value_out & alu_issue_queue_rs2_value_out;
    end
    `ALU_OP_OR: begin
        alu_out = alu_issue_queue_rs1_value_out | alu_issue_queue_rs2_value_out;
    end
    `ALU_OP_XOR: begin
        alu_out = alu_issue_queue_rs1_value_out ^ alu_issue_queue_rs2_value_out;
    end
    `ALU_OP_SLL: begin
        alu_out = alu_issue_queue_rs1_value_out << alu_issue_queue_rs2_value_out[4:0];
    end
    `ALU_OP_SRL: begin
        alu_out = alu_issue_queue_rs1_value_out >> alu_issue_queue_rs2_value_out[4:0];
    end
    `ALU_OP_SUB: begin
        alu_out = alu_issue_queue_rs1_value_out - alu_issue_queue_rs2_value_out;
    end
    `ALU_OP_SRA: begin
        alu_out = alu_issue_queue_rs1_value_out >>> alu_issue_queue_rs2_value_out[4:0];
    end
    default: alu_out = `WORD_WIDTH'b0;
   endcase 
end

assign md_out = mul_div_issue_queue_rs1_value_out*mul_div_issue_queue_rs2_value_out; 
assign div_quotient = mul_div_issue_queue_rs2_value_out/mul_div_issue_queue_rs1_value_out;
assign div_remainder = mul_div_issue_queue_rs2_value_out%mul_div_issue_queue_rs1_value_out;

always @(*) begin
   case (mul_div_issue_queue_op_out)
    `ALU_OP_MUL:begin
        mul_div_out = md_out;
        mul_en      = mul_div_issue_en_out;
        div_en      = 1'b0;
    end
    `ALU_OP_MULH, `ALU_OP_MULHSU, `ALU_OP_MULHU:begin
        mul_div_out = md_out[`WORD_WIDTH+`WORD_WIDTH-1 : `WORD_WIDTH];
        mul_en      = mul_div_issue_en_out;
        div_en      = 1'b0;
    end
    `ALU_OP_DIV, `ALU_OP_DIVU: begin
        mul_div_out = div_quotient;
        mul_en      = 1'b0;
        div_en      = mul_div_issue_en_out;
    end
    `ALU_OP_REM, `ALU_OP_REMU: begin
        mul_div_out = div_remainder;
        mul_en      = 1'b0;
        div_en      = mul_div_issue_en_out;
    end
    default: begin
        mul_div_out = `WORD_WIDTH'b0;
        mul_en      = 1'b0;
        div_en      = 1'b0;
    end
   endcase 
end
// -------------------alu---------------------------------------------------
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        wb_alu_dst_Paddr <= 'b0;
        wb_alu_out   <= 'b0;
        wb_alu_valid <= 'b0;
    end else if (alu_issue_en_out) begin
        wb_alu_dst_Paddr <= alu_issue_queue_Pdst_out;
        wb_alu_out   <= alu_out;
        wb_alu_valid <= 'b1;
    end else begin
        wb_alu_dst_Paddr <= 'b0;
        wb_alu_out   <= 'b0;
        wb_alu_valid <= 'b0;
    end
end
// ----------------------------------mul-----------------------------------
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        wb_mul_dst_Paddr_tmp <= 'b0;
        wb_mul_out_tmp   <= 'b0;
        wb_mul_valid_tmp <= 'b0;
    end else if (mul_div_issue_en_out && mul_en) begin
        wb_mul_dst_Paddr_tmp <= mul_div_issue_queue_Pdst_out;
        wb_mul_out_tmp   <= mul_div_out;
        wb_mul_valid_tmp <= 'b1;
    end else begin
        wb_mul_dst_Paddr_tmp <= 'b0;
        wb_mul_out_tmp   <= 'b0;
        wb_mul_valid_tmp <= 'b0;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        wb_mul_dst_Paddr_tmp_r1 <= 'b0;
        wb_mul_out_tmp_r1   <= 'b0;
        wb_mul_valid_tmp_r1 <= 'b0;
    end else begin
        wb_mul_dst_Paddr_tmp_r1 <= wb_mul_dst_Paddr_tmp;
        wb_mul_out_tmp_r1   <= wb_mul_out_tmp;
        wb_mul_valid_tmp_r1 <= wb_mul_valid_tmp;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        wb_mul_dst_Paddr_tmp_r2 <= 'b0;
        wb_mul_out_tmp_r2   <= 'b0;
        wb_mul_valid_tmp_r2 <= 'b0;
    end else begin
        wb_mul_dst_Paddr_tmp_r2 <= wb_mul_dst_Paddr_tmp_r1;
        wb_mul_out_tmp_r2   <= wb_mul_out_tmp_r1;
        wb_mul_valid_tmp_r2 <= wb_mul_valid_tmp_r1;
    end
end

// ------------------------div-------------------------------------------
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        wb_div_dst_Paddr_tmp <= 'b0;
        wb_div_out_tmp   <= 'b0;
        wb_div_valid_tmp <= 'b0;
    end else if (mul_div_issue_en_out && div_en) begin
        wb_div_dst_Paddr_tmp <= mul_div_issue_queue_Pdst_out;
        wb_div_out_tmp   <= mul_div_out;
        wb_div_valid_tmp <= 'b1;
    end else begin
        wb_div_dst_Paddr_tmp <= 'b0;
        wb_div_out_tmp   <= 'b0;
        wb_div_valid_tmp <= 'b0;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        wb_div_dst_Paddr_tmp_r1 <= 'b0;
        wb_div_out_tmp_r1   <= 'b0;
        wb_div_valid_tmp_r1 <= 'b0;
    end else begin
        wb_div_dst_Paddr_tmp_r1 <= wb_div_dst_Paddr_tmp;
        wb_div_out_tmp_r1   <= wb_div_out_tmp;
        wb_div_valid_tmp_r1 <= wb_div_valid_tmp;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        wb_div_dst_Paddr_tmp_r2 <= 'b0;
        wb_div_out_tmp_r2   <= 'b0;
        wb_div_valid_tmp_r2 <= 'b0;
    end else begin
        wb_div_dst_Paddr_tmp_r2 <= wb_div_dst_Paddr_tmp_r1;
        wb_div_out_tmp_r2   <= wb_div_out_tmp_r1;
        wb_div_valid_tmp_r2 <= wb_div_valid_tmp_r1;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        wb_div_dst_Paddr_tmp_r3 <= 'b0;
        wb_div_out_tmp_r3   <= 'b0;
        wb_div_valid_tmp_r3 <= 'b0;
    end else begin
        wb_div_dst_Paddr_tmp_r3 <= wb_div_dst_Paddr_tmp_r2;
        wb_div_out_tmp_r3   <= wb_div_out_tmp_r2;
        wb_div_valid_tmp_r3 <= wb_div_valid_tmp_r2;
    end
end
//-----------------------------------------------------------------------

assign  wb_div_dst_Paddr= wb_div_dst_Paddr_tmp_r3; 
assign  wb_div_out      = wb_div_out_tmp_r3;
assign  wb_div_valid    = wb_div_valid_tmp_r3;
assign  wb_mul_dst_Paddr= wb_mul_dst_Paddr_tmp_r2;
assign  wb_mul_out      = wb_mul_out_tmp_r2;
assign  wb_mul_valid    = wb_mul_valid_tmp_r2;

assign mul_ready        = !wb_mul_valid_tmp && !wb_mul_valid_tmp_r1;
assign div_ready        = !wb_div_valid_tmp && !wb_div_valid_tmp_r1 && !wb_div_valid_tmp_r2;

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
    #1 begin
       id_alu_op             = `ALU_OP_NOP;
       id_pc                 = 0;
       id_imm                = 0;
       id_alloc_rob          = 0;
       rs1_rat_valid         = 0;
       rs1_Paddr             = 0;
       rs2_rat_valid         = 0;
       rs2_Paddr             = 0;
       rs1_value_fromGPR     = 0;
       rs2_value_fromGPR     = 0;
       rob_commit_branch_taken = 0;
       rob_commit_exp_en     = 0;
       mem_ready             = 1;
    end
    @(posedge clk)
    #1  begin  // P1: mul x10 = 10 * 20 = 200
            id_alu_op             = (!mul_div_issue_queue_full)?`ALU_OP_MUL : 0;
            id_pc                 = (!mul_div_issue_queue_full)?4:0;
            id_imm                = 0;
            id_alloc_rob          = 1;
            rs1_rat_valid         = 0;
            rs1_Paddr             = 0;
            rs2_rat_valid         = 0;
            rs2_Paddr             = 0;
            rs1_value_fromGPR     = 10;
            rs2_value_fromGPR     = 20;
            rob_commit_branch_taken = 0;
            rob_commit_exp_en     = 0;
            mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin  // P2: mul x11 = x10 * 2 = 400
        id_alu_op             = (!mul_div_issue_queue_full)? `ALU_OP_MUL : 0;
        id_pc                 = (!mul_div_issue_queue_full)? 8 :0;
        id_imm                = 0;
        id_alloc_rob          = 2;
        rs1_rat_valid         = 1;
        rs1_Paddr             = 1;
        rs2_rat_valid         = 0;
        rs2_Paddr             = 0;
        rs1_value_fromGPR     = 0;
        rs2_value_fromGPR     = 2;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin  
        // P3: addi x12 = x11 + 36 = 40 (rs2/rs1)
        id_alu_op             = (!alu_issue_queue_full)? `ALU_OP_ADDI : `ALU_OP_NOP;
        id_pc                 = (!alu_issue_queue_full)? 12:0;
        id_imm                = 0;
        id_alloc_rob          = 3;
        rs1_rat_valid         = 0;
        rs1_Paddr             = 0;
        rs2_rat_valid         = 1;
        rs2_Paddr             = 2;
        rs1_value_fromGPR     = 36;
        rs2_value_fromGPR     = 0;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin  // P4: mul x13 = x11 *x12 = 400*40 = 16000
        id_alu_op             = (!mul_div_issue_queue_full)? `ALU_OP_MUL : `ALU_OP_NOP;
        id_pc                 = (!mul_div_issue_queue_full)? 16 : 0;
        id_imm                = 0;
        id_alloc_rob          = 4;
        rs1_rat_valid         = 1;
        rs1_Paddr             = 2;
        rs2_rat_valid         = 1;
        rs2_Paddr             = 3;
        rs1_value_fromGPR     = 0;
        rs2_value_fromGPR     = 0;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1  begin  // P5: div x14 = x13/x11 = 16000/400 = 40
        id_alu_op             = (!mul_div_issue_queue_full)? `ALU_OP_DIV : `ALU_OP_NOP;
        id_pc                 = (!mul_div_issue_queue_full)? 20:0;
        id_imm                = 0;
        id_alloc_rob          = 5;
        rs1_rat_valid         = 1;
        rs1_Paddr             = 2;
        rs2_rat_valid         = 1;
        rs2_Paddr             = 4;
        rs1_value_fromGPR     = 0;
        rs2_value_fromGPR     = 0;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin  // P6: div x15 = 10/2 = 5
        id_alu_op             = (!mul_div_issue_queue_full)? `ALU_OP_DIV : `ALU_OP_NOP;
        id_pc                 = (!mul_div_issue_queue_full)? 24 : 0;
        id_imm                = 0;
        id_alloc_rob          = 6;
        rs1_rat_valid         = 0;
        rs1_Paddr             = 0;
        rs2_rat_valid         = 0;
        rs2_Paddr             = 0;
        rs1_value_fromGPR     = 2;
        rs2_value_fromGPR     = 10;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin   // p7: mul x16= x10*x11=80000
        id_alu_op             = (!mul_div_issue_queue_full)? `ALU_OP_MUL : `ALU_OP_NOP;
        id_pc                 = (!mul_div_issue_queue_full)? 28:0;
        id_imm                = 0;
        id_alloc_rob          = 7;
        rs1_rat_valid         = 0;
        rs1_Paddr             = 0;
        rs2_rat_valid         = 0;
        rs2_Paddr             = 0;
        rs1_value_fromGPR     = 200;
        rs2_value_fromGPR     = 400;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin   // p8: mul x17= x10*x12= 200 * 40 = 8000
        id_alu_op             = (!mul_div_issue_queue_full)? `ALU_OP_MUL : `ALU_OP_NOP;
        id_pc                 = (!mul_div_issue_queue_full)? 32:0;
        id_imm                = 0;
        id_alloc_rob          = 8;
        rs1_rat_valid         = 0;
        rs1_Paddr             = 0;
        rs2_rat_valid         = 0;
        rs2_Paddr             = 0;
        rs1_value_fromGPR     = 200;
        rs2_value_fromGPR     = 40;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin   // p8: mem: store M[rs1+imm][]=rs2[] = M[x14+imm][]=X1[]
        id_alu_op             = `ALU_OP_NOP;
        id_mem_op             = (!mem_issue_queue_full)? `MEM_OP_SW : `MEM_OP_NOP;
        id_pc                 = (!mem_issue_queue_full)? 36:0;
        id_imm                = 10;
        id_alloc_rob          = 9;
        rs1_rat_valid         = 1;
        rs1_Paddr             = 5;
        rs2_rat_valid         = 0;
        rs2_Paddr             = 0;
        rs1_value_fromGPR     = 0;
        rs2_value_fromGPR     = 31;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin   // p9: mem: store M[rs1+imm][]=rs2[] = M[x15+imm][]=X1[]
        id_alu_op             = `ALU_OP_NOP;
        id_mem_op             = (!mem_issue_queue_full)? `MEM_OP_SB : `MEM_OP_NOP;
        id_pc                 = (!mem_issue_queue_full)? 40:0;
        id_imm                = 20;
        id_alloc_rob          = 10;
        rs1_rat_valid         = 1;
        rs1_Paddr             = 6;
        rs2_rat_valid         = 0;
        rs2_Paddr             = 0;
        rs1_value_fromGPR     = 0;
        rs2_value_fromGPR     = 31; //x1=31
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin   // p10: mem: store M[rs1+imm][]=rs2[] = M[x10+imm][]= x1[]
        id_alu_op             = `ALU_OP_NOP;
        id_mem_op             = (!mem_issue_queue_full)? `MEM_OP_SH : `MEM_OP_NOP;
        id_pc                 = (!mem_issue_queue_full)? 44:0;
        id_imm                = 30;
        id_alloc_rob          = 11;
        rs1_rat_valid         = 0;
        rs1_Paddr             = 0;
        rs2_rat_valid         = 0;
        rs2_Paddr             = 0;
        rs1_value_fromGPR     = 10;
        rs2_value_fromGPR     = 31;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin   // p11: mem: store M[rs1+imm][]=rs2[] = M[x10+imm][]= x1[]
        id_alu_op             = `ALU_OP_NOP;
        id_mem_op             = (!mem_issue_queue_full)? `MEM_OP_SH : `MEM_OP_NOP;
        id_pc                 = (!mem_issue_queue_full)? 48:0;
        id_imm                = 40;
        id_alloc_rob          = 12;
        rs1_rat_valid         = 0;
        rs1_Paddr             = 0;
        rs2_rat_valid         = 0;
        rs2_Paddr             = 0;
        rs1_value_fromGPR     = 10;
        rs2_value_fromGPR     = 31;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin   // p12: mem: store M[rs1+imm][]=rs2[] = M[x11+imm][]= x1[]
        id_alu_op             = `ALU_OP_NOP;
        id_mem_op             = (!mem_issue_queue_full)? `MEM_OP_SH : `MEM_OP_NOP;
        id_pc                 = (!mem_issue_queue_full)? 52:0;
        id_imm                = 50;
        id_alloc_rob          = 13;
        rs1_rat_valid         = 0;
        rs1_Paddr             = 0;
        rs2_rat_valid         = 0;
        rs2_Paddr             = 0;
        rs1_value_fromGPR     = 400;
        rs2_value_fromGPR     = 31;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    @(posedge clk)
    #1 begin
        id_alu_op             = `ALU_OP_NOP;
        id_mem_op             = `MEM_OP_NOP;
        id_pc                 = 0;
        id_imm                = 0;
        id_alloc_rob          = 0;
        rs1_rat_valid         = 0;
        rs1_Paddr             = 0;
        rs2_rat_valid         = 0;
        rs2_Paddr             = 0;
        rs1_value_fromGPR     = 0;
        rs2_value_fromGPR     = 0;
        rob_commit_branch_taken = 0;
        rob_commit_exp_en     = 0;
        mem_ready             = 1;
    end
    #200 $finish;
end

initial begin
    $fsdbDumpfile("wave_issue_queue_ctrl.fsdb");
    $fsdbDumpvars();
    $fsdbDumpMDA();
end

endmodule
