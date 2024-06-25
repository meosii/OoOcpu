`ifndef OOOCPU_ISSUE_QUEUE_OOO_MD
`define OOOCPU_ISSUE_QUEUE_OOO_MD
`include "define.v"
module issue_queue_OoO_MD #(
    parameter OP_WIDTH
)(
    input wire                                  clk,
    input wire                                  rst_n,
    
    // from issue queue ctrl
    input wire                                  issue2queue_en,
    input wire [OP_WIDTH - 1 : 0]               id_op,      // include div and mul
    input wire [`PC_WIDTH-1 : 0]                id_pc,
    input wire [`WORD_WIDTH-1 : 0]              id_imm,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       id_alloc_rob,   // from rob -> if_reg

    input wire                                  rs1_rat_valid,  // when rat_valid==0, rs1_value_fromGPR is valid.
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       rs1_Paddr,
    input wire                                  rs2_rat_valid,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       rs2_Paddr,
    input wire [`WORD_WIDTH-1 : 0]              rs1_value_fromGPR,
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

    // FU ready: MUL DIV
    input wire                                  mul_ready,
    input wire                                  div_ready,

    // outputs
    // to FU
    output wire                                 issue_en_out,
    output wire [OP_WIDTH-1 : 0]                issue_queue_op_out,
    output wire [`PC_WIDTH-1 : 0]               issue_queue_pc_out,
    output wire [`WORD_WIDTH-1 : 0]             issue_queue_imm_out,
    output wire [`WORD_WIDTH-1 : 0]             issue_queue_rs1_value_out,
    output wire [`WORD_WIDTH-1 : 0]             issue_queue_rs2_value_out,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      issue_queue_Pdst_out,
    // 
    output wire                                 issue_queue_full
);

// -----------------------------------issue queue---------------------------------------------------------
reg [$clog2(`ROB_DEPTH)-1 : 0]  issue_queue_Prs1                [0 : `ISSUE_QUEUE_DEPTH-1];
reg                             issue_queue_Prs1_valid          [0 : `ISSUE_QUEUE_DEPTH-1];
reg [$clog2(`ROB_DEPTH)-1 : 0]  issue_queue_Prs2                [0 : `ISSUE_QUEUE_DEPTH-1];
reg                             issue_queue_Prs2_valid          [0 : `ISSUE_QUEUE_DEPTH-1];
reg [$clog2(`ROB_DEPTH)-1 : 0]  issue_queue_Pdst                [0 : `ISSUE_QUEUE_DEPTH-1]; // alloc rob
wire                            issue_queue_valid               [0 : `ISSUE_QUEUE_DEPTH-1]; // not be refresh
wire                            issue_queue_ready               [0 : `ISSUE_QUEUE_DEPTH-1]; // data1 and data2 ready
// -----------------------------------payload RAM---------------------------------------------------------
reg [OP_WIDTH-1 : 0]            issue_queue_op                  [0 : `ISSUE_QUEUE_DEPTH-1];
reg [`PC_WIDTH-1 : 0]           issue_queue_pc                  [0 : `ISSUE_QUEUE_DEPTH-1];
reg [`WORD_WIDTH-1 : 0]         issue_queue_imm                 [0 : `ISSUE_QUEUE_DEPTH-1];
reg [`WORD_WIDTH-1 : 0]         issue_queue_rs1_value           [0 : `ISSUE_QUEUE_DEPTH-1];
reg [`WORD_WIDTH-1 : 0]         issue_queue_rs2_value           [0 : `ISSUE_QUEUE_DEPTH-1];
//--------------------------------------------------------------------------------------------------------

// issue queue(fifo): compressed structure, issue_queue[0] is the oldest insn in the queue,
// is easy to arbiter which one issue to FU
wire                            fifo_empty;
wire                            fifo_full;
wire                            issue_en;
wire [1:0]                      issue_tag;
wire                            fu_ready;
wire                            fifo_write_en;
wire                            fifo_issue_en;
reg [`ISSUE_QUEUE_DEPTH : 0]    fifo_p;         // push:'b0_0001 -> 'b0_0010 -> 'b0_0100 -> 'b0_1000 -> 'b1_0000(full)
                                                // pop: 'b1_0000(full) -> 'b0_1000 -> 'b0_0100 -> 'b0_0010 -> 'b0_0001(empty)
                                                // `ISSUE_QUEUE_DEPTH = 4
assign fifo_empty       =   fifo_p[0];
assign fifo_full        =   fifo_p[`ISSUE_QUEUE_DEPTH];

assign issue_queue_full = fifo_full;

assign issue_queue_valid[0] = !fifo_p[0];
assign issue_queue_valid[1] = !fifo_p[0] && !fifo_p[1];
assign issue_queue_valid[2] = !fifo_p[0] && !fifo_p[1] && !fifo_p[2];
assign issue_queue_valid[3] = !fifo_p[0] && !fifo_p[1] && !fifo_p[2] && !fifo_p[3];


assign issue_en         =   (issue_queue_ready[0] && issue_queue_valid[0])   ||
                            (issue_queue_ready[1] && issue_queue_valid[1])   ||
                            (issue_queue_ready[2] && issue_queue_valid[2])   ||
                            (issue_queue_ready[3] && issue_queue_valid[3]);

assign issue_tag        =   (issue_queue_ready[0] && issue_queue_valid[0])?   2'd0  :
                            (issue_queue_ready[1] && issue_queue_valid[1])?   2'd1  :
                            (issue_queue_ready[2] && issue_queue_valid[2])?   2'd2  :
                            (issue_queue_ready[3] && issue_queue_valid[3])?   2'd3  :   2'd0;

assign fu_ready         =  ((issue_queue_op[issue_tag] == `ALU_OP_MUL   ) ||
                            (issue_queue_op[issue_tag] == `ALU_OP_MULH  ) ||
                            (issue_queue_op[issue_tag] == `ALU_OP_MULHSU) ||
                            (issue_queue_op[issue_tag] == `ALU_OP_MULHU )   )?  mul_ready   :
                           ((issue_queue_op[issue_tag] == `ALU_OP_DIV   ) ||
                            (issue_queue_op[issue_tag] == `ALU_OP_DIVU  ) ||
                            (issue_queue_op[issue_tag] == `ALU_OP_REM   ) ||
                            (issue_queue_op[issue_tag] == `ALU_OP_REMU  )   )?  div_ready   : 1'b0;

assign fifo_write_en    =   !fifo_full  && issue2queue_en;
assign fifo_issue_en    =   !fifo_empty && issue_en && fu_ready;

assign issue_en_out     =   fifo_issue_en;

assign issue_queue_op_out           =   issue_queue_op          [issue_tag];
assign issue_queue_pc_out           =   issue_queue_pc          [issue_tag];
assign issue_queue_imm_out          =   issue_queue_imm         [issue_tag];
assign issue_queue_rs1_value_out    =   issue_queue_rs1_value   [issue_tag];
assign issue_queue_rs2_value_out    =   issue_queue_rs2_value   [issue_tag];
assign issue_queue_Pdst_out         =   issue_queue_Pdst        [issue_tag];

// ------------------------ writeback-------------------------------------------------------------------------------------
wire [`WORD_WIDTH-1 : 0]    writeback_rs1_data  [0 : `ISSUE_QUEUE_DEPTH-1];
wire [`WORD_WIDTH-1 : 0]    writeback_rs2_data  [0 : `ISSUE_QUEUE_DEPTH-1];
wire                        writeback_rs1_en    [0 : `ISSUE_QUEUE_DEPTH-1];
wire                        writeback_rs2_en    [0 : `ISSUE_QUEUE_DEPTH-1];

wire                        wb_alu_in_writers1;
wire                        wb_alu_in_writers2;
wire                        wb_div_in_writers1;
wire                        wb_div_in_writers2;
wire                        wb_mul_in_writers1;
wire                        wb_mul_in_writers2;
wire                        wb_load_in_writers1;
wire                        wb_load_in_writers2;
wire                        wb_br_in_writers1;
wire                        wb_br_in_writers2;

assign wb_alu_in_writers1   = (wb_alu_dst_Paddr == rs1_Paddr)   && rs1_rat_valid && fifo_write_en;
assign wb_alu_in_writers2   = (wb_alu_dst_Paddr == rs2_Paddr)   && rs2_rat_valid && fifo_write_en;
assign wb_div_in_writers1   = (wb_div_dst_Paddr == rs1_Paddr)   && rs1_rat_valid && fifo_write_en;
assign wb_div_in_writers2   = (wb_div_dst_Paddr == rs2_Paddr)   && rs2_rat_valid && fifo_write_en;
assign wb_mul_in_writers1   = (wb_mul_dst_Paddr == rs1_Paddr)   && rs1_rat_valid && fifo_write_en;
assign wb_mul_in_writers2   = (wb_mul_dst_Paddr == rs2_Paddr)   && rs2_rat_valid && fifo_write_en;
assign wb_load_in_writers1  = (wb_load_dst_Paddr== rs1_Paddr)   && rs1_rat_valid && fifo_write_en;
assign wb_load_in_writers2  = (wb_load_dst_Paddr== rs2_Paddr)   && rs2_rat_valid && fifo_write_en;
assign wb_br_in_writers1    = (wb_br_rob        == rs1_Paddr)   && rs1_rat_valid && fifo_write_en;
assign wb_br_in_writers2    = (wb_br_rob        == rs2_Paddr)   && rs2_rat_valid && fifo_write_en;

genvar i;
    // rs1
generate
    for (i=0; i<`ISSUE_QUEUE_DEPTH; i++) begin: GENERATE_ISSUE_WRITEBACK_RS1EN
        assign    writeback_rs1_en[i] = (((issue_queue_valid[i] && (wb_alu_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_alu_in_writers1 && fifo_p[i])) && wb_alu_valid)   ||
                                        (((issue_queue_valid[i] && (wb_div_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_div_in_writers1 && fifo_p[i])) && wb_div_valid)   ||
                                        (((issue_queue_valid[i] && (wb_mul_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_mul_in_writers1 && fifo_p[i])) && wb_mul_valid)   ||
                                        (((issue_queue_valid[i] && (wb_load_dst_Paddr== issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_load_in_writers1&& fifo_p[i])) && wb_load_valid)  ||
                                        (((issue_queue_valid[i] && (wb_br_rob        == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_br_in_writers1  && fifo_p[i])) && wb_br_out_valid);
    end
endgenerate

generate
    for (i=0; i<`ISSUE_QUEUE_DEPTH; i++) begin: GENERATE_ISSUE_WRITEBACK_RS1DATA
        assign    writeback_rs1_data[i] =   ((((wb_alu_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_alu_in_writers1 && fifo_p[i])) && wb_alu_valid )?    wb_alu_out   :
                                            ((((wb_div_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_div_in_writers1 && fifo_p[i])) && wb_div_valid )?    wb_div_out   :
                                            ((((wb_mul_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_mul_in_writers1 && fifo_p[i])) && wb_mul_valid )?    wb_mul_out   :
                                            ((((wb_load_dst_Paddr== issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_load_in_writers1&& fifo_p[i])) && wb_load_valid)?    wb_load_data :
                                            ((((wb_br_rob        == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_br_in_writers1  && fifo_p[i])) && wb_br_out_valid)?  wb_br_out    :   'b0;
    end
endgenerate
    // rs2
generate 
    for (i=0; i<`ISSUE_QUEUE_DEPTH; i++) begin: GENERATE_ISSUE_WRITEBACK_RS2EN
        assign    writeback_rs2_en[i] = (((issue_queue_valid[i] && (wb_alu_dst_Paddr == issue_queue_Prs2[i]) && issue_queue_Prs2_valid[i]) || (wb_alu_in_writers2 && fifo_p[i])) && wb_alu_valid)   ||
                                        (((issue_queue_valid[i] && (wb_div_dst_Paddr == issue_queue_Prs2[i]) && issue_queue_Prs2_valid[i]) || (wb_div_in_writers2 && fifo_p[i])) && wb_div_valid)   ||
                                        (((issue_queue_valid[i] && (wb_mul_dst_Paddr == issue_queue_Prs2[i]) && issue_queue_Prs2_valid[i]) || (wb_mul_in_writers2 && fifo_p[i])) && wb_mul_valid)   ||
                                        (((issue_queue_valid[i] && (wb_load_dst_Paddr== issue_queue_Prs2[i]) && issue_queue_Prs2_valid[i]) || (wb_load_in_writers2&& fifo_p[i])) && wb_load_valid)  ||
                                        (((issue_queue_valid[i] && (wb_br_rob        == issue_queue_Prs2[i]) && issue_queue_Prs2_valid[i]) || (wb_br_in_writers2  && fifo_p[i])) && wb_br_out_valid);
    end
endgenerate

generate
    for (i=0; i<`ISSUE_QUEUE_DEPTH; i++) begin: GENERATE_ISSUE_WRITEBACK_RS2DATA
        assign    writeback_rs2_data[i] =   ((((wb_alu_dst_Paddr == issue_queue_Prs2[i]) && issue_queue_Prs2_valid[i]) || (wb_alu_in_writers2 && fifo_p[i])) && wb_alu_valid )?    wb_alu_out   :
                                            ((((wb_div_dst_Paddr == issue_queue_Prs2[i]) && issue_queue_Prs2_valid[i]) || (wb_div_in_writers2 && fifo_p[i])) && wb_div_valid )?    wb_div_out   :
                                            ((((wb_mul_dst_Paddr == issue_queue_Prs2[i]) && issue_queue_Prs2_valid[i]) || (wb_mul_in_writers2 && fifo_p[i])) && wb_mul_valid )?    wb_mul_out   :
                                            ((((wb_load_dst_Paddr== issue_queue_Prs2[i]) && issue_queue_Prs2_valid[i]) || (wb_load_in_writers2&& fifo_p[i])) && wb_load_valid)?    wb_load_data :
                                            ((((wb_br_rob        == issue_queue_Prs2[i]) && issue_queue_Prs2_valid[i]) || (wb_br_in_writers2  && fifo_p[i])) && wb_br_out_valid)?  wb_br_out    :   'b0;
    end
endgenerate

// -------------------------------------------------------------------------------------------------------------
// fifo_p: "1" means the next clock to write
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        fifo_p <= 'b1;
    end else if (rob_commit_br_taken || rob_commit_exp_en) begin    // refresh the issue_queue
        fifo_p <= 'b1;
    end else if (fifo_write_en && !fifo_issue_en) begin                 // only write in issue_queue
        fifo_p <= fifo_p << 1;
    end else if (fifo_issue_en && !fifo_write_en) begin                 // only read to FU
        fifo_p <= fifo_p >> 1;
    end
end
// ----------------------------------write or shift the issus queue---------------------------------------------
// write and issue
genvar j;
generate
    for (j=0; j<`ISSUE_QUEUE_DEPTH-1; j++) begin: GENERATE_W_S
        always @(posedge clk) begin
            if (fifo_write_en && fifo_p[j] && !fifo_issue_en) begin         // only write in [j]
                issue_queue_Prs1        [j] <= rs1_Paddr;
                issue_queue_Prs2        [j] <= rs2_Paddr;
                issue_queue_Pdst        [j] <= id_alloc_rob;
                issue_queue_op          [j] <= id_op;
                issue_queue_pc          [j] <= id_pc;
                issue_queue_imm         [j] <= id_imm;
            end else if (fifo_write_en && fifo_p[j+1] && fifo_issue_en) begin   // write in [j+1] and issue
                issue_queue_Prs1        [j] <= rs1_Paddr;
                issue_queue_Prs2        [j] <= rs2_Paddr;
                issue_queue_Pdst        [j] <= id_alloc_rob;
                issue_queue_op          [j] <= id_op;
                issue_queue_pc          [j] <= id_pc;
                issue_queue_imm         [j] <= id_imm;
            end else if (fifo_issue_en && (issue_tag<=j)) begin             // only issue, compressed
                issue_queue_Prs1        [j] <= issue_queue_Prs1     [j+1];
                issue_queue_Prs2        [j] <= issue_queue_Prs2     [j+1];
                issue_queue_Pdst        [j] <= issue_queue_Pdst     [j+1];
                issue_queue_op          [j] <= issue_queue_op   [j+1];
                issue_queue_pc          [j] <= issue_queue_pc       [j+1];
                issue_queue_imm         [j] <= issue_queue_imm      [j+1];
            end                                                             // not write and not issue, hold
        end
    end
endgenerate

// when issue, issue_queue[`ISSUE_QUEUE_DEPTH-1] stays the same, because it's valid = 0.
always @(posedge clk) begin
    if (fifo_write_en && fifo_p[`ISSUE_QUEUE_DEPTH-1] && !fifo_issue_en) begin  // write in
        issue_queue_Prs1        [`ISSUE_QUEUE_DEPTH-1] <= rs1_Paddr;
        issue_queue_Prs2        [`ISSUE_QUEUE_DEPTH-1] <= rs2_Paddr;
        issue_queue_Pdst        [`ISSUE_QUEUE_DEPTH-1] <= id_alloc_rob;
        issue_queue_op          [`ISSUE_QUEUE_DEPTH-1] <= id_op;
        issue_queue_pc          [`ISSUE_QUEUE_DEPTH-1] <= id_pc;
        issue_queue_imm         [`ISSUE_QUEUE_DEPTH-1] <= id_imm;
    end
end

//------------------------Prs1_valid=0 means data1 ready---------------------------------------------------------------
// time: write in queue, writeback replace the writeindata, then issue to shift
// judje priority: first determine the valid conditions, wb -> write
generate
    for (j=0; j<`ISSUE_QUEUE_DEPTH-1; j++) begin: GENERATE_RATRS1VALID
        always @(posedge clk) begin
            if (writeback_rs1_en[j] && !(fifo_issue_en && (issue_tag<=j))) begin        // writeback in [j] without issue
                issue_queue_Prs1_valid[j] <= 1'b0;
            end else if (writeback_rs1_en[j+1] && (fifo_issue_en && (issue_tag<=j))) begin  // writeback in [j+1] and issue
                issue_queue_Prs1_valid[j] <= 1'b0;
            end else if (fifo_write_en && fifo_p[j] && !fifo_issue_en) begin                // write in [j] without issue
                issue_queue_Prs1_valid[j] <= rs1_rat_valid;
            end else if (fifo_write_en && fifo_p[j+1] && fifo_issue_en) begin               // write in [j+1] and issue
                issue_queue_Prs1_valid[j] <= rs1_rat_valid;
            end else if (fifo_issue_en && (issue_tag<=j)) begin                             // only issue
                issue_queue_Prs1_valid[j] <= issue_queue_Prs1_valid  [j+1];
            end
        end
    end
endgenerate

// when issue, issue_queue[`ISSUE_QUEUE_DEPTH-1] stays the same, because it's valid = 0.
always @(posedge clk) begin
    if (fifo_write_en && fifo_p[`ISSUE_QUEUE_DEPTH-1] && !fifo_issue_en) begin  // only write in [`ISSUE_QUEUE_DEPTH-1]
        issue_queue_Prs1_valid  [`ISSUE_QUEUE_DEPTH-1] <= rs1_rat_valid;
    end else if (writeback_rs1_en[`ISSUE_QUEUE_DEPTH-1]) begin                  // writeback in [`ISSUE_QUEUE_DEPTH-1]
        issue_queue_Prs1_valid  [`ISSUE_QUEUE_DEPTH-1] <= 1'b0;
    end                                                                             // not write, hold
end

//------------------------Prs2_valid=0 means data2 ready--------------------------------------------------------------
generate
    for (j=0; j<`ISSUE_QUEUE_DEPTH-1; j++) begin: GENERATE_RATRS2VALID
        always @(posedge clk) begin
            if (writeback_rs2_en[j] && !(fifo_issue_en && (issue_tag<=j))) begin        // writeback in [j] without issue
                issue_queue_Prs2_valid[j] <= 1'b0;
            end else if (writeback_rs2_en[j+1] && (fifo_issue_en && (issue_tag<=j))) begin  // writeback in [j+1] and issue
                issue_queue_Prs2_valid[j] <= 1'b0;
            end else if (fifo_write_en && fifo_p[j] && !fifo_issue_en) begin                // write in [j] without issue
                issue_queue_Prs2_valid[j] <= rs2_rat_valid;
            end else if (fifo_write_en && fifo_p[j+1] && fifo_issue_en) begin               // write in [j+1] and issue
                issue_queue_Prs2_valid[j] <= rs2_rat_valid;
            end else if (fifo_issue_en && (issue_tag<=j)) begin                             // only issue
                issue_queue_Prs2_valid[j] <= issue_queue_Prs2_valid  [j+1];
            end
        end
    end
endgenerate

// when issue, issue_queue[`ISSUE_QUEUE_DEPTH-1] stays the same, because it's valid = 0.
always @(posedge clk) begin
    if (fifo_write_en && fifo_p[`ISSUE_QUEUE_DEPTH-1] && !fifo_issue_en) begin      // only write in [`ISSUE_QUEUE_DEPTH-1]
        issue_queue_Prs2_valid  [`ISSUE_QUEUE_DEPTH-1] <= rs2_rat_valid;
    end else if (writeback_rs2_en[`ISSUE_QUEUE_DEPTH-1]) begin                      // writeback in [`ISSUE_QUEUE_DEPTH-1]
        issue_queue_Prs2_valid  [`ISSUE_QUEUE_DEPTH-1] <= 1'b0;
    end                                                                                 // not write, hold
end
//----------------------------------data1 value and data2 value-----------------------------------------
// write and issue and writeback
// input from the earliest prepared numeric value (this number does not change later)
generate
    for (j=0; j<`ISSUE_QUEUE_DEPTH-1; j++) begin: GENERATE_DATA1
        always @(posedge clk) begin
            if (fifo_write_en && fifo_p[j] && !fifo_issue_en && !rs1_rat_valid) begin           // only write in [j]
                issue_queue_rs1_value[j]    <= rs1_value_fromGPR;
            end else if (fifo_write_en && fifo_p[j+1] && fifo_issue_en && !rs1_rat_valid) begin // write in [j+1] and issue
                issue_queue_rs1_value[j]    <= rs1_value_fromGPR;
            end else if (writeback_rs1_en[j+1] && (fifo_issue_en && (issue_tag<=j))) begin      // writeback in [j+1] and issue
                issue_queue_rs1_value[j]    <= writeback_rs1_data[j+1];
            end else if (writeback_rs1_en[j] && !(fifo_issue_en && (issue_tag<=j))) begin       // writeback in [j] without issue
                issue_queue_rs1_value[j]    <= writeback_rs1_data[j];
            end else if (fifo_issue_en && (issue_tag<=j)) begin                                 // only issue
                issue_queue_rs1_value[j]    <= issue_queue_rs1_value[j+1];
            end 
        end
    end
endgenerate

// when issue, issue_queue[`ISSUE_QUEUE_DEPTH-1] stays the same, because it's valid = 0.
always @(posedge clk) begin
    if (fifo_write_en && fifo_p[`ISSUE_QUEUE_DEPTH-1] && !fifo_issue_en && !rs1_rat_valid) begin    // only write in [`ISSUE_QUEUE_DEPTH-1]
        issue_queue_rs1_value[`ISSUE_QUEUE_DEPTH-1] <= rs1_value_fromGPR;
    end else if (writeback_rs1_en[`ISSUE_QUEUE_DEPTH-1]) begin                          // writeback in [`ISSUE_QUEUE_DEPTH-1] without issue
        issue_queue_rs1_value[`ISSUE_QUEUE_DEPTH-1] <= writeback_rs1_data[`ISSUE_QUEUE_DEPTH-1];
    end
end

generate
    for (j=0; j<`ISSUE_QUEUE_DEPTH-1; j++) begin: GENERATE_DATA2
        always @(posedge clk) begin
            if (fifo_write_en && fifo_p[j] && !fifo_issue_en && !rs2_rat_valid) begin           // only write in [j]
                issue_queue_rs2_value[j]    <= rs2_value_fromGPR;
            end else if (fifo_write_en && fifo_p[j+1] && fifo_issue_en && !rs2_rat_valid) begin // write in [j+1] and issue
                issue_queue_rs2_value[j]    <= rs2_value_fromGPR;
            end else if (writeback_rs2_en[j+1] && (fifo_issue_en && (issue_tag<=j))) begin      // writeback in [j+1] and issue
                issue_queue_rs2_value[j]    <= writeback_rs2_data[j+1];
            end else if (writeback_rs2_en[j] && !(fifo_issue_en && (issue_tag<=j))) begin       // writeback in [j] without issue
                issue_queue_rs2_value[j]    <= writeback_rs2_data[j];
            end else if (fifo_issue_en && (issue_tag<=j)) begin                                 // only issue
                issue_queue_rs2_value[j]    <= issue_queue_rs2_value[j+1];
            end 
        end
    end
endgenerate

// when issue, issue_queue[`ISSUE_QUEUE_DEPTH-1] stays the same, because it's valid = 0.
always @(posedge clk) begin
    if (fifo_write_en && fifo_p[`ISSUE_QUEUE_DEPTH-1] && !fifo_issue_en && !rs2_rat_valid) begin    // only write in [`ISSUE_QUEUE_DEPTH-1]
        issue_queue_rs2_value[`ISSUE_QUEUE_DEPTH-1] <= rs2_value_fromGPR;
    end  else if (writeback_rs2_en[`ISSUE_QUEUE_DEPTH-1]) begin                         // writeback in [`ISSUE_QUEUE_DEPTH-1] without issue
        issue_queue_rs2_value[`ISSUE_QUEUE_DEPTH-1] <= writeback_rs2_data[`ISSUE_QUEUE_DEPTH-1];
    end
end
//------------------------------------issue_queue_ready------------------------------------------------------------
generate
    for (j=0; j<`ISSUE_QUEUE_DEPTH; j++) begin: GENERATE_READY
        assign issue_queue_ready[j] = !issue_queue_Prs1_valid[j] && !issue_queue_Prs2_valid[j];
    end
endgenerate

endmodule
`endif
