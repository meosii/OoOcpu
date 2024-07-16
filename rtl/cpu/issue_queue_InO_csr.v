`ifndef OOOCPU_ISSUE_QUEUE_INO_CSR
`define OOOCPU_ISSUE_QUEUE_INO_CSR
module issue_queue_InO_csr #(
    parameter OP_WIDTH
)(
    input wire                                  clk,
    input wire                                  rst_n,
    
    // from issue queue ctrl
    input wire                                  issue2queue_en,
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       id_alloc_rob,   // from rob -> if_reg

    input wire                                  rs1_rat_valid,  // when rat_valid==0, rs1_value_fromGPR is valid.
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       rs1_Paddr,
    input wire [`WORD_WIDTH-1 : 0]              rs1_value_fromGPR,
    
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
        // from csr
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       wb_csr_dst_Paddr,
    input wire [`WORD_WIDTH - 1 : 0]            wb_csr_data,
    input wire                                  wb_csr_valid,

    // branch and exception
    input wire                                  rob_commit_br_taken,    // branch -> refresh the issue_queue
    input wire                                  rob_commit_exp_en,          // exp -> refresh the issue_queue

    // outputs
    // to FU
    output wire                                 issue_en_out,
    output wire [`WORD_WIDTH-1 : 0]             issue_queue_rs1_value_out,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      issue_queue_Pdst_out,
    // 
    output wire                                 issue_queue_full
);

// -----------------------------------issue queue---------------------------------------------------------
reg [$clog2(`ROB_DEPTH)-1 : 0]  issue_queue_Prs1                [0 : `ISSUE_QUEUE_DEPTH-1];
reg                             issue_queue_Prs1_valid          [0 : `ISSUE_QUEUE_DEPTH-1];
reg [$clog2(`ROB_DEPTH)-1 : 0]  issue_queue_Pdst                [0 : `ISSUE_QUEUE_DEPTH-1]; // alloc rob
wire                            issue_queue_valid               [0 : `ISSUE_QUEUE_DEPTH-1]; // not be flushed
wire                            issue_queue_ready               [0 : `ISSUE_QUEUE_DEPTH-1]; // data1 and data2 ready
// -----------------------------------payload RAM---------------------------------------------------------
reg [`WORD_WIDTH-1 : 0]         issue_queue_rs1_value           [0 : `ISSUE_QUEUE_DEPTH-1];
//--------------------------------------------------------------------------------------------------------

// issue queue(fifo): compressed structure, issue_queue[0] is the oldest insn in the queue,
// is easy to arbiter which one issue to FU
wire                                fifo_empty;
wire                                fifo_full;
wire                                issue_en;
wire [1:0]                          issue_tag;
wire                                fifo_write_en;
wire                                fifo_issue_en;
reg [`ISSUE_QUEUE_DEPTH : 0]        fifo_p;         // push:'b0_0001 -> 'b0_0010 -> 'b0_0100 -> 'b0_1000 -> 'b1_0000(full)
                                                    // pop: 'b1_0000(full) -> 'b0_1000 -> 'b0_0100 -> 'b0_0010 -> 'b0_0001(empty)
                                                    // `ISSUE_QUEUE_DEPTH = 4
assign fifo_empty       =   fifo_p[0];
assign fifo_full        =   fifo_p[`ISSUE_QUEUE_DEPTH];

assign issue_queue_full = fifo_full;

assign issue_queue_valid[0] = !fifo_p[0];
assign issue_queue_valid[1] = !fifo_p[0] && !fifo_p[1];
assign issue_queue_valid[2] = !fifo_p[0] && !fifo_p[1] && !fifo_p[2];
assign issue_queue_valid[3] = !fifo_p[0] && !fifo_p[1] && !fifo_p[2] && !fifo_p[3];


assign issue_en         =   issue_queue_ready[0] && issue_queue_valid[0];

//---------------------------In order--------------------------------------------------------------------------------
assign issue_tag        =   2'b0;   // in order issue
//-------------------------------------------------------------------------------------------------------------------
assign fifo_write_en    =   !fifo_full  && issue2queue_en;
assign fifo_issue_en    =   !fifo_empty && issue_en;
assign issue_en_out     =   fifo_issue_en;

assign issue_queue_rs1_value_out    =   issue_queue_rs1_value   [issue_tag];
assign issue_queue_Pdst_out         =   issue_queue_Pdst        [issue_tag];
                        
// ------------------------ writeback--------------------------------------------------------------------------------
wire [`WORD_WIDTH-1 : 0]    writeback_rs1_data  [0 : `ISSUE_QUEUE_DEPTH-1];
wire                        writeback_rs1_en    [0 : `ISSUE_QUEUE_DEPTH-1];
wire                        wb_alu_in_writers1;
wire                        wb_div_in_writers1;
wire                        wb_mul_in_writers1;
wire                        wb_load_in_writers1;
wire                        wb_br_in_writers1;
wire                        wb_csr_in_writers1;

assign wb_alu_in_writers1   = (wb_alu_dst_Paddr == rs1_Paddr)   && rs1_rat_valid && fifo_write_en;
assign wb_div_in_writers1   = (wb_div_dst_Paddr == rs1_Paddr)   && rs1_rat_valid && fifo_write_en;
assign wb_mul_in_writers1   = (wb_mul_dst_Paddr == rs1_Paddr)   && rs1_rat_valid && fifo_write_en;
assign wb_load_in_writers1  = (wb_load_dst_Paddr== rs1_Paddr)   && rs1_rat_valid && fifo_write_en;
assign wb_br_in_writers1    = (wb_br_rob        == rs1_Paddr)   && rs1_rat_valid && fifo_write_en;
assign wb_csr_in_writers1   = (wb_csr_dst_Paddr == rs1_Paddr)   && rs1_rat_valid && fifo_write_en;

genvar i;
    // rs1
generate
    for (i=0; i<`ISSUE_QUEUE_DEPTH; i++) begin: GENERATE_ISSUE_WRITEBACK_RS1EN
        assign    writeback_rs1_en[i] = (((issue_queue_valid[i] && (wb_alu_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_alu_in_writers1 && fifo_p[i])) && wb_alu_valid)   ||
                                        (((issue_queue_valid[i] && (wb_div_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_div_in_writers1 && fifo_p[i])) && wb_div_valid)   ||
                                        (((issue_queue_valid[i] && (wb_mul_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_mul_in_writers1 && fifo_p[i])) && wb_mul_valid)   ||
                                        (((issue_queue_valid[i] && (wb_load_dst_Paddr== issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_load_in_writers1&& fifo_p[i])) && wb_load_valid)  ||
                                        (((issue_queue_valid[i] && (wb_br_rob        == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_br_in_writers1  && fifo_p[i])) && wb_br_out_valid)||
                                        (((issue_queue_valid[i] && (wb_csr_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_csr_in_writers1 && fifo_p[i])) && wb_csr_valid);
    end
endgenerate

generate
    for (i=0; i<`ISSUE_QUEUE_DEPTH; i++) begin: GENERATE_ISSUE_WRITEBACK_RS1DATA
        assign    writeback_rs1_data[i] =   ((((wb_alu_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_alu_in_writers1 && fifo_p[i])) && wb_alu_valid    )?  wb_alu_out  :
                                            ((((wb_div_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_div_in_writers1 && fifo_p[i])) && wb_div_valid    )?  wb_div_out  :
                                            ((((wb_mul_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_mul_in_writers1 && fifo_p[i])) && wb_mul_valid    )?  wb_mul_out  :
                                            ((((wb_load_dst_Paddr== issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_load_in_writers1&& fifo_p[i])) && wb_load_valid   )?  wb_load_data:
                                            ((((wb_br_rob        == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_br_in_writers1  && fifo_p[i])) && wb_br_out_valid )?  wb_br_out   :
                                            ((((wb_csr_dst_Paddr == issue_queue_Prs1[i]) && issue_queue_Prs1_valid[i]) || (wb_csr_in_writers1 && fifo_p[i])) && wb_csr_valid    )?  wb_csr_data :   'b0;
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
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                issue_queue_Prs1        [j] <= {$clog2(`ROB_DEPTH){1'b0}};
                issue_queue_Pdst        [j] <= {$clog2(`ROB_DEPTH){1'b0}};
            end else if (fifo_write_en && fifo_p[j] && !(fifo_issue_en && (issue_tag<=j))) begin    // only write in [j]
                issue_queue_Prs1        [j] <= rs1_Paddr;
                issue_queue_Pdst        [j] <= id_alloc_rob;
            end else if (fifo_write_en && fifo_p[j+1] && fifo_issue_en && (issue_tag<=j)) begin   // write in [j+1] and issue
                issue_queue_Prs1        [j] <= rs1_Paddr;
                issue_queue_Pdst        [j] <= id_alloc_rob;
            end else if (fifo_issue_en && (issue_tag<=j)) begin                                   // only issue, compressed
                issue_queue_Prs1        [j] <= issue_queue_Prs1     [j+1];
                issue_queue_Pdst        [j] <= issue_queue_Pdst     [j+1];
            end                                                                 // not write and not issue, hold
        end
    end
endgenerate

// when issue, issue_queue[`ISSUE_QUEUE_DEPTH-1] stays the same, because it's valid = 0.
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        issue_queue_Prs1        [`ISSUE_QUEUE_DEPTH-1] <= {$clog2(`ROB_DEPTH){1'b0}};
        issue_queue_Pdst        [`ISSUE_QUEUE_DEPTH-1] <= {$clog2(`ROB_DEPTH){1'b0}};
    end else if (fifo_write_en && fifo_p[`ISSUE_QUEUE_DEPTH-1] && !fifo_issue_en) begin  // write in
        issue_queue_Prs1        [`ISSUE_QUEUE_DEPTH-1] <= rs1_Paddr;
        issue_queue_Pdst        [`ISSUE_QUEUE_DEPTH-1] <= id_alloc_rob;
    end else if (fifo_issue_en) begin
        issue_queue_Prs1        [`ISSUE_QUEUE_DEPTH-1] <= {$clog2(`ROB_DEPTH){1'b0}};
        issue_queue_Pdst        [`ISSUE_QUEUE_DEPTH-1] <= {$clog2(`ROB_DEPTH){1'b0}};
    end
end
//------------------------Prs1_valid=0 means data1 ready--------------------------------------------------------
// time: write in queue, writeback replace the writeindata, then issue to shift
// judje priority: first determine the valid conditions, wb -> write
generate
    for (j=0; j<`ISSUE_QUEUE_DEPTH-1; j++) begin: GENERATE_RATRS1VALID
        always @(posedge clk or negedge rst_n) begin
            if (!rst_n) begin
                issue_queue_Prs1_valid[j]   <= 1'b0;
            end else if (writeback_rs1_en[j] && !(fifo_issue_en && (issue_tag<=j))) begin        // writeback in [j] without issue
                issue_queue_Prs1_valid[j]   <= 1'b0;
                issue_queue_rs1_value[j]    <= writeback_rs1_data[j];
            end else if (writeback_rs1_en[j+1] && (fifo_issue_en && (issue_tag<=j))) begin  // writeback in [j+1] and issue
                issue_queue_Prs1_valid[j]   <= 1'b0;
                issue_queue_rs1_value[j]    <= writeback_rs1_data[j+1];
            end else if (fifo_write_en && fifo_p[j] && !fifo_issue_en) begin                // write in [j] without issue
                issue_queue_Prs1_valid[j]   <= rs1_rat_valid;
                issue_queue_rs1_value[j]    <= rs1_value_fromGPR;
            end else if (fifo_write_en && fifo_p[j+1] && fifo_issue_en) begin               // write in [j+1] and issue
                issue_queue_Prs1_valid[j]   <= rs1_rat_valid;
                issue_queue_rs1_value[j]    <= rs1_value_fromGPR;
            end else if (fifo_issue_en && (issue_tag<=j)) begin                             // only issue
                issue_queue_Prs1_valid[j]   <= issue_queue_Prs1_valid  [j+1];
                issue_queue_rs1_value[j]    <= issue_queue_rs1_value[j+1];
            end
        end
    end
endgenerate

// when issue, issue_queue[`ISSUE_QUEUE_DEPTH-1] stays the same, because it's valid = 0.
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        issue_queue_Prs1_valid[`ISSUE_QUEUE_DEPTH-1]   <= 1'b0;
    end else if (fifo_write_en && fifo_p[`ISSUE_QUEUE_DEPTH-1] && !fifo_issue_en) begin  // only write in [`ISSUE_QUEUE_DEPTH-1]
        issue_queue_Prs1_valid  [`ISSUE_QUEUE_DEPTH-1]  <= rs1_rat_valid;
        issue_queue_rs1_value   [`ISSUE_QUEUE_DEPTH-1]  <= rs1_value_fromGPR;
    end else if (writeback_rs1_en[`ISSUE_QUEUE_DEPTH-1]) begin                  // writeback in [`ISSUE_QUEUE_DEPTH-1]
        issue_queue_Prs1_valid  [`ISSUE_QUEUE_DEPTH-1]  <= 1'b0;
        issue_queue_rs1_value   [`ISSUE_QUEUE_DEPTH-1]  <= writeback_rs1_data[`ISSUE_QUEUE_DEPTH-1];
    end                                                                             // not write, hold
end
//------------------------------------issue_queue_ready---------------------------------------------------------
generate
    for (j=0; j<`ISSUE_QUEUE_DEPTH; j++) begin: GENERATE_READY
        assign issue_queue_ready[j] = !issue_queue_Prs1_valid[j];
    end
endgenerate

endmodule
`endif
