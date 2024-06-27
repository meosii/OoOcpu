`ifndef SIICPU_MEM_CTRL
`define SIICPU_MEM_CTRL
module mem_ctrl (
    input wire                                  clk,
    input wire                                  rst_n,
    // input from issue queue
    input wire                                  mem_issue_en,
    input wire [`DATA_WIDTH_MEM_OP-1 : 0]       mem_issue_queue_op,
    input wire [`WORD_WIDTH-1 : 0]              mem_issue_queue_imm,
    input wire [`WORD_WIDTH-1 : 0]              mem_issue_queue_rs1_value,
    input wire [`WORD_WIDTH-1 : 0]              mem_issue_queue_rs2_value, // store data
    input wire [$clog2(`ROB_DEPTH)-1 : 0]       mem_issue_queue_Pdst,
    // input from AHB
    input wire [`WORD_WIDTH - 1 : 0]            CPU_HRDATA,
    input wire                                  CPU_HREADY,
    input wire [1 : 0]                          CPU_HRESP,
    // from spm
    input wire [`WORD_WIDTH - 1 : 0]            spm_rd_data,

    // outputs
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
    // mem writeback
    output wire [`WORD_WIDTH - 1 : 0]           load_data,
    output wire                                 load_data_valid,
    output wire                                 mem_ready,
    output wire [$clog2(`ROB_DEPTH)-1 : 0]      mem_dst_Paddr,
    output wire                                 store_finish,
    // exp_code
    output wire [`DATA_WIDTH_ISA_EXP - 1 : 0]   exp_code_in_mem_ctrl
);

// to ahb_mem_ctrl
wire                                 memory_wr_en;
wire                                 memory_rd_en;
wire [`WORD_WIDTH - 1 : 0]           memory_addr;
wire [`WORD_WIDTH - 1 : 0]           store_data;
wire [3 : 0]                         store_byteena;
// from ahb_mem_ctrl
wire [`WORD_WIDTH - 1 : 0]          mem_load_data;       // mem_load_data -> load_data
wire                                bus_ahb_enable;     // ahb bus not get the valid data
wire                                bus_spm_enable;     // Select SPM for address bus
wire                                trans_end_en;
wire [`DATA_WIDTH_ISA_EXP - 1 : 0]  ahb_exp_code;


assign memory_wr_en = mem_issue_en && ( (mem_issue_queue_op == `MEM_OP_SW)  ||
                                        (mem_issue_queue_op == `MEM_OP_SH)  ||
                                        (mem_issue_queue_op == `MEM_OP_SB)  );

assign memory_rd_en = mem_issue_en && ( (mem_issue_queue_op == `MEM_OP_LOAD_LW  )   ||
                                        (mem_issue_queue_op == `MEM_OP_LOAD_LH  )   ||
                                        (mem_issue_queue_op == `MEM_OP_LOAD_LHU )   ||
                                        (mem_issue_queue_op == `MEM_OP_LOAD_LB  )   ||
                                        (mem_issue_queue_op == `MEM_OP_LOAD_LBU )   );

assign memory_addr  =   mem_issue_queue_imm + mem_issue_queue_rs1_value;

assign store_byteena=   (mem_issue_queue_op == `MEM_OP_SW)? 4'b1111 :
                        (mem_issue_queue_op == `MEM_OP_SH)? 4'b0011 :
                        (mem_issue_queue_op == `MEM_OP_SB)? 4'b0001 : 4'b0000;

assign store_data   =   (mem_issue_queue_op == `MEM_OP_SW)?         mem_issue_queue_rs2_value               :
                        (mem_issue_queue_op == `MEM_OP_SH)? {16'b0, mem_issue_queue_rs2_value[15 : 0]   }   :
                        (mem_issue_queue_op == `MEM_OP_SB)? {24'b0, mem_issue_queue_rs2_value[7 : 0]    }   : `WORD_WIDTH'b0;

// -------------------------------- exception: memory addr miss align -------------------------------------------------------
wire [`DATA_WIDTH_OFFSET - 1 : 0]   offset;
wire                                miss_align_in_spm;

assign offset                   =   memory_addr[`BYTE_OFFSET_LOC];
assign miss_align_in_spm        =   (memory_addr[`SPM_ADDR_HIGH_LOCA] == `SPM_ADDR_HIGH) && (offset != `BYTE_OFFSET_WORD);

assign exp_code_in_mem_ctrl     =   (miss_align_in_spm && memory_rd_en)? `ISA_EXP_LOAD_MISALIGNED     :
                                    (miss_align_in_spm && memory_wr_en)? `ISA_EXP_STORE_MISALIGNED    : `ISA_EXP_NO_EXP;

// ------------------------------------- writeback to rob -----------------------------------------------------------------------------
// one ahb trans finished, or spm finished
// registers
reg                                 mem_issue_en_r1; // spm ready
reg                                 bus_spm_enable_r1;
reg [`DATA_WIDTH_MEM_OP - 1 : 0]    mem_issue_queue_op_r;
reg [$clog2(`ROB_DEPTH)-1 : 0]      mem_issue_queue_Pdst_r;
reg                                 mem_rd_en_r;
reg                                 mem_wr_en_r;

wire [`DATA_WIDTH_MEM_OP - 1 : 0]   mem_op;     // when trans ready
wire                                trans_finish;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        mem_issue_en_r1 <= 1'b1;
        bus_spm_enable_r1 <= 1'b0;
    end else begin
        mem_issue_en_r1 <= mem_issue_en;
        bus_spm_enable_r1 <= bus_spm_enable;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        mem_issue_queue_op_r   <= `DATA_WIDTH_MEM_OP'b0;
        mem_issue_queue_Pdst_r <= 'b0;
        mem_rd_en_r            <= 1'b0;
        mem_wr_en_r            <= 1'b0;
    end else if (mem_issue_en) begin
        mem_issue_queue_op_r   <= mem_issue_queue_op;
        mem_issue_queue_Pdst_r <= mem_issue_queue_Pdst;
        mem_rd_en_r            <= memory_rd_en;
        mem_wr_en_r            <= memory_wr_en;
    end
end

assign trans_finish = trans_end_en || (mem_issue_en_r1 && bus_spm_enable_r1);
assign mem_op       = (trans_finish)? mem_issue_queue_op_r   :   `DATA_WIDTH_MEM_OP'b0;
assign mem_dst_Paddr= (trans_finish)? mem_issue_queue_Pdst_r :   'b0;
assign load_data =  (mem_op == `MEM_OP_LOAD_LW  )?  mem_load_data                                                                   :
                    (mem_op == `MEM_OP_LOAD_LH  )?  {{16{mem_load_data[`WORD_WIDTH/2 - 1]}}, mem_load_data[`WORD_WIDTH/2 - 1 : 0]}  :
                    (mem_op == `MEM_OP_LOAD_LHU )?  {16'b0, mem_load_data[`WORD_WIDTH/2 - 1 : 0]}                                   :
                    (mem_op == `MEM_OP_LOAD_LB  )?  {{24{mem_load_data[`WORD_WIDTH/4 - 1]}}, mem_load_data[`WORD_WIDTH/4 - 1 : 0]}  :
                    (mem_op == `MEM_OP_LOAD_LBU )?  {24'b0, mem_load_data[`WORD_WIDTH/4 - 1 : 0]}                                   : `WORD_WIDTH'b0;
assign load_data_valid  = trans_finish && mem_rd_en_r;
assign store_finish     = trans_finish && mem_wr_en_r;
// ------------------------------------- mem_ready: could issue ? ----------------------------------------------------------------------------
reg mem_ready_tmp;
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        mem_ready_tmp <= 1'b1;
    end else if (mem_issue_en && bus_ahb_enable && !trans_end_en) begin
        mem_ready_tmp <= 1'b0;
    end else if (trans_end_en) begin
        mem_ready_tmp <= 1'b1;
    end
end

assign mem_ready = mem_ready_tmp || trans_end_en; // function unit is ready
//---------------------------------------spm or AHB ?----------------------------------------------------------------------------

ahb_mem_ctrl u_ahb_mem_ctrl(
    .clk                (clk                ),
    .rst_n              (rst_n              ),
    .memory_wr_en       (memory_wr_en       ),
    .memory_rd_en       (memory_rd_en       ),
    .memory_addr        (memory_addr        ),
    .store_data         (store_data         ),
    .store_byteena      (store_byteena      ),
    .CPU_HRDATA         (CPU_HRDATA         ),
    .CPU_HREADY         (CPU_HREADY         ),
    .CPU_HRESP          (CPU_HRESP          ),
    .spm_rd_data        (spm_rd_data        ),
    // outputs
    .CPU_HADDR          (CPU_HADDR          ),
    .CPU_HWRITE         (CPU_HWRITE         ),
    .CPU_HSIZE          (CPU_HSIZE          ),
    .CPU_HBURST         (CPU_HBURST         ),
    .CPU_HTRANS         (CPU_HTRANS         ),
    .CPU_HMASTLOCK      (CPU_HMASTLOCK      ),
    .CPU_HWDATA         (CPU_HWDATA         ),
    .spm_store_byteena  (spm_store_byteena  ),
    .spm_write_data     (spm_write_data     ),
    .spm_rdaddress      (spm_rdaddress      ),
    .spm_rden           (spm_rden           ),
    .spm_wraddress      (spm_wraddress      ),
    .spm_wren           (spm_wren           ),
    .load_rd_data       (mem_load_data      ),
    .bus_spm_enable     (bus_spm_enable     ),
    .bus_ahb_enable     (bus_ahb_enable     ),
    .trans_end_en       (trans_end_en       ),
    .ahb_exp_code       (ahb_exp_code       )
);


endmodule

`endif 
