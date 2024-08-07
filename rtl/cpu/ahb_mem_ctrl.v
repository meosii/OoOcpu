`ifndef SIICPU_AHB_MEM_CTRL
`define SIICPU_AHB_MEM_CTRL
module ahb_mem_ctrl(
    input wire                                  clk,
    input wire                                  rst_n,
    // from mem_ctrl
    input wire                                  memory_wr_en,
    input wire                                  memory_rd_en,
    input wire [`WORD_WIDTH - 1 : 0]            memory_addr,
    input wire [`WORD_WIDTH - 1 : 0]            store_data,
    input wire [3 : 0]                          store_byteena,
    // from AHB
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
    // to mem_ctrl
    output wire [`WORD_WIDTH - 1 : 0]           load_rd_data,   // data from spm or bus
    output wire                                 bus_spm_enable,     // Select SPM for address bus
    output wire                                 bus_ahb_enable, // SPM not be selected
    output reg                                  trans_end_en,
    // exp
    output wire [`DATA_WIDTH_ISA_EXP - 1 : 0]   ahb_exp_code
);

wire    ahb_exp_en;         // slave error trans

wire    load_store_enable;
wire    bus_trans_ready;
wire    bus_trans_wait;
wire    bus_trans_error_stage1;
wire    bus_trans_error_stage2;

reg                         bus_spm_enable_r1;
reg [`WORD_WIDTH - 1 : 0]   spm_rd_data_r1;

wire    ahb_bus_wait;   // ahb bus not get the valid data

assign  load_store_enable   = (memory_rd_en || memory_wr_en);
assign  bus_spm_enable      = (memory_addr[`SPM_ADDR_HIGH_LOCA] == `SPM_ADDR_HIGH) && load_store_enable;
assign  bus_ahb_enable      = !bus_spm_enable && load_store_enable;

assign  bus_trans_ready         =  CPU_HREADY  && (CPU_HRESP == `HRESP_OKAY);
assign  bus_trans_wait          =  !CPU_HREADY && (CPU_HRESP == `HRESP_OKAY);
assign  bus_trans_error_stage1  =  !CPU_HREADY && (CPU_HRESP == `HRESP_ERROR);
assign  bus_trans_error_stage2  =  CPU_HREADY  && (CPU_HRESP == `HRESP_ERROR);

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        bus_spm_enable_r1   <= 1'b0;
        spm_rd_data_r1      <= `WORD_WIDTH'b0;
    end else begin
        bus_spm_enable_r1   <= bus_spm_enable;
        spm_rd_data_r1      <= spm_rd_data;
    end
end

// ahb
// transfer state
localparam NON_TRANS    = 1'b0;
localparam TRANSING     = 1'b1;

reg c_trans_state;
reg n_trans_state;
reg trans_start_en;
reg trans_write;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        trans_write <= 1'b0;
    end else if (trans_start_en) begin
        if (memory_wr_en) begin
            trans_write <= 1'b1; // ahb trans write
        end else if (memory_rd_en) begin
            trans_write <= 1'b0; // ahb trans read
        end
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        c_trans_state <= NON_TRANS;
    end else begin
        c_trans_state <= n_trans_state;
    end
end

always @* begin
    case (c_trans_state)
    NON_TRANS: begin
        if (bus_ahb_enable) begin
            n_trans_state = TRANSING;
            trans_start_en = 1'b1;
        end else begin
            n_trans_state = NON_TRANS;
            trans_start_en = 1'b0;
        end
        trans_end_en = 1'b0;
	 end
    TRANSING:
        if (bus_trans_ready && !bus_ahb_enable) begin                   // The current transfer ends,
            n_trans_state = NON_TRANS;                                  // And there is no next transfer after this trans.
            trans_start_en = 1'b0;
            trans_end_en = 1'b1;
        end else if (bus_trans_ready && bus_ahb_enable) begin           // The current transfer ends,
            n_trans_state = TRANSING;                                   // And this transfer is immediately followed by the next transfer.
            trans_start_en = 1'b1;
            trans_end_en = 1'b1;
        end else if (bus_trans_error_stage1 && !bus_ahb_enable) begin   // The current transfer errors,
            n_trans_state = NON_TRANS;                                  // And there is no next transfer after this trans.
            trans_start_en = 1'b0;
            trans_end_en = 1'b1;
        end else if (bus_trans_error_stage1 && bus_ahb_enable) begin   // The current transfer errors,
            n_trans_state = TRANSING;                                  // And this transfer is immediately followed by the next transfer.
            trans_start_en = 1'b1;
            trans_end_en = 1'b1;
        end else begin
            n_trans_state = TRANSING;                                   // The current transfer is not complete
            trans_start_en = 1'b0;
            trans_end_en = 1'b0;
        end
    default: begin
        n_trans_state = NON_TRANS;
        trans_start_en = 1'b0;
        trans_end_en = 1'b0;
    end
    endcase
end

// Address phase
assign CPU_HTRANS       =   (trans_start_en                             )?   `HTRANS_NONSEQ : `HTRANS_IDLE;
assign CPU_HADDR        =   (trans_start_en                             )?   memory_addr    : `WORD_WIDTH'b0;
assign CPU_HBURST       =   (trans_start_en                             )?   `HBRUST_SINGLE : 3'b000;
assign CPU_HMASTLOCK    =   (trans_start_en                             )?   1'b1           : 1'b0;
assign CPU_HWRITE       =   (memory_wr_en                               )?   `HWRITE_WRITE  : `HWRITE_READ;
assign CPU_HSIZE        =   (memory_wr_en && store_byteena == 4'b1111   )?   `HSIZE_32      :
                            (memory_wr_en && store_byteena == 4'b0011   )?   `HSIZE_16      :
                            (memory_wr_en && store_byteena == 4'b0001   )?   `HSIZE_8       : `HSIZE_32;
						
// ahb_reg

assign ahb_bus_wait = (c_trans_state == TRANSING) && bus_trans_wait;

reg [`WORD_WIDTH - 1 : 0]   ahb_wdata;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        ahb_wdata <= `WORD_WIDTH'b0;
    end else if (!ahb_bus_wait) begin
        ahb_wdata <= store_data;
    end
end

// Data phase
assign CPU_HWDATA = (c_trans_state == TRANSING)? ahb_wdata : `WORD_WIDTH'b0;

// to spm
assign spm_rden             = (bus_spm_enable)? memory_rd_en : 1'b0;
assign spm_wren             = (bus_spm_enable)? memory_wr_en : 1'b0;
assign spm_store_byteena    = store_byteena;
assign spm_write_data       = store_data;
assign spm_rdaddress        = memory_addr;
assign spm_wraddress        = memory_addr;

// to mem_ctrl
assign load_rd_data =   (bus_spm_enable_r1  )?  spm_rd_data_r1 : 
                        (trans_end_en       )?  CPU_HRDATA  : `WORD_WIDTH'b0;
// exception
assign ahb_exp_en   =   ((c_trans_state == TRANSING) && (bus_trans_error_stage1))? 1'b1 : 1'b0;
assign ahb_exp_code =   (ahb_exp_en && trans_write  )?  `ISA_EXP_AHB_ERROR_STORE    :
                        (ahb_exp_en && !trans_write )?  `ISA_EXP_AHB_ERROR_LOAD     : `ISA_EXP_NO_EXP;

endmodule
`endif
