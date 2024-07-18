module tb_ooocpu();
    reg                         clk;
    reg                         rst_n;
    reg                         cpu_en;
    // int req
    reg                         irq_external;
    reg                         irq_timer;
    reg                         irq_software;
    // insn and pc
    reg [`WORD_WIDTH - 1 : 0]   insn;
    // from AHB
    reg [`WORD_WIDTH - 1 : 0]   CPU_HRDATA;
    reg                         CPU_HREADY;
    reg [1 : 0]                 CPU_HRESP;
    // from spm
    reg [`WORD_WIDTH - 1 : 0]   spm_rd_data;
    // outputs
    wire                        rd_insn_en;
    wire [`PC_WIDTH - 1 : 0]    pc;
    // to AHB
    wire [`WORD_WIDTH - 1 : 0]  CPU_HADDR;
    wire                        CPU_HWRITE;
    wire [2 : 0]                CPU_HSIZE;
    wire [2 : 0]                CPU_HBURST;
    wire [1 : 0]                CPU_HTRANS;
    wire                        CPU_HMASTLOCK;
    wire [`WORD_WIDTH - 1 : 0]  CPU_HWDATA;
    // to spm
    wire [3 : 0]                spm_store_byteena;
    wire [`WORD_WIDTH - 1 : 0]  spm_write_data;
    wire [`WORD_WIDTH - 1 : 0]  spm_rdaddress;
    wire                        spm_rden;
    wire [`WORD_WIDTH - 1 : 0]  spm_wraddress;
    wire                        spm_wren;
    // int clear
    wire                        external_int_clear;
    wire                        software_int_clear;
    wire                        timer_int_clear;

// insn memory
reg [31:0] insn_pc_r1;
reg [31:0] insn_memory [0:1023];

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        insn_pc_r1 <= 'b0;
    end else if (rd_insn_en) begin
        insn_pc_r1 <= insn_memory[pc[31:2]];
    end
end

ooocpu u_ooocpu(
    .clk                (clk                ),
    .rst_n              (rst_n              ),
    .cpu_en             (cpu_en             ),
    .irq_external       (irq_external       ),
    .irq_timer          (irq_timer          ),
    .irq_software       (irq_software       ),
    .insn               (insn_pc_r1         ),
    .rd_insn_en         (rd_insn_en         ),
    .pc                 (pc                 ),
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
    .external_int_clear (external_int_clear ),
    .software_int_clear (software_int_clear ),
    .timer_int_clear    (timer_int_clear    )
);

spm u_spm(
    .clk                (clk                ),
    .rst_n              (rst_n              ),
    .spm_rdaddress      (spm_rdaddress[9:0] ),
    .spm_wraddress      (spm_wraddress[9:0] ),
    .spm_rden           (spm_rden           ),
    .spm_wren           (spm_wren           ),
    .spm_write_data     (spm_write_data     ),
    .spm_store_byteena  (spm_store_byteena  ),
    .spm_rd_data        (spm_rd_data        )
);

always #5 clk = ~clk;

initial begin
    #0 begin
        clk = 0;
        rst_n = 0;
        irq_software = 0;
        irq_timer = 0;
        irq_external = 0;
        cpu_en = 1;
    end
    #3 begin
        rst_n = 1;
    end
    #1000 $finish;
end


initial begin
    $readmemh("../os/insn.h",insn_memory);
    $fsdbDumpfile("wave_ooocpu.fsdb");
    $fsdbDumpvars();
    $fsdbDumpMDA();
end

endmodule
