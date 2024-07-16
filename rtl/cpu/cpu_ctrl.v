`ifndef OOOCPU_CTRL
`define OOOCPU_CTRL
module cpu_ctrl(
    input wire                                  clk,
    input wire                                  rst_n,
    // inputs
    input wire                                  if_en,
    input wire                                  stall_in_issue,     // issue_queue full
    input wire                                  stall_in_decoder,   // jp_rs1 data not get
    input wire                                  jp_taken,           // jump
    input wire                                  rob_commit_br_taken,// branch
    // exception
    input wire [2:0]                            rob_commit_ebreak_ecall_mret,
    input wire [`PC_WIDTH-1 : 0]                rob_commit_pc,
    input wire                                  rob_commit_exp_en,
    input wire [`DATA_WIDTH_ISA_EXP-1:0]        rob_commit_exp_op,
    input wire                                  rob_empty,          // interrupt executed when rob is empty
    input wire                                  rob_full,           // rob full

    // signals from csr
    input wire                                  csr_mstatus_mie,    // machine interrupt enable
    input wire                                  csr_mie_meie,       // external interrupt enable
    input wire                                  csr_mie_mtie,       // timer interrupt enable
    input wire                                  csr_mie_msie,       // software interrupt enable
    input wire                                  csr_mip_meip,       // external interrupt pending
    input wire                                  csr_mip_mtip,       // timer interrupt pending
    input wire                                  csr_mip_msip,       // software interrupt pending
    input wire [29 : 0]                         csr_mtvec_base,     // jump pc_base
    input wire [1 : 0]                          csr_mtvec_mode,     // jump pc_mode
    input wire [`PC_WIDTH - 1 : 0]              csr_mepc_pc,        // restore pc
    // outputs
    output wire                                 allocate_en,
    output wire                                 pc_stall,
    output wire                                 if_stall,
    output wire                                 id_stall,
    output wire                                 if_flush,
    output wire                                 id_flush,
    // to csr
    output reg                                  mstatus_mie_clear_en,
    output reg                                  mstatus_mie_set_en,
    output reg                                  mepc_set_en,
    output reg [`PC_WIDTH - 1 :0]               mepc_set_pc,
    output reg                                  mcause_set_en,
    output reg [`WORD_WIDTH - 1 : 0]            mcause_set_cause,
    output reg                                  mtval_set_en,
    output reg [`WORD_WIDTH - 1 : 0]            mtval_set_tval,
    // to pc
    output wire                                 trap_happened,
    output reg [`PC_WIDTH - 1 : 0]              ctrl_pc,
    // to clint
    output reg                                  external_int_clear,
    output reg                                  software_int_clear,
    output reg                                  timer_int_clear
);
//---------------------------- trap-------------------------------------------------------------
wire        external_int_en;
wire        timer_int_en;
wire        software_int_en;
wire        interrupt_happened;
wire        exception_en;
wire        mcause_set_cause_interrupt;
wire[30:0]  mcause_set_cause_expcode;
reg [2:0]   int_handler; //int_handler = {timer_int_handler, software_int_handler, external_int_handler};
reg         interrupt_happened_r;
wire        interrupt_exe;

assign external_int_en      = csr_mstatus_mie && (csr_mie_meie && csr_mip_meip);
assign timer_int_en         = csr_mstatus_mie && (csr_mie_mtie && csr_mip_mtip);
assign software_int_en      = csr_mstatus_mie && (csr_mie_msie && csr_mip_msip);
assign interrupt_happened   = external_int_en || timer_int_en|| software_int_en;
assign exception_en         = rob_commit_exp_en || rob_commit_ebreak_ecall_mret[1] || rob_commit_ebreak_ecall_mret[2];

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        interrupt_happened_r <= 1'b0;
    end else if (interrupt_happened) begin
        interrupt_happened_r <= 1'b1;
    end else if (rob_empty) begin
        interrupt_happened_r <= 1'b0;
    end
end

assign interrupt_exe    =   (interrupt_happened || interrupt_happened_r) && rob_empty;
assign trap_happened    =   exception_en || interrupt_exe;
assign mcause_set_cause_interrupt = (interrupt_exe  )? `MCAUSE_INTERRUPT :
                                    (exception_en   )? `MCAUSE_EXCEPTION : 1'b0;

assign mcause_set_cause_expcode =   (external_int_en                                )?      `MCAUSE_MACHINE_EXTERNAL_INT        :
                                    (timer_int_en                                   )?      `MCAUSE_MACHINE_TIMER_INT           :
                                    (software_int_en                                )?      `MCAUSE_MACHINE_SOFTWARE_INT        :
                                    (rob_commit_ebreak_ecall_mret[1]                )?      `MCAUSE_ENVIRONMENT_CALL_FROM_M_MODE:
                                    (rob_commit_ebreak_ecall_mret[2]                )?      `MCAUSE_BREAKPOINT                  :
                                    (rob_commit_exp_op == `ISA_EXP_UNDEF_INSN       )?      `MCAUSE_ILLEGAL_INSTRUCTION         :
                                    (rob_commit_exp_op == `ISA_EXP_ALU_OVERFLOW     )?      `MCAUSE_ALU_OVERFLOW                :
                                    (rob_commit_exp_op == `ISA_EXP_LOAD_MISALIGNED  )?      `MCAUSE_LOAD_ADDRESS_MISALIGNED     :
                                    (rob_commit_exp_op == `ISA_EXP_STORE_MISALIGNED )?      `MCAUSE_STORE_ADDRESS_MISALIGNED    : 31'b0;
// int_clear
// which kind of int is handling
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        int_handler <= 3'b000;
    end else if (external_int_en) begin     // External interrupt handler begins
        int_handler <= 3'b001;
    end else if (software_int_en) begin     // Software interrupt handler begins
        int_handler <= 3'b010;
    end else if (timer_int_en) begin        // Timer interrupt handler begins
        int_handler <= 3'b100;
    end else if (rob_commit_ebreak_ecall_mret[0]) begin // The interrupt handler completes
        int_handler <= 3'b000;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        external_int_clear <= 1'b0;
    end else if (int_handler[0] && rob_commit_ebreak_ecall_mret[0]) begin   // The external interrupt handler completes
        external_int_clear <= 1'b1;                 // Hardware reset
    end else if (!csr_mip_meip) begin               // Software also writes registers in interrupt handlers to clear interrupts
        external_int_clear <= 1'b0;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        software_int_clear <= 1'b0;
    end else if (int_handler[1] && rob_commit_ebreak_ecall_mret[0]) begin   // The software interrupt handler completes
        software_int_clear <= 1'b1;
    end else if (!csr_mip_msip) begin
        software_int_clear <= 1'b0;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        timer_int_clear <= 1'b0;
    end else if (int_handler[2] && rob_commit_ebreak_ecall_mret[0]) begin   // The timer interrupt handler completes
        timer_int_clear <= 1'b1;
    end else if (!csr_mip_mtip) begin
        timer_int_clear <= 1'b0;
    end
end

always @(*) begin
    if (trap_happened) begin
            mstatus_mie_clear_en    = `ENABLE;
            mstatus_mie_set_en      = `DISABLE;
            mepc_set_en             = `ENABLE;
            mepc_set_pc             = (interrupt_exe)? rob_commit_pc+4 : rob_commit_pc; // if exp is ecall/ebreak, soft will change pc as pc+4
            mcause_set_en           = `ENABLE;
            mcause_set_cause        = {mcause_set_cause_interrupt, mcause_set_cause_expcode};
            mtval_set_en            = `DISABLE; // reserved
            mtval_set_tval          = 32'b0;    // reserved
            ctrl_pc                 = (csr_mtvec_mode == `MTVEC_MODE_DIRECT)?   {csr_mtvec_base, 2'b00} :
                                                                                {csr_mtvec_base, 2'b00} + (mcause_set_cause_expcode << 2);
    end else if (rob_commit_ebreak_ecall_mret[0]) begin
            mstatus_mie_clear_en    = `DISABLE;
            mstatus_mie_set_en      = `ENABLE;
            mepc_set_en             = `DISABLE;
            mepc_set_pc             = 32'b0;
            mcause_set_en           = `DISABLE;
            mcause_set_cause        = 32'b0;
            mtval_set_en            = `DISABLE;
            mtval_set_tval          = 32'b0;
            ctrl_pc                 = csr_mepc_pc;
    end else begin
            mstatus_mie_clear_en    = `DISABLE;
            mstatus_mie_set_en      = `DISABLE;
            mepc_set_en             = `DISABLE;
            mepc_set_pc             = 32'b0;
            mcause_set_en           = `DISABLE;
            mcause_set_cause        = 32'b0;
            mtval_set_en            = `DISABLE;
            mtval_set_tval          = 32'b0;
            ctrl_pc                 = 32'b0;
    end
end
// ---------------------------------------------------------------------------------------------
assign allocate_en = if_en && !if_stall;

//------------------------- stall and flush----------------------------------------------------
assign pc_stall = rob_full || stall_in_issue || stall_in_decoder || (interrupt_happened_r && !rob_empty);
assign if_stall = rob_full || stall_in_issue || stall_in_decoder || interrupt_happened_r;
assign id_stall = rob_full || stall_in_issue || interrupt_happened_r;

assign if_flush = jp_taken || rob_commit_br_taken || interrupt_happened;
assign id_flush = rob_commit_br_taken || interrupt_happened;


endmodule
`endif
