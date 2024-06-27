`ifndef OOOCPU_DECODER
`define OOOCPU_DECODER
module decoder (
    //from if
    input wire                                  if_en,
    input wire [`PC_WIDTH-1 : 0]                if_pc,
    input wire [`WORD_WIDTH - 1 : 0]            if_insn,
    // jp: rs1
    input wire                                  jp_rs1_rat_valid,   // from rat
    input wire [`WORD_WIDTH - 1 : 0]            jp_rs1_data_fromGPR,// from gpr
    //to rob
    output reg [`GPR_ADDR_WIDTH - 1 : 0]        rs1_addr,
    output reg [`GPR_ADDR_WIDTH - 1 : 0]        rs2_addr,
    output reg [`GPR_ADDR_WIDTH - 1 : 0]        dst_addr,
    output reg                                  dst_wen,
    // to issue queue
    output wire [`DATA_WIDTH_ALU_OP - 1 : 0]    alu_op,
    output wire [`DATA_WIDTH_MEM_OP - 1 : 0]    mem_op,
    output wire [`DATA_WIDTH_BR_OP - 1 : 0]     br_op,
    output reg [`WORD_WIDTH-1 : 0]              imm,
    // jump
    output wire                                 jp_taken,
    output wire [`PC_WIDTH-1 : 0]               jp_addr,
    output wire [`GPR_ADDR_WIDTH-1 : 0]         jp_rs1_addr,    // to rat, judge if the data can be got from gpr
    output wire                                 stall_in_decoder,   // wait get the rs1 value
    // to cpu ctrl
    output wire                                 ebreak_en,
    output wire                                 ecall_en,
    output wire                                 mret_en,
 
    // exception
    output wire [`DATA_WIDTH_ISA_EXP - 1 : 0]   exp_code


//    //csr to gpr
//    output reg [`WORD_WIDTH - 1 : 0]            csr_to_gpr_data,
//    //to csr
//    output reg                                  csr_rd_en,
//    output reg [`CSR_ADDR_WIDTH - 1 :0]         csr_rd_addr,
//    output reg                                  csr_w_en,
//    output reg [`CSR_ADDR_WIDTH - 1 :0]         csr_w_addr,
//    output reg [`WORD_WIDTH - 1 : 0]            csr_w_data,
//    //from csr
//    input wire [`WORD_WIDTH - 1 : 0]            csr_rd_data,
);

localparam SIGN_PC_WIDTH = `PC_WIDTH + 1;

wire    [`ALL_TYPE_OPCODE]          opcode;
//wire        [`WORD_WIDTH - 1 : 0]       rs1_data;
//wire        [`WORD_WIDTH - 1 : 0]       rs2_data;

assign opcode = if_insn[`ALL_TYPE_OPCODE];

always @(*) begin
    case(opcode)
        `OP_IMM,`OP_JALR,`OP_LOAD: begin
            dst_wen     = (if_en) ? 1'b1 : 1'b0;
            rs1_addr    = if_insn[`I_TYPE_RS1];
            rs2_addr    = `GPR_ADDR_WIDTH'b0;
            dst_addr    = if_insn[`I_TYPE_RD];
            imm         = {{20{if_insn[`INSN_MSB]}}, if_insn[`I_TYPE_IMM]};
        end
        `OP_LUI,`OP_AUIPC: begin
            dst_wen     = (if_en) ? 1'b1 : 1'b0;
            rs1_addr    = `GPR_ADDR_WIDTH'b0;
            rs2_addr    = `GPR_ADDR_WIDTH'b0;
            dst_addr    = if_insn[`U_TYPE_RD];
            imm         = {if_insn[`U_TYPE_IMM], 12'b0};
        end
        `OP: begin // mul, div
            dst_wen     = (if_en) ? 1'b1 : 1'b0;
            rs1_addr    = if_insn[`R_TYPE_RS1];
            rs2_addr    = if_insn[`R_TYPE_RS2];
            dst_addr    = if_insn[`R_TYPE_RD];
            imm         = `WORD_WIDTH'b0;
        end
        `OP_JAL: begin
            dst_wen     = (if_en) ? 1'b1 : 1'b0;
            rs1_addr    = `GPR_ADDR_WIDTH'b0;
            rs2_addr    = `GPR_ADDR_WIDTH'b0;
            dst_addr    = if_insn[`J_TYPE_RD];
            imm         = {{12{if_insn[`J_TYPE_IMM_20]}}, if_insn[`J_TYPE_IMM_19_12], if_insn[`J_TYPE_IMM_11], if_insn[`J_TYPE_IMM_10_1], 1'b0};
        end
        `OP_BRANCH: begin
            dst_wen     = 1'b0;
            rs1_addr    = if_insn[`B_TYPE_RS1];
            rs2_addr    = if_insn[`B_TYPE_RS2];
            dst_addr    = `GPR_ADDR_WIDTH'b0;
            imm         = {{20{if_insn[`B_TYPE_IMM_12]}}, if_insn[`B_TYPE_IMM_11], if_insn[`B_TYPE_IMM_10_5], if_insn[`B_TYPE_IMM_4_1], 1'b0 };
        end
        `OP_STORE: begin
            dst_wen     = 1'b0;
            rs1_addr    = if_insn[`S_TYPE_RS1];
            rs2_addr    = if_insn[`S_TYPE_RS2];
            dst_addr    = `GPR_ADDR_WIDTH'b0;
            imm         = {{20{if_insn[`S_TYPE_IMM_11]}}, if_insn[`S_TYPE_IMM_11_5], if_insn[`S_TYPE_IMM_4_0]};
        end
        `OP_SYSTEM: begin
            dst_wen     = ((if_insn[`I_TYPE_FUNCT3] != 3'b000) && if_en) ? 1'b1 : 1'b0;
            rs1_addr    = (if_insn[`I_TYPE_FUNCT3] != 3'b000)? if_insn[`I_TYPE_RS1] : `GPR_ADDR_WIDTH'b0;
            rs2_addr    = `GPR_ADDR_WIDTH'b0;
            dst_addr    = (if_insn[`I_TYPE_FUNCT3] != 3'b000)? if_insn[`I_TYPE_RD] : `GPR_ADDR_WIDTH'b0;
            imm         = (if_insn[`I_TYPE_FUNCT3] != 3'b000)? {{20{if_insn[`INSN_MSB]}}, if_insn[`I_TYPE_IMM]} : `WORD_WIDTH'b0;
        end
        default: begin
            dst_wen     = 1'b0;
            rs1_addr    = `GPR_ADDR_WIDTH'b0;
            rs2_addr    = `GPR_ADDR_WIDTH'b0;
            dst_addr    = `GPR_ADDR_WIDTH'b0;
            imm         = `WORD_WIDTH'b0;
        end
    endcase
end

//always @(*) begin
//    if (opcode == `OP_SYSTEM) begin
//        case (if_insn[`I_TYPE_FUNCT3])
//            `FUNCT3_CSRRW: begin //t = CSRs[csr]; CSRs[csr] = x[rs1]; x[rd] = t
//                csr_rd_en       = (if_en)? `ENABLE : `DISABLE;
//                csr_rd_addr     = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_en        = (if_en && rs1_data_valid)? `ENABLE : `DISABLE;
//                csr_w_addr      = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_data      = rs1_data;
//                csr_to_gpr_data = csr_rd_data;
//            end
//            `FUNCT3_CSRRS: begin //t = CSRs[csr]; CSRs[csr] = t | x[rs1]; x[rd] = t
//                csr_rd_en       = (if_en)? `ENABLE : `DISABLE;
//                csr_rd_addr     = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_en        = (if_en && rs1_data_valid)? `ENABLE : `DISABLE;
//                csr_w_addr      = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_data      = csr_rd_data | rs1_data;
//                csr_to_gpr_data = csr_rd_data;
//            end
//            `FUNCT3_CSRRC: begin //t = CSRs[csr]; CSRs[csr] = t &~x[rs1]; x[rd] = t
//                csr_rd_en       = (if_en)? `ENABLE : `DISABLE;
//                csr_rd_addr     = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_en        = (if_en && rs1_data_valid)? `ENABLE : `DISABLE;
//                csr_w_addr      = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_data      = csr_rd_data & ~rs1_data;
//                csr_to_gpr_data = csr_rd_data;
//            end
//            `FUNCT3_CSRRWI: begin //x[rd] = CSRs[csr]; CSRs[csr] = zimm
//                csr_rd_en       = (if_en)? `ENABLE : `DISABLE;
//                csr_rd_addr     = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_en        = (if_en)? `ENABLE : `DISABLE;
//                csr_w_addr      = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_data      = {27'b0,if_insn[`I_TYPE_RS1]};
//                csr_to_gpr_data = csr_rd_data;
//            end
//            `FUNCT3_CSRRSI: begin //t = CSRs[csr]; CSRs[csr] = t | zimm; x[rd] = t
//                csr_rd_en       = (if_en)? `ENABLE : `DISABLE;
//                csr_rd_addr     = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_en        = (if_en)? `ENABLE : `DISABLE;
//                csr_w_addr      = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_data      = csr_rd_data | {27'b0,if_insn[`I_TYPE_RS1]};
//                csr_to_gpr_data = csr_rd_data;
//            end
//            `FUNCT3_CSRRCI: begin //t = CSRs[csr]; CSRs[csr] = t &~zimm; x[rd] = t
//                csr_rd_en       = (if_en)? `ENABLE : `DISABLE;
//                csr_rd_addr     = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_en        = (if_en)? `ENABLE : `DISABLE;
//                csr_w_addr      = imm[`CSR_ADDR_WIDTH - 1 : 0];
//                csr_w_data      = csr_rd_data & ~{27'b0,if_insn[`I_TYPE_RS1]};
//                csr_to_gpr_data = csr_rd_data;
//            end
//            `FUNCT3_ECALL_EBREAK: begin // deal in csr
//                csr_rd_en       = `DISABLE;
//                csr_rd_addr     = `CSR_ADDR_WIDTH'b0;
//                csr_w_en        = `DISABLE;
//                csr_w_addr      = `CSR_ADDR_WIDTH'b0;
//                csr_w_data      = `WORD_WIDTH'b0;
//                csr_to_gpr_data = `WORD_WIDTH'b0; // rd_addr = 5'b00000;
//            end
//            default: begin
//                csr_rd_en       = `DISABLE;
//                csr_rd_addr     = `CSR_ADDR_WIDTH'b0;
//                csr_w_en        = `DISABLE;
//                csr_w_addr      = `CSR_ADDR_WIDTH'b0;
//                csr_w_data      = `WORD_WIDTH'b0;
//                csr_to_gpr_data = `WORD_WIDTH'b0;
//            end
//        endcase
//    end else begin
//        csr_rd_en       = `DISABLE;
//        csr_rd_addr     = `CSR_ADDR_WIDTH'b0;
//        csr_w_en        = `DISABLE;
//        csr_w_addr      = `CSR_ADDR_WIDTH'b0;
//        csr_w_data      = `WORD_WIDTH'b0;
//        csr_to_gpr_data = `WORD_WIDTH'b0;
//    end
//end



assign alu_op = (!if_en)? `ALU_OP_NOP :
                ((opcode == `OP_IMM) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_ADDI)                                                   )? `ALU_OP_ADDI     :
                ((opcode == `OP_IMM) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_SLTI)                                                   )? `ALU_OP_SLTI     :
                ((opcode == `OP_IMM) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_SLTIU)                                                  )? `ALU_OP_SLTIU    :
                ((opcode == `OP_IMM) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_ANDI)                                                   )? `ALU_OP_ANDI     :
                ((opcode == `OP_IMM) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_ORI)                                                    )? `ALU_OP_ORI      :
                ((opcode == `OP_IMM) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_XORI)                                                   )? `ALU_OP_XORI     :
                ((opcode == `OP_IMM) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_SLLI)                                                   )? `ALU_OP_SLLI     :
                ((opcode == `OP_IMM) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_SRLI_SRAI) && if_insn[`I_TYPE_IMM_11_5] == 7'b0000000   )? `ALU_OP_SRLI     :
                ((opcode == `OP_IMM) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_SRLI_SRAI) && if_insn[`I_TYPE_IMM_11_5] == 7'b0100000   )? `ALU_OP_SRAI     :
                ((opcode == `OP_LUI)                                                                                                )? `ALU_OP_LUI      :
                ((opcode == `OP_AUIPC)                                                                                              )? `ALU_OP_AUIPC    :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0000000) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_ADD)             )? `ALU_OP_ADD      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0000000) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SLT)             )? `ALU_OP_SLT      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0000000) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SLTU)            )? `ALU_OP_SLTU     :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0000000) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_AND)             )? `ALU_OP_AND      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0000000) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_OR)              )? `ALU_OP_OR       :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0000000) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_XOR)             )? `ALU_OP_XOR      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0000000) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SLL)             )? `ALU_OP_SLL      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0000000) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SRL)             )? `ALU_OP_SRL      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0100000) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SUB)             )? `ALU_OP_SUB      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0100000) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SRA)             )? `ALU_OP_SRA      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == `FUNCT7_MULDIV) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_MUL)         )? `ALU_OP_MUL      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == `FUNCT7_MULDIV) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_MULH)        )? `ALU_OP_MULH     :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == `FUNCT7_MULDIV) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_MULHSU)      )? `ALU_OP_MULHSU   :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == `FUNCT7_MULDIV) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_MULHU)       )? `ALU_OP_MULHU    :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == `FUNCT7_MULDIV) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_DIV)         )? `ALU_OP_DIV      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == `FUNCT7_MULDIV) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_DIVU)        )? `ALU_OP_DIVU     :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == `FUNCT7_MULDIV) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_REM)         )? `ALU_OP_REM      :
                ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == `FUNCT7_MULDIV) && (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_REMU)        )? `ALU_OP_REMU     :   `ALU_OP_NOP;

// ebreak ecall
assign ebreak_en = ((if_en) && (if_insn == `EBREAK_INSN))?  `ENABLE : `DISABLE;
assign ecall_en  = ((if_en) && (if_insn == `ECALL_INSN) )?  `ENABLE : `DISABLE;
// mret, jump return
assign mret_en   = ((if_en) && (if_insn == `MRET_INSN))?  `ENABLE : `DISABLE;

assign exp_code = (!if_en)? `ISA_EXP_NO_EXP :
                (((opcode == `OP_IMM) && (  (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_ADDI    ) ||
                                            (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_SLTI    ) ||
                                            (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_SLTIU   ) ||
                                            (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_ANDI    ) ||
                                            (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_ORI     ) ||
                                            (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_XORI    ) ||
                                            (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_SLLI    ) ||
                                            ((if_insn[`I_TYPE_FUNCT3] == `FUNCT3_SRLI_SRAI) && ((if_insn[`I_TYPE_IMM_11_5] == 7'b0000000) || (if_insn[`I_TYPE_IMM_11_5] == 7'b0100000)))))
                ||  ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0000000) && ((if_insn[`R_TYPE_FUNCT3] == `FUNCT3_ADD  ) ||
                                                                                   (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SLT  ) ||
                                                                                   (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SLTU ) ||
                                                                                   (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_AND  ) ||
                                                                                   (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_OR   ) ||
                                                                                   (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_XOR  ) ||
                                                                                   (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SLL  ) ||
                                                                                   (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SRL  ))      )
                ||  ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == 7'b0100000) && ((if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SUB) ||
                                                                                    (if_insn[`R_TYPE_FUNCT3] == `FUNCT3_SRA))       )
                ||  ((opcode == `OP) && (if_insn[`R_TYPE_FUNCT7] == `FUNCT7_MULDIV))
                ||  (opcode == `OP_LUI      )
                ||  (opcode == `OP_AUIPC    )
                ||  (opcode == `OP_JAL      )
                ||  (opcode == `OP_JALR     )
                ||  (opcode == `OP_BRANCH   )
                ||  (opcode == `OP_LOAD     )
                ||  (opcode == `OP_STORE    )
                ||  (opcode == `OP_SYSTEM   ))? `ISA_EXP_NO_EXP : `ISA_EXP_UNDEF_INSN;

// jump and branch

assign br_op =  opcode == `OP_JAL    ?                                                  `BR_OP_JAL     : 
                opcode == `OP_JALR   ?                                                  `BR_OP_JALR    :
                (opcode == `OP_BRANCH && (if_insn[`B_TYPE_FUNCT3] == `FUNCT3_BEQ ))?    `BR_OP_BEQ     :
                (opcode == `OP_BRANCH && (if_insn[`B_TYPE_FUNCT3] == `FUNCT3_BNE ))?    `BR_OP_BNE     :
                (opcode == `OP_BRANCH && (if_insn[`B_TYPE_FUNCT3] == `FUNCT3_BLT ))?    `BR_OP_BLT     :
                (opcode == `OP_BRANCH && (if_insn[`B_TYPE_FUNCT3] == `FUNCT3_BLTU))?    `BR_OP_BLTU    :
                (opcode == `OP_BRANCH && (if_insn[`B_TYPE_FUNCT3] == `FUNCT3_BGE ))?    `BR_OP_BGE     :
                (opcode == `OP_BRANCH && (if_insn[`B_TYPE_FUNCT3] == `FUNCT3_BGEU))?    `BR_OP_BGEU    : `BR_OP_NOP;

assign jp_rs1_addr = (opcode == `OP_JALR)? rs1_addr : `GPR_ADDR_WIDTH'b0;

assign stall_in_decoder = (opcode == `OP_JALR) && jp_rs1_rat_valid; // here wait commit to rat, or writeback!

assign jp_taken =   (opcode == `OP_JAL                                          ) ||
                    ((opcode == `OP_JALR) && (rs1_addr == `GPR_ADDR_WIDTH'b0)   ) ||
                    ((opcode == `OP_JALR) && !jp_rs1_rat_valid                  );


assign jp_addr = (opcode == `OP_JAL     )?  if_pc + imm :
                 ((opcode == `OP_JALR) && (rs1_addr == `GPR_ADDR_WIDTH'b0)  )?  imm                         :
                 ((opcode == `OP_JALR) && !jp_rs1_rat_valid                 )?  jp_rs1_data_fromGPR + imm   :`PC_WIDTH'b0;


// mem ctrl
assign mem_op = ((opcode == `OP_LOAD) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_LW))?  `MEM_OP_LOAD_LW     :
                ((opcode == `OP_LOAD) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_LH))?  `MEM_OP_LOAD_LH     :
                ((opcode == `OP_LOAD) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_LHU))? `MEM_OP_LOAD_LHU    :
                ((opcode == `OP_LOAD) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_LB))?  `MEM_OP_LOAD_LB     :
                ((opcode == `OP_LOAD) && (if_insn[`I_TYPE_FUNCT3] == `FUNCT3_LBU))? `MEM_OP_LOAD_LBU    :
                ((opcode == `OP_STORE) && (if_insn[`S_TYPE_FUNCT3] == `FUNCT3_SW))? `MEM_OP_SW          :
                ((opcode == `OP_STORE) && (if_insn[`S_TYPE_FUNCT3] == `FUNCT3_SH))? `MEM_OP_SH          :
                ((opcode == `OP_STORE) && (if_insn[`S_TYPE_FUNCT3] == `FUNCT3_SB))? `MEM_OP_SB          : `MEM_OP_NOP;


endmodule
`endif
