`ifndef OOOCPU_DIV
`define OOOCPU_DIV
`include "div_find_ones.v"
`include "define.v"
// 1. The division period is determined by the dividend, and the division
//    is calculated from the first non-zero position of the dividend.
// 2. In risc-v, division by zero and overflow do not generate exceptions,
//    and the division operation ends in a single cycle.
module div (
    input wire                      clk, 
    input wire                      rst_n,
    input wire                      div_start,
    input wire [2:0]                div_opcode,
    input wire [`WORD_WIDTH-1 : 0]  div_divident,
    input wire [`WORD_WIDTH-1 : 0]  div_divisor,
    output wire [`WORD_WIDTH-1 : 0] div_quotient,
    output wire [`WORD_WIDTH-1 : 0] div_remainder,
    output wire                     div_finish
);

localparam ALU_SUB = 1'b0;
localparam ALU_ADD = 1'b1;

reg [`WORD_WIDTH-1 : 0] div_divident_tmp;
reg [`WORD_WIDTH-1 : 0] div_divisor_tmp;
reg [2:0]               div_opcode_tmp;
wire [`WORD_WIDTH : 0]  absolute_divident;
wire [`WORD_WIDTH : 0]  absolute_divisor;
wire [32:0]             alu_remainder;
wire [4:0]              location_one;
wire [65:0]             initial_remainder_66reg;
wire                    div_by_0;
wire                    div_overflow;

reg [5:0]               shift_bit_cnt;
reg [65:0]              remainder_66reg;
reg                     div_shift_finish;
reg [`WORD_WIDTH-1 : 0] div_divident_r;
reg [`WORD_WIDTH-1 : 0] div_divisor_r;
reg [2:0]               div_opcode_r;

// div_divisor_r and div_divident_r save input value, because inputs only hold for 1 cycle

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        div_divident_r <= 'b0;
        div_divisor_r  <= 'b0;
        div_opcode_r  <= 'b0;
    end else if (div_start) begin
        div_divident_r <= div_divident;
        div_divisor_r  <= div_divisor;
        div_opcode_r  <= div_opcode;
    end
end

assign div_divident_tmp = (div_start)?  div_divident    :   div_divident_r;
assign div_divisor_tmp  = (div_start)?  div_divisor     :   div_divisor_r;
assign div_opcode_tmp   = (div_start)?  div_opcode      :   div_opcode_r;

assign absolute_divident =  (   (div_opcode_tmp == `DIV_OP_DIVU)
                            ||  ((div_opcode_tmp == `DIV_OP_DIV) && (!div_divident_tmp[31]))    )? {1'b0, div_divident_tmp} :
                            (   (div_opcode_tmp == `DIV_OP_DIV) && (div_divident_tmp[31])       )? (~{1'b1, div_divident_tmp} + 33'd1) : 33'b0;

assign absolute_divisor =   (   (div_opcode_tmp == `DIV_OP_DIVU)
                            ||  ((div_opcode_tmp == `DIV_OP_DIV) && (!div_divisor_tmp[31]))    )? {1'b0, div_divisor_tmp} :
                            (   (div_opcode_tmp == `DIV_OP_DIV) && (div_divisor_tmp[31])       )? (~{1'b1, div_divisor_tmp} + 33'd1) : 33'b0;

assign initial_remainder_66reg = (absolute_divident[32])? ({33'b0, absolute_divident} << 1) : ({33'b0, absolute_divident} << (6'd33 - location_one)); // Shift the first nonzero of the dividend to 66reg[33]

assign alu_remainder =  (div_start)?    (initial_remainder_66reg[65:33]-absolute_divisor) : (remainder_66reg[65:33]-absolute_divisor);

assign div_by_0     = (div_divisor_tmp == 32'd0);
assign div_overflow = (div_divident_tmp == {1'b1, {31{1'b0}}}) && (div_divisor_tmp == {32{1'b1}}) && (div_opcode_tmp == `DIV_OP_DIV);

assign div_quotient =   (div_by_0 && (div_opcode_tmp == `DIV_OP_DIVU)                                                                           )?  {32{1'b1}}                      : // x/0
                        (div_by_0 && (div_opcode_tmp == `DIV_OP_DIV)                                                                            )?  -1                              : // x/0
                        (div_overflow                                                                                                       )?  {1'b1, {31{1'b0}}}              : // -2^32/-1 = overflow
                        (((div_divident_tmp[31] && div_divisor_tmp[31]) || (!div_divident_tmp[31] && !div_divisor_tmp[31])) && (div_opcode_tmp == `DIV_OP_DIV)  )?  remainder_66reg[31:0]           : // -/- = +; +/+ = +
                        (((div_divident_tmp[31] && !div_divisor_tmp[31]) || (!div_divident_tmp[31] && div_divisor_tmp[31])) && (div_opcode_tmp == `DIV_OP_DIV)  )?  ~{remainder_66reg[31:0]} + 32'd1: // -/+ = -; +/- = -
                                                                                                                                                remainder_66reg[31:0];

assign div_remainder =  (div_by_0 && (div_opcode_tmp == `DIV_OP_DIVU)                                                                           )?  div_divident_tmp                    : // x/0
                        (div_by_0 && (div_opcode_tmp == `DIV_OP_DIV)                                                                            )?  div_divident_tmp                    : // x/0
                        (div_overflow                                                                                                       )?  32'b0                           : // -2^32/-1 = overflow
                        (((div_divident_tmp[31] && div_divisor_tmp[31]) || (div_divident_tmp[31] && !div_divisor_tmp[31]))  && (div_opcode_tmp == `DIV_OP_DIV)  )? ~{remainder_66reg[64:33]} + 32'd1: // -%- = -; -%+ = -
                        (((!div_divident_tmp[31] && div_divisor_tmp[31]) || (!div_divident_tmp[31] && !div_divisor_tmp[31])) && (div_opcode_tmp == `DIV_OP_DIV) )?  remainder_66reg[64:33]          : // +%- = +; +%+ = +                                        
                                                                                                                                                remainder_66reg[64:33];

assign div_finish   =   (div_start && (div_by_0 || (div_overflow))) || div_shift_finish;

div_find_ones u_div_find_ones(
    .data_in        (absolute_divident[31:0]    ),
    .location_one   (location_one               )
);

// In this cycle, the register shifted "shift_bit_cnt + 1" bits to the left
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        shift_bit_cnt <= 6'd0;
    end else if (div_by_0 || div_overflow) begin // div finish
        shift_bit_cnt <= 6'd33;
    end else if (div_start && absolute_divident[32]) begin  // divident= -2^32
        shift_bit_cnt <= 6'd1;
    end else if (div_start) begin
        shift_bit_cnt <= 6'd33 - location_one;
    end else if (shift_bit_cnt <= 6'd32) begin
        shift_bit_cnt <= shift_bit_cnt + 6'd1;
    end else if (shift_bit_cnt == 6'd33) begin // remainder_66reg dont shift
        shift_bit_cnt <= 6'd33;
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        remainder_66reg <= {33'b0, absolute_divident};
    end else if (div_start) begin
        if (alu_remainder[32]) begin            // divident < divisor
            if (location_one == 5'd0) begin     // div finish
                remainder_66reg <= {absolute_divisor, initial_remainder_66reg[31:0], 1'b0}; // remainder = absolute_divisor; quotient[location_one] = 1'b1;
            end else begin
                remainder_66reg <= {initial_remainder_66reg[64:0], 1'b0};                   // quotient[location_one] = 1'b0;
            end
        end else if (!alu_remainder[32]) begin  // divident >= divisor
            if (location_one == 5'd0) begin     // div finish
                remainder_66reg <= {alu_remainder[32:0], initial_remainder_66reg[31:0], 1'b1}; // remainder = alu_remainder; quotient[location_one] = 1'b1;
            end else begin
                remainder_66reg <= {alu_remainder[31:0],initial_remainder_66reg[32:0], 1'b1}; // quotient[location_one] = 1'b1;
            end
        end
    end else if (alu_remainder[32]) begin
        if (shift_bit_cnt < 6'd32) begin // divident < divisor
            remainder_66reg <= {remainder_66reg[64:0],1'b0};
        end else if (shift_bit_cnt == 6'd32) begin
            remainder_66reg <= {remainder_66reg[65:33],remainder_66reg[31:0],1'b0}; // The remainder is not shifted , The quotient is shifted one bit.
        end
    end else if (!alu_remainder[32]) begin
        if (shift_bit_cnt < 6'd32) begin // divident > divisor
            remainder_66reg <= {alu_remainder[31:0],remainder_66reg[32:0], 1'b1};
        end else if (shift_bit_cnt == 6'd32) begin
            remainder_66reg <= {alu_remainder[32:0],remainder_66reg[31:0],1'b1};  // The remainder is not shifted , The quotient is shifted one bit.
        end
    end
end

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        div_shift_finish <= 1'b0;
    end else if (shift_bit_cnt == 6'd32) begin
        div_shift_finish <= 1'b1;
    end else begin
        div_shift_finish <= 1'b0;
    end
end

endmodule
`endif
