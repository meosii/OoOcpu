`ifndef SIICPU_MUL
`define SIICPU_MUL
`include "mul_booth_radix_4.v"
`include "mul_wallace.v"
`include "define.v"
module mul #(
    parameter SIGNED_WORD_WIDTH = `WORD_WIDTH + 1,
    parameter PARTIAL_PRODUCT_WIDTH = SIGNED_WORD_WIDTH + SIGNED_WORD_WIDTH
)(
    input wire                                  clk,
    input wire                                  rst_n,
    // from fu_top
    input wire                                  mul_en,
    input wire [2:0]                            mul_opcode,
    input wire [`WORD_WIDTH-1 : 0]              mul_data1,
    input wire [`WORD_WIDTH-1 : 0]              mul_data2,
    output wire [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_result,
    output wire                                 mul_result_valid
);

wire [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_add_a;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_add_b;
// from booth radix-4
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product1;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product2;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product3;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product4;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product5;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product6;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product7;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product8;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product9;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product10;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product11;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product12;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product13;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product14;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product15;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product16;
wire [PARTIAL_PRODUCT_WIDTH-1 : 0]  mul_partial_product17;
// reg 1
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product1_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product2_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product3_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product4_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product5_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product6_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product7_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product8_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product9_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product10_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product11_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product12_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product13_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product14_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product15_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product16_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_partial_product17_r1;
reg                                 mul_en_r1;
// reg 2
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_add_a_r1;
reg [PARTIAL_PRODUCT_WIDTH-1 : 0]   mul_add_b_r1;
reg                                 mul_en_r2;

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        mul_partial_product1_r1  <= 'b0;
        mul_partial_product2_r1  <= 'b0;
        mul_partial_product3_r1  <= 'b0;
        mul_partial_product4_r1  <= 'b0;
        mul_partial_product5_r1  <= 'b0;
        mul_partial_product6_r1  <= 'b0;
        mul_partial_product7_r1  <= 'b0;
        mul_partial_product8_r1  <= 'b0;
        mul_partial_product9_r1  <= 'b0;
        mul_partial_product10_r1 <= 'b0;
        mul_partial_product11_r1 <= 'b0;
        mul_partial_product12_r1 <= 'b0;
        mul_partial_product13_r1 <= 'b0;
        mul_partial_product14_r1 <= 'b0;
        mul_partial_product15_r1 <= 'b0;
        mul_partial_product16_r1 <= 'b0;
        mul_partial_product17_r1 <= 'b0;
        mul_en_r1                <= 1'b0;
    end else begin
        mul_partial_product1_r1  <= mul_partial_product1;
        mul_partial_product2_r1  <= mul_partial_product2;
        mul_partial_product3_r1  <= mul_partial_product3;
        mul_partial_product4_r1  <= mul_partial_product4;
        mul_partial_product5_r1  <= mul_partial_product5;
        mul_partial_product6_r1  <= mul_partial_product6;
        mul_partial_product7_r1  <= mul_partial_product7;
        mul_partial_product8_r1  <= mul_partial_product8;
        mul_partial_product9_r1  <= mul_partial_product9;
        mul_partial_product10_r1 <= mul_partial_product10;
        mul_partial_product11_r1 <= mul_partial_product11;
        mul_partial_product12_r1 <= mul_partial_product12;
        mul_partial_product13_r1 <= mul_partial_product13;
        mul_partial_product14_r1 <= mul_partial_product14;
        mul_partial_product15_r1 <= mul_partial_product15;
        mul_partial_product16_r1 <= mul_partial_product16;
        mul_partial_product17_r1 <= mul_partial_product17;
        mul_en_r1                <= mul_en;
    end
end

mul_booth_radix_4 u_mul_booth_radix_4(
    .mul_opcode                     (mul_opcode                     ),
    .mul_data1                      (mul_data1                      ),
    .mul_data2                      (mul_data2                      ),
    .mul_partial_product1           (mul_partial_product1           ),
    .mul_partial_product2           (mul_partial_product2           ),
    .mul_partial_product3           (mul_partial_product3           ),
    .mul_partial_product4           (mul_partial_product4           ),
    .mul_partial_product5           (mul_partial_product5           ),
    .mul_partial_product6           (mul_partial_product6           ),
    .mul_partial_product7           (mul_partial_product7           ),
    .mul_partial_product8           (mul_partial_product8           ),
    .mul_partial_product9           (mul_partial_product9           ),
    .mul_partial_product10          (mul_partial_product10          ),
    .mul_partial_product11          (mul_partial_product11          ),
    .mul_partial_product12          (mul_partial_product12          ),
    .mul_partial_product13          (mul_partial_product13          ),
    .mul_partial_product14          (mul_partial_product14          ),
    .mul_partial_product15          (mul_partial_product15          ),
    .mul_partial_product16          (mul_partial_product16          ),
    .mul_partial_product17          (mul_partial_product17          )
);

mul_wallace #(
    .SIGNED_WORD_WIDTH              (SIGNED_WORD_WIDTH              ),
    .PARTIAL_PRODUCT_WIDTH          (PARTIAL_PRODUCT_WIDTH          )
) u_mul_wallace(
    .mul_partial_product1           (mul_partial_product1_r1        ),
    .mul_partial_product2           (mul_partial_product2_r1        ),
    .mul_partial_product3           (mul_partial_product3_r1        ),
    .mul_partial_product4           (mul_partial_product4_r1        ),
    .mul_partial_product5           (mul_partial_product5_r1        ),
    .mul_partial_product6           (mul_partial_product6_r1        ),
    .mul_partial_product7           (mul_partial_product7_r1        ),
    .mul_partial_product8           (mul_partial_product8_r1        ),
    .mul_partial_product9           (mul_partial_product9_r1        ),
    .mul_partial_product10          (mul_partial_product10_r1       ),
    .mul_partial_product11          (mul_partial_product11_r1       ),
    .mul_partial_product12          (mul_partial_product12_r1       ),
    .mul_partial_product13          (mul_partial_product13_r1       ),
    .mul_partial_product14          (mul_partial_product14_r1       ),
    .mul_partial_product15          (mul_partial_product15_r1       ),
    .mul_partial_product16          (mul_partial_product16_r1       ),
    .mul_partial_product17          (mul_partial_product17_r1       ),
    .mul_add_a                      (mul_add_a                      ),
    .mul_add_b                      (mul_add_b                      )
);
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        mul_add_a_r1             <= 'b0;
        mul_add_b_r1             <= 'b0;
        mul_en_r2                <= 1'b0;
    end else begin
        mul_add_a_r1             <= mul_add_a;
        mul_add_b_r1             <= mul_add_b;
        mul_en_r2                <= mul_en_r1;
    end
end

assign mul_result       = mul_add_a_r1 + mul_add_b_r1;
assign mul_result_valid = mul_en_r2;

endmodule
`endif
