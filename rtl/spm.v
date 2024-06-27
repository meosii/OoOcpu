`ifndef OOOCPU_SPM
`define OOOCPU_SPM

//Dual Port RAM
//One port for IF, the other for MEM

module spm #(
    parameter READ  = 1,
    parameter WRITE = 0
)(
    input wire              clk,
    input wire              rst_n,
    input wire  [31 : 0]    spm_rdaddress,
    input wire  [31 : 0]    spm_wraddress,
    input wire              spm_rden,
    input wire              spm_wren,
    input wire  [31 : 0]    spm_write_data,
    input wire  [3 : 0]     spm_store_byteena,
    output wire [31 : 0]    spm_rd_data
);

reg [7 : 0] spm [0 : 1023];

assign spm_rd_data  =   (spm_rden) ? 
                            {   spm[spm_wraddress],
                                spm[spm_wraddress + 1], 
                                spm[spm_wraddress + 2], 
                                spm[spm_wraddress + 3] } : 32'b0;

always @(posedge clk) begin
    if (rst_n) begin
        if (spm_wren && spm_store_byteena[3]) begin
            spm[spm_wraddress]       <= spm_write_data[31 : 24];
        end
    end
end

always @(posedge clk) begin
    if (rst_n) begin
        if (spm_wren && spm_store_byteena[2]) begin
            spm[spm_wraddress + 1]   <= spm_write_data[23 : 16];
        end
    end
end

always @(posedge clk) begin
    if (rst_n) begin
        if (spm_wren && spm_store_byteena[1]) begin
            spm[spm_wraddress + 2]   <= spm_write_data[15 : 8];
        end
    end
end

always @(posedge clk) begin
    if (rst_n) begin
        if (spm_wren && spm_store_byteena[0]) begin
            spm[spm_wraddress + 3]   <= spm_write_data[7 : 0];
        end
    end
end

endmodule

`endif 
