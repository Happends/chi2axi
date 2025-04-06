



import grchirnf_pkg::*;
import amba::*;

module chi2axi #(
    parameter HINDEX = 0
    ) 
    (
        input logic clk,
        input logic arst_n,

        chi_channel_inf.rx rx_reg,
        chi_channel_inf.rx rx_dat,
        chi_channel_inf.tx tx_rsp,
        chi_channel_inf.tx tx_dat,

        input axi4_somi_type,
        output axi4_mosi_type,

    );








endmodule
