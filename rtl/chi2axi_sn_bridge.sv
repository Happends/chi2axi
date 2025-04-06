/// Notes: 
//
// 1- LLC_DATA_WIDTH is 512 while CFG_AHBDW parameter is set to 128. 
//    So, we need to communicate in 4-beat incrementing bursts
//
// 2- LLC_ADDR_WIDTH is set to 48 while the haddr parameter in AHB master interface is 32.
//    we simply prune the 16 MSBs and only use the lower 32 bits of address in our experiments. 
//

import chi_package::*; 
import amba::*;
import llc_config_pkg::*; 
import llc_common_pkg::*; 

module chi2ahb_sn_bridge #(
    parameter HINDEX = 0
)
(
    input logic clk, 
    input logic arst_n, 
    // CHI side 
    chi_channel_inf.rx  rx_req, 
    chi_channel_inf.rx  rx_dat, 
    chi_channel_inf.tx  tx_rsp, 
    chi_channel_inf.tx  tx_dat,
    // AHB side
    output ahb_mst_out_type  ahb_mo, 
    input  ahb_mst_in_type   ahb_mi
); 

request_flit_t  chi_req_q; 
data_flit_t     chi_data_flit_out, chi_data_flit_in;
response_flit_t chi_rsp_flit; 
logic           chi_req_valid; 
logic           chi_data_flit_in_valid; 
logic           done; 
logic [31:0]    ahb_addr_q, ahb_addr_d, ahb_addr_increment; 
logic [127:0]   rdata0, rdata1, rdata2, rdata3; 
logic           read_burst; 
logic           write_burst; 
enum {RESET, IDLE, R0, R1, R2, R3, RSEND, WRECEIVE, W0, W1, W2, W3} q_current_state, d_next_state, q_last_state; 

assign read_burst = ((q_current_state == R0) || (q_current_state == R1) || (q_current_state == R2) || (q_current_state == R3)) ? 1'b1 : 1'b0; 
assign write_burst = ((q_current_state == W0) || (q_current_state == W1) || (q_current_state == W2) || (q_current_state == W3)) ? 1'b1 : 1'b0; 

//=============================================================================
//============================== CHI Interface ================================
// -------- CHI Interface - REQ in:
always_ff @(posedge clk or negedge arst_n) begin
    if (~arst_n) begin 
        chi_req_valid <= 0; 
    end else if (rx_req.flit_v) begin 
        chi_req_valid <= 1; 
        chi_req_q <= rx_req.flit; 
    end else if (d_next_state == IDLE) begin 
        chi_req_valid <= 0; 
    end 
end

assign rx_req.lcrd_v = (q_current_state != IDLE) && (d_next_state == IDLE) ? 1'b1 : 1'b0; 

// -------- CHI Interface - RSP out: 

assign chi_rsp_flit.src_id      = SN_ID;
assign chi_rsp_flit.tgt_id      = HN_ID;
assign chi_rsp_flit.txn_id      = chi_req_q.txn_id; // assuming COMP_DBID_RESP
assign chi_rsp_flit.opcode      = COMP_DBID_RESP;
assign chi_rsp_flit.dbid        = 0; // there is only one req buff entry 
assign chi_rsp_flit.trace_tag   = 0; // mem tagging not supported
assign chi_rsp_flit.tag_op      = 0; // mem tagging not supported 
assign chi_rsp_flit.pcrd_type   = 0; // retry not supported 
assign chi_rsp_flit.cbusy       = 0; // not supported 
assign chi_rsp_flit.fwd_state   = 0; // not supported 
assign chi_rsp_flit.resp_err    = 0; // Normal: no error 
assign chi_rsp_flit.resp        = 0; // default value 
assign chi_rsp_flit.qos         = '{default: 1}; // set max prio (same for all messages)

assign tx_rsp.flit = chi_rsp_flit; 
assign tx_rsp.flit_v = (q_current_state == IDLE) && (d_next_state == WRECEIVE) ? 1'b1 : 1'b0; 


// -------- CHI interface - DAT in: 
always_ff @(posedge clk or negedge arst_n) begin
    if (~arst_n) begin 
        chi_data_flit_in_valid <= 1'b0; 
    end else if (rx_dat.flit_v) begin 
        chi_data_flit_in_valid <= 1'b1; 
        chi_data_flit_in <= rx_dat.flit; 
    end else if (d_next_state == IDLE) begin 
        chi_data_flit_in_valid <= 1'b0; 
    end 
end 

assign rx_dat.lcrd_v = (q_current_state != IDLE) && (d_next_state == IDLE) ? 1'b1 : 1'b0; 


// -------- CHI interface - DAT out: 
assign chi_data_flit_out.src_id         = SN_ID;
assign chi_data_flit_out.txn_id         = chi_req_q.txn_id; 
assign chi_data_flit_out.be             = '{default:'1}; // all bytes enabled 
assign chi_data_flit_out.resp_err       = 0; // normal: no error  
assign chi_data_flit_out.cah            = 1; // inclusive assumption 
assign chi_data_flit_out.trace_tag      = 0; // mem. tagging not supported 
assign chi_data_flit_out.tu             = 0; // mem. tagging not supported 
assign chi_data_flit_out.tag            = 0; // mem. tagging not supported 
assign chi_data_flit_out.tag_op         = 0; // mem. tagging not supported 
assign chi_data_flit_out.data_id        = 2'b00; // 512 bit data bus width
assign chi_data_flit_out.cc_id          = 0; // doesn't matter / not supported
assign chi_data_flit_out.cbusy          = 0; // not supported 
assign chi_data_flit_out.data_source    = 0; // not required 
assign chi_data_flit_out.qos            = '{default: 1}; // set max prio (same for all messages)
assign chi_data_flit_out.tgt_id         = HN_ID; 
assign chi_data_flit_out.opcode         = COMP_DATA; 
assign chi_data_flit_out.home_nid       = HN_ID; 
assign chi_data_flit_out.dbid           = 0; // there is only one req buff entry 
assign chi_data_flit_out.data           = {rdata0, rdata1, rdata2, rdata3}; //TODO: check if this order is correct
assign chi_data_flit_out.resp           = 3'b000; // doesn't matter / not supported

assign tx_dat.flit = chi_data_flit_out; 
assign tx_dat.flit_v = (q_last_state == RSEND) ? 1'b1 : 1'b0; 

//TODO: make sure that we have NoC credit 



//=============================================================================
//================================== Internals  ===============================

// --------  handling the address for different beats:
always_ff @(posedge clk or negedge arst_n) begin
    if (~arst_n) begin 
        ahb_addr_q <= 0; 
    end else begin 
        ahb_addr_q <= ahb_addr_d; 
    end     
end

// each beat contains 128 bits i.e. 4-words so the address should increase by 128/8 = 16 
assign ahb_addr_increment = ahb_addr_q + 16; 

always_comb begin
    // ahb_addr_q should be input addr while q_current_state is R0 or W0
    // ahb_addr_q should be its previous value + increment while q_current_state is R1-3 or W1-3 
    // ahb_addr_q should should not increment if the state is not changing i.e. hready = 0 
    //if (!(read_burst || write_burst)) begin 
    //    ahb_addr_d = 0; 
    //end else 
    if ((d_next_state == R0) || (d_next_state == W0)) begin 
        ahb_addr_d = chi_req_q.addr[31:0]; 
    end else if (d_next_state != q_current_state) begin 
        ahb_addr_d = ahb_addr_increment; 
    end else begin 
        ahb_addr_d = ahb_addr_q; 
    end 
end

// --------  FSM: 
always_ff @(posedge clk or negedge arst_n) begin
    if (~arst_n) begin 
        q_current_state <= RESET; 
        q_last_state <= RESET; 
    end else begin 
        q_current_state <= d_next_state; 
        q_last_state <= q_current_state; 
    end 
end

always_comb begin
    d_next_state = IDLE; 
    unique case (q_current_state)
        RESET: begin 
            d_next_state = IDLE; 
        end 
        IDLE: begin 
            if (chi_req_valid && (chi_req_q.opcode == READ_NO_SNP)) begin 
                d_next_state = R0; 
            end else if (chi_req_valid && (chi_req_q.opcode == WRITE_NO_SNP_FULL)) begin 
                d_next_state = WRECEIVE;
            end else begin 
                d_next_state = IDLE; 
            end 
        end
        R0: begin 
            if (!ahb_mi.hready) begin 
                d_next_state = R0; 
            end else begin 
                d_next_state = R1; 
            end 
        end 
        R1: begin 
            if (!ahb_mi.hready) begin 
                d_next_state = R1; 
            end else begin 
                d_next_state = R2; 
            end 
        end 
        R2: begin 
            if (!ahb_mi.hready) begin 
                d_next_state = R2; 
            end else begin 
                d_next_state = R3; 
            end 
        end 
        R3: begin 
            if (!ahb_mi.hready) begin 
                d_next_state = R3; 
            end else begin 
                d_next_state = RSEND; 
            end 
        end 
        RSEND: begin 
            d_next_state = IDLE; 
        end 
        WRECEIVE: begin 
            if (chi_data_flit_in_valid) begin 
                d_next_state = W0; 
            end else begin 
                d_next_state = WRECEIVE; 
            end 
        end 
        W0: begin 
            if (!ahb_mi.hready) begin 
                d_next_state = W0; 
            end else begin 
                d_next_state = W1; 
            end 
        end 
        W1: begin 
            if (!ahb_mi.hready) begin 
                d_next_state = W1; 
            end else begin 
                d_next_state = W2; 
            end 
        end 
        W2: begin 
            if (!ahb_mi.hready) begin 
                d_next_state = W2; 
            end else begin 
                d_next_state = W3; 
            end 
        end 
        W3: begin 
            if (!ahb_mi.hready) begin 
                d_next_state = W3; 
            end else begin 
                d_next_state = IDLE; 
            end 
        end 
    endcase 
end

//=============================================================================
//================================ AHB Interface ==============================

// AHB master interface 
assign ahb_mo.hlock = 1'b0; 
assign ahb_mo.haddr = ahb_addr_q; 
   assign ahb_mo.hsize = 3'b100;
 // 128 (4-word)
   assign ahb_mo.hburst = 3'b011;
 // INCR4 4-beat incrementing burst 
   assign ahb_mo.hprot = 4'b0011;
 // Modifilable|Bufferable|Privileged|Data/Opcode
assign ahb_mo.hirq = 0; 
assign ahb_mo.hconfig = '{default: 0};  
assign ahb_mo.hindex = HINDEX; 
assign ahb_mo.hbusreq = (read_burst || write_burst) ? 1'b1 : 1'b0; 
assign ahb_mo.hwrite = write_burst; 
always_comb begin
    if ((q_current_state == R0) || (q_current_state == W0)) begin 
        ahb_mo.htrans   = 2'b10; //NONSEQ
    end else if (read_burst || write_burst) begin 
        ahb_mo.htrans   = 2'b11; //SEQ 
    end else begin 
        ahb_mo.htrans   = 2'b00; //IDLE 
    end 
end
always_comb begin
    if ((q_last_state == W0) && (q_current_state == W1)) begin 
        ahb_mo.hwdata = chi_data_flit_in.data[511:384]; //TODO: Check if this order is correct
    end else if ((q_last_state == W1) && (q_current_state == W2)) begin 
        ahb_mo.hwdata = chi_data_flit_in.data[383:256];
    end else if ((q_last_state == W2) && (q_current_state == W3)) begin 
        ahb_mo.hwdata = chi_data_flit_in.data[255:128];
    end else if ((q_last_state == W3) && (q_current_state == IDLE)) begin 
        ahb_mo.hwdata = chi_data_flit_in.data[127:0];
    end else begin 
        ahb_mo.hwdata = 0; 
    end 
end


// ------ read data buffers: 
always_ff @(posedge clk or negedge arst_n) begin
    if (~arst_n) begin 
        rdata0 <= 0; 
        rdata1 <= 0; 
        rdata2 <= 0; 
        rdata3 <= 0; 
    end else if (ahb_mi.hready && (q_current_state == R1)) begin
        rdata0 <= ahb_mi.hrdata;        
    end else if (ahb_mi.hready && (q_current_state == R2)) begin
        rdata1 <= ahb_mi.hrdata;
    end else if (ahb_mi.hready && (q_current_state == R3)) begin
        rdata2 <= ahb_mi.hrdata;        
    end else if (ahb_mi.hready && (q_current_state == RSEND)) begin
        rdata3 <= ahb_mi.hrdata;  
    end     
end




endmodule
