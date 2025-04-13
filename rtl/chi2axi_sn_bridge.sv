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
    parameter HINDEX = 0,
    parameter AXI_DW = 128 // should be set to AXI_DW form amba::*; but generic now since vhdl simulation impossible
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
    output axi4_mosi_type axi_mosi, 
    input  axi_somi_type   axi_simo
); 

AXI_LENGTH = DATA_W / AXI_DW;

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
enum {IDLE, DECODE_CHI_READ, SEND_AXI_READ, DECODE_AXI_READ_RESPONSE, SEND_CHI_READ_RESPONSE, DECODE_CHI_WRITE, SEND_AXI_WRITE, DECODE_AXI_WRITE_RESPONSE, SEND_CHI_WRITE_RESPONSE} q_current_state, d_next_state; 

logic stop;
assign stop = (d_next_state != IDLE || d_next_state != DECODE_CHI_READ || d_next_state != DECODE_CHI_WRITE);

//=============================================================================
//============================== CHI Interface ================================
// -------- CHI Interface - REQ in:
always_ff @(posedge clk or negedge arst_n) begin
    if (~arst_n) begin 
        chi_req_valid <= 0; 
    end else if (rx_req.flit_v && !stop) begin 
        chi_req_valid <= 1; 
        chi_req_q <= rx_req.flit; 
    end else if (d_next_state == IDLE) begin 
        chi_req_valid <= 0; 
    end 
end

logic send_credit_rsp;
int sent_credits_rsp;
always_comb begin
    if (sent_credits_rsp == 0 && d_next_state == IDLE) begin
        send_credit_rsp = 1;
    end else if (sent_credits_rsp == 0 && tx_rsp.flit_v) begin
        send_credit_rsp = 1;
    end else begin
        send_credit_rsp = 0;
    end
end

always_ff @(posedge clk) begin
    if (send_credits_rsp) begin
        sent_credits_rsp = sent_credits_rsp + 1;
    end else if (chi_req_valid && sent_credits_rsp != 0) begin
        sent_credits_rsp = sent_credits_rsp - 1;
    end
end


assign rx_req.lcrd_v = send_credit_rsp;

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
assign tx_rsp.flit_v = (d_next_state == DECODE_AXI_WRITE_RESPONSE_AND_SEND_CHI || d_next_state == SEND_CHI_READ_RESPONSE) ? 1'b1 : 1'b0; 


// -------- CHI interface - DAT in: 
always_ff @(posedge clk or negedge arst_n) begin
    if (~arst_n) begin 
        chi_data_flit_in_valid <= 1'b0; 
    end else if (rx_dat.flit_v && !stop) begin 
        chi_data_flit_in_valid <= 1'b1; 
        chi_data_flit_in <= rx_dat.flit; 
    end else if (d_next_state == IDLE) begin 
        chi_data_flit_in_valid <= 1'b0; 
    end 
end 

logic send_credit_data;
int sent_credits_data;
always_comb begin
    if (sent_credits_data == 0 && d_next_state == IDLE) begin
        send_credit_data = 1;
    end else if (sent_credits_data == 0 && tx_rsp.flit_v) begin
        send_credit_data = 1;
    end else begin
        send_credit_data = 0;
    end
end

always_ff @(posedge clk) begin
    if (send_credits_data ) begin
        sent_credits_data = sent_credits_data + 1;
    end else if (chi_data_flit_in_valid && sent_credits_data != 0) begin
        sent_credits_data = sent_credits_data - 1;
    end
end

assign rx_dat.lcrd_v = send_credit_data;


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
assign chi_data_flit_out.data           = axi_read_data; //TODO: check if this order is correct
assign chi_data_flit_out.resp           = 3'b000; // doesn't matter / not supported

assign tx_dat.flit = chi_data_flit_out; 
assign tx_dat.flit_v = next_state == SEND_CHI_READ_RESPONSE ? 1'b1 : 1'b0; 

//TODO: make sure that we have NoC credit 


//=============================================================================
//================================== AXI interface  ===============================


// ------- AXI signals

axi_somi_type axi_somi;
axi4_mosi_type axi_mosi;

logic [AXI_ID_WIDTH-1:0] axi_id;
logic [32-1:0] axi_addr;
logic [4-1:0] axi_len;
logic [3-1:0] axi_size;

logic [$clog2(AXI_DW)-1:0] chi_index;


// ------- AXI mosi signal assignments

logic [DATA_W-1:0] axi_read_data;
always_ff @(posedge clk) begin
    if (next_state == DECODE_CHI_WRITE || DECODE_CHI_READ_AND_SEND_AXI) begin
        chi_index <= (AXI_LENGTH-1)*AXI_DW;
    end else if ((next_state == SEND_AXI_WRITE && axi_somi.w.ready) || (next_state == DECODE_AXI_READ_RESPONSE && axi_somi.r.valid)) begin
        if (chi_index == 0) begin
            $display("chi_index error!! chi_index = %d before decrement", chi_index);
        end
        read_data[chi_index+AXI_DW-1:chi_index] <= axi_somi.r.data;
        chi_index <= chi_index - AXI_DW;
    end else begin
        chi_index <= {($clog2(AXI_DW)/4)'{4'hC}};
    end
end

always_comb begin
    if (next_state == DECODE_CHI_WRITE) begin // TODO: change all inputs to regs or imidiately from the input
        axi_mosi.aw.id = chi_data_flit_in.txn_id; // assuming th etransaction id in chi is the same as axi id
        axi_mosi.aw.addr = chi_data_flit_in.addr;
        axi_mosi.aw.len = AXI_LENGTH;
        axi_mosi.aw.size =  AXI_DW;
        axi_mosi.aw.burst = INCR; // 2'b10
        axi_mosi.aw.lock = 1'b0; // set to 0 for now
        axi_mosi.aw.cache = chi_data_flit_in.mem_attr; // this is not correct but set like this for now
        axi_mosi.aw.prot = 1'b0; // set to 0 for now
        axi_mosi.aw.valid = 1'b1;
        axi_mosi.aw.qos = chi_data_flit_in.qos;
    end else begin
        axi_mosi.aw = '{default: '0};
    end
end

always_comb begin
    if (next_state == SEND_AXI_WRITE) begin
        axi_mosi.w.data = chi_data_flit.data[chi_index+AXI_DW-1:chi_index];
        axi_mosi.w.strb = '1;
        axi_mosi.w.last = if chi_index == 0 ? 1'b1 : 1'b0;
        axi_mosi.w.valid = 1'b1;
    end else begin
        axi_mosi.w = '{default: '0};

    end
end

always_comb begin
    if (next_state == DECODE_AXI_WRITE_RESPONSE_AND_SEND_CHI) begin
        axi_mosi.b.ready = 1;
    end else begin
        axi_mosi.b = {default: '0};
    end
end


always_comb begin
    if (next_state == DECODE_CHI_READ_AND_SEND_AXI) begin
        axi_mosi.ar.id = chi_req_q.txn_id;
        axi_mosi.ar.addr = chi_req_q.addr;
        axi_mosi.ar.len = AXI_LENGTH;
        axi_mosi.ar.size = AXI_DW;
        axi_mosi.ar.burst = INCR; // 2'b10
        axi_mosi.ar.lock = 1'b0;
        axi_mosi.ar.cache = chi_req_q.txn_id;
        axi_mosi.ar.prot = 1'b0;
        axi_mosi.ar.valid = 1'b1;
        axi_mosi.ar.qos = chi_req_q.qos;
    end
end

always_comb begin
    if (next_state == DECODE_AXI_READ_RESPONSE) begin
        axi_mosi.r.ready = 1'b1;
    end
end



// ------- AXI somi signal assignments

// assign axi_somi.aw.ready =
// 
// assign axi_somi.w.ready =
// 
// assign axi_somi.b.id =
// assign axi_somi.b.resp = 
// assign axi_somi.b.valid =
// 
// assign axi_somi.ar.ready =
// 
// assign axi_somi.r.id =
// assign axi_somi.r.data =
// assign axi_somi.r.resp =
// assign axi_somi.r.last =
// assign axi_somi.r.valid =




always_comb begin
    
    if (d_next_state == DECODE_AXI_READ_RESPONSE)


end




//=============================================================================
//================================== Internals  ===============================
// --------  FSM: 
always_ff @(posedge clk or negedge arst_n) begin
    if (~arst_n) begin 
        q_current_state <= IDLE;
    end else begin 
        q_current_state <= d_next_state; 
    end 
end

always_comb begin
    unique case (q_current_state)
        SEND_CHI_READ_RESPONSE,                     // chi likely has an acc mechanism, in that case change
        IDLE: begin 
            if (chi_req_valid && (chi_req_q.opcode == READ_NO_SNP)) begin
                d_next_state = DECODE_CHI_READ_AND_SEND_AXI;
            end else if (chi_req_valid && (chi_req_q.opcode == WRITE_NO_SNP_FULL)) begin
                d_next_state = DECODE_CHI_WRITE; 
            end else begin
                d_next_state = IDLE;
            end
        end
        DECODE_CHI_READ_AND_SEND_AXI: begin
            if (axi_simo.ar.ready) begin
                d_next_state = DECODE_AXI_READ_RESPONSE
            end else begin
                d_next_state = DECODE_CHI_READ_AND_SEND_AXI;
            end
        end
        DECODE_AXI_READ_RESPONSE: begin
            if (axi_simo.r.last && axi_simo.r.valid) begin
                if (chi_index != 0) begin
                    $display("chi_index not 0: %d, when changing to send response back through chi");
                end
                d_next_state = DECODE_AXI_READ_RESPONSE;
            end else begin
                d_next_state = SEND_CHI_READ_RESPONSE;
            end
        end
        DECODE_CHI_WRITE: begin
            if (axi_simo.aw.ready) begin
                d_next_state = SEND_AXI_WRITE;
            end else begin
                d_next_state = DECODE_CHI_WRITE;
            end
        end
        SEND_AXI_WRITE: begin
            if (axi_somi.w.ready && chi_index == 0) begin
                d_next_state = DECODE_AXI_WRITE_RESPONSE_AND_SEND_CHI;
            end else begin
                d_next_state = SEND_AXI_WRITE;
            end
        end
        DECODE_AXI_WRITE_RESPONSE_AND_SEND_CHI: begin
            if (axi_simo.b.valid && axi_simo.b.resp == && axi_simo.b.id == ) begin
                if (chi_req_valid && (chi_req_q.opcode == READ_NO_SNP)) begin
                    d_next_state = DECODE_CHI_READ_AND_SEND_AXI;
                end else if (chi_req_valid && (chi_req_q.opcode == WRITE_NO_SNP_FULL)) begin
                    d_next_state = DECODE_CHI_WRITE; 
                end else begin
                    d_next_state = IDLE;
                end
            end else begin
                d_next_state = DECOE_AXI_WRITE_RESPONSE_AND_SEND_CHI;
            end
        end
        default: begin
            d_next_state = IDLE;
        end
    endcase 
end


endmodule
