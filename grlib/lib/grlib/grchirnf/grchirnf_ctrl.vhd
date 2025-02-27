------------------------------------------------------------------------------
--  This file is a part of the GRLIB VHDL IP LIBRARY
--  Copyright (C) 2003 - 2008, Gaisler Research
--  Copyright (C) 2008 - 2014, Aeroflex Gaisler
--  Copyright (C) 2015 - 2023, Cobham Gaisler
--  Copyright (C) 2023,        Frontgrade Gaisler
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; version 2.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 
-----------------------------------------------------------------------------   
-- Entity:      grchirnf_ctrlrnf_ctrl
-- File:        grchirnf_ctrl.vhd
-- Author:      Krishna K R - Frontgrade Gaisler AB
-- Description: Main control module for fully coherent request node.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library grlib;
use grlib.config_types.all;
use grlib.config.all;
use grlib.stdlib.all;
use grlib.amba.all;
use grlib.devices.all;
use grlib.grchirnf_pkg.all;

-----------------------------------------------------------------------------
-- state machines for generating :
-- CHI request and data flits towards the homenode when and L2C miss happens
-- CHI response flits towards home node
------------------------------------------------------------------------------
-- 
-----------------------------------------------------------------------------

entity grchirnf_ctrl is
  generic (
    dbits      : integer range 128 to 512 := DATA_W;  -- Data bus width allowed values :
                                                      -- 128, 256 and 512 bits
    addr_width : integer range 32 to 52   := ADDR_W);
  port (
    rstn             : in  std_ulogic;  -- Active low reset signal
    clk              : in  std_ulogic;  -- System clock signal
    sts_out          : out ctrlmod_sts_type;
    apb_ctrl         : in  grchirnf_ctrl_type;  -- Control configuration signals from
                                                -- APB interface
    err_status       : in  std_ulogic;  -- Error status in the APB register
    -- Signals from the l2c backend
    -- Note : We are using the same signals that was used in l2c_lite for bus
    -- master integration    
    -- READ SIGNALS
    bmrd_addr        : in  std_logic_vector(addr_width-1 downto 0);
    bmrd_size        : in  std_logic_vector(log2ext(max_size_l)-1 downto 0);
    bmrd_req         : in  std_logic;
    -- WRITE SIGNALS
    bmwr_addr        : in  std_logic_vector(addr_width-1 downto 0);
    bmwr_size        : in  std_logic_vector(log2ext(max_size_l)-1 downto 0);
    bmwr_req         : in  std_logic;
    bmwr_data        : in  std_logic_vector(dbits-1 downto 0);
    -- Signals to the l2c backend
    -- READ SIGNALS
    bmrd_req_granted : out std_logic;
    bmrd_data        : out std_logic_vector(dbits-1 downto 0);
    bmrd_valid       : out std_logic;
    bmrd_done        : out std_logic;
    bmrd_error       : out std_logic;
    -- WRITE SIGNALS
    bmwr_full        : out std_logic;
    bmwr_done        : out std_logic;
    bmwr_error       : out std_logic;
    bmwr_req_granted : out std_logic;
    -- Snoop signals between RNF and L2C-lite  -- Note CR: Bundle these signals?
    c_snp_done       : in  std_logic;
    cl_status_l2     : in  std_logic_vector(1 downto 0);
    dirty_d_v        : in  std_logic;
    snp_data_l2      : in  std_logic_vector(dbits-1 downto 0);
    l2_snp_valid     : out std_logic;
    l2_snp_addr      : out std_logic_vector(addr_width-1 downto 0);
    -- RNF signals towards HNF
    rnf_in           : in  rnf_in_type;         -- Signals from home node
    rnf_out          : out rnf_out_type         -- Signals to the home node
    );
end entity grchirnf_ctrl;

------------------------------------------------------------------------------
-- Architecture of grchirnf_ctrl
------------------------------------------------------------------------------

architecture rtl of grchirnf_ctrl is
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";

  -----------------------------------------------------------------------------
  -- Component instantiation
  ----------------------------------------------------------------------------- 
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------
  -- Reset configuration
  constant ASYNC_RST : boolean := GRLIB_CONFIG_ARRAY(grlib_async_reset_enable) = 1;

  -----------------------------------------------------------------------------
  -- Type and record 
  -----------------------------------------------------------------------------
  -- input type
  type input_type is record
    -- READ signals
    bmrd_addr     : std_logic_vector(addr_width-1 downto 0);
    bmrd_size     : std_logic_vector(log2ext(max_size_l)-1 downto 0);
    bmrd_req      : std_logic;
    -- WRITE SIGNALS
    bmwr_addr     : std_logic_vector(addr_width-1 downto 0);
    bmwr_size     : std_logic_vector(log2ext(max_size_l)-1 downto 0);
    bmwr_req      : std_logic;
    bmwr_data     : std_logic_vector(dbits-1 downto 0);
    -- Signals from L2C-lite
    c_snp_done    : std_logic;
    cl_status_l2  : std_logic_vector(1 downto 0);
    dirty_d_v     : std_logic;
    snp_data_l2   : std_logic_vector(dbits-1 downto 0);
  end record;

  --- Control FSM states ---
  type ctrl_state_type is (idle, gen_rd_req, wait_data, gen_resp, gen_wr_req, wait_resp, gen_data);
  -- Snoop FSM state
  type snp_state_type is (idle, wait_l2_resp, rnf_snp_resp);

  -- grchirnf_ctrl local reg type
  type ctrl_reg_type is record
    state          : ctrl_state_type;
    snp_state      : snp_state_type;
    apb_err_flg    : std_logic;
    error          : std_logic;
    ongoing        : std_logic;
    rd_req_granted : std_logic;
    rd_error       : std_logic;
    wr_error       : std_logic;
    wr_req_granted : std_logic;
    wr_bk_data     : std_logic_vector(dbits-1 downto 0);
    txn_size       : std_logic_vector(log2ext(max_size_l)-1 downto 0);
    dbid           : std_logic_vector(RSP_DBID_LN-1 downto 0);
    snp_dbid       : std_logic_vector(RSP_DBID_LN-1 downto 0);
    tgt_addr       : std_logic_vector(addr_width-1 downto 0);
    req_lc_cnt     : std_logic_vector(3 downto 0);
    txd_lc_cnt     : std_logic_vector(3 downto 0);
    txrsp_lc_cnt   : std_logic_vector(3 downto 0);
    snp_lcr_sts    : std_logic;
    cl_dirty       : std_logic;
    snpdata        : std_logic_vector(dbits-1 downto 0);
  end record;

  -- Reset value for grchirnf_ctrl local reg type
  constant CTRL_REG_RST : ctrl_reg_type := (
    state          => idle,
    snp_state      => idle,
    apb_err_flg    => '0',
    error          => '0',
    ongoing        => '0',
    rd_req_granted => '1',
    rd_error       => '0',
    wr_error       => '0',
    wr_req_granted => '1',
    wr_bk_data     => (others => '0'),
    txn_size       => (others => '0'),
    dbid           => (others => '0'),
    snp_dbid       => (others => '0'),
    tgt_addr       => (others => '0'),
    req_lc_cnt     => X"1",
    txd_lc_cnt     => X"1",
    txrsp_lc_cnt   => X"1",
    snp_lcr_sts    => '0',
    cl_dirty       => '0',
    snpdata        => (others => '0')
    );

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  signal r, rin     : ctrl_reg_type;
  signal inp        : input_type;
  -- Debug signals fro req flit
  signal qos        : std_logic_vector(QOS_W-1 downto 0);
  signal tgtid      : std_logic_vector(NID_W-1 downto 0);
  signal srcid      : std_logic_vector(NID_W-1 downto 0);
  signal txnid      : std_logic_vector(TXNID_W-1 downto 0);
  signal return_nid : std_logic_vector(NID_W-1 downto 0);
  signal stashid_v  : std_ulogic;
  signal rtn_txnid  : std_logic_vector(TXNID_W-1 downto 0);
  signal opc        : std_logic_vector(6 downto 0);
  signal size       : std_logic_vector(2 downto 0);
  signal addr       : std_logic_vector(ADDR_W-1 downto 0);
  signal ns         : std_ulogic;
  signal nse        : std_ulogic;
  signal lklysh     : std_ulogic;
  signal allwretry  : std_ulogic;
  signal order      : std_logic_vector(1 downto 0);
  signal pcrdtype   : std_logic_vector(PCRDTYPE_W-1 downto 0);
  signal memattr    : std_logic_vector(3 downto 0);
  signal snpattr    : std_ulogic;
  signal pgroupid   : std_logic_vector(7 downto 0);
  signal excl       : std_ulogic;
  signal expcompack : std_ulogic;
  signal tagop      : std_logic_vector(TAGOP_W-1 downto 0);
  signal tracetag   : std_ulogic;
  signal padding    : std_logic_vector(15 downto 0);

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin  -- rtl

  -----------------------------------------------------------------------------
  -- Signal Assignments
  -----------------------------------------------------------------------------
    -- Input signal reading
        -- READ signals
    inp.bmrd_addr   <= bmrd_addr;
    inp.bmrd_size   <= bmrd_size;
    inp.bmrd_req    <= bmrd_req;
    -- WRITE SIGNALS
    inp.bmwr_addr   <= bmwr_addr;
    inp.bmwr_size   <= bmwr_size;
    inp.bmwr_req    <= bmwr_req;
    inp.bmwr_data   <= bmwr_data;
    -- Signals from L2C-lite
    inp.c_snp_done  <= c_snp_done;
    inp.cl_status_l2 <= cl_status_l2;
    inp.dirty_d_v   <= dirty_d_v;
    inp.snp_data_l2 <= snp_data_l2;

  -----------------------------------------------------------------------------
  -- Combinational logic
  ----------------------------------------------------------------------------- 
  comb : process (r, rnf_in, apb_ctrl, err_status, inp)

    variable v              : ctrl_reg_type;
    variable snp_lcrdv      : std_ulogic;
    variable rxrsp_lcrdv    : std_ulogic;
    variable txreq_flit_p   : std_logic;
    variable txreq_flit_v   : std_logic;
    variable txrsp_flit_p   : std_logic;
    variable txdata_flit_p  : std_logic;
    variable rsp_opc        : std_logic_vector(RSP_OPC_LN-1 downto 0);
    variable rsp_type       : std_logic_vector(RSP_RSP_LN-1 downto 0);
    variable curr_size      : std_logic_vector(RQ_SIZE_LN-1 downto 0);
    variable req_type       : std_logic_vector(RQ_OPC_LN-1 downto 0);
    variable wr_full        : std_logic;
    variable txreq_flit     : std_logic_vector(REQFLT_W-1 downto 0);
    variable wr_data_opc    : std_logic_vector(D_OPC_LN-1 downto 0);
    variable txdata_flit_v  : std_logic;
    variable snpdata_flit_v : std_logic;
    variable snprsp_flit_v  : std_logic;
    variable wr_done        : std_logic;
    variable rxd_lcrdv      : std_ulogic;
    variable rxdata         : std_logic_vector(dbits-1 downto 0);
    variable txrsp_flit_v   : std_logic;
    variable txrsp_flit     : std_logic_vector(RESPFLT_W-1 downto 0);
    variable rd_valid       : std_ulogic;
    variable rd_done        : std_logic;
    variable txdata_flit    : std_logic_vector(DATAFLT_W-1 downto 0);
    variable snp_valid      : std_logic;
    variable snp_addr       : std_logic_vector(ADDR_W-1 downto 0);
    variable snp_opc        : std_logic_vector(SNP_OPC_LN-1 downto 0);
    variable snp_resp_opc   : std_logic_vector(RSP_OPC_LN-1 downto 0);
    variable snp_rsp_type   : std_logic_vector(RSP_RSP_LN-1 downto 0);
    variable snp_data_opc   : std_logic_vector(D_OPC_LN-1 downto 0);
  begin
    -- Variable initialization
    v              := r;
    snp_lcrdv      := '0';
    rxd_lcrdv      := '0';
    rxrsp_lcrdv    := '0';
    txreq_flit_p   := '0';
    txreq_flit_v   := '0';
    txrsp_flit_p   := '0';
    txrsp_flit_v   := '0';
    txdata_flit_p  := '0';
    txdata_flit_v  := '0';
    snpdata_flit_v := '0';
    snprsp_flit_v  := '0';
    wr_full        := '1';
    wr_done        := '0';
    txreq_flit     := (others => '0');
    txrsp_flit     := (others => '0');
    txdata_flit    := (others => '0');
    rd_valid       := '0';
    rd_done        := '0';
    snp_valid      := '0';
    snp_addr       := (others => '0');
    snp_resp_opc   := (others => '0');
    rxdata         := (others => '0');
    v.error        := '0';  -- Not used at the moment. This indicates the core error

    -- Read channel
    v.rd_error := '0';  -- Not used at the moment . TODO : Check thr RXdata flit
                       -- and take error bit


    -- Write channel
    v.wr_error := '0';

    -- Increment the link credits counter on lcrdv input signal for each channel
    if rnf_in.tx_lcrdv.req_lcrdv = '1' and conv_integer(r.req_lc_cnt) /= 15 then
      v.req_lc_cnt := r.req_lc_cnt + 1;
    end if;

    if rnf_in.tx_lcrdv.data_lcrdv = '1' and conv_integer(r.txd_lc_cnt) /= 15 then
      v.txd_lc_cnt := r.txd_lc_cnt + 1;
    end if;

    if rnf_in.tx_lcrdv.rsp_lcrdv = '1' and conv_integer(r.txrsp_lc_cnt) /= 15 then
      v.txrsp_lc_cnt := r.txrsp_lc_cnt + 1;
    end if;

    -- register to detect Negatiev Edge of data flit valid signal
    --v.rxd_flit_v   := rnf_in.rxdata.flit_v;
    -- register to detect Negatiev Edge of response flit valid signal
    --v.rxrsp_flit_v := rnf_in.rxrsp.flit_v;

    v.apb_err_flg := err_status;
    if err_status = '0' and r.apb_err_flg = '1' then
      v.apb_err_flg := '0';
    end if;

    --------------------------------------------------------------------
    --------------- State machine for snoop handling -------------------
    --------------------------------------------------------------------

    case r.snp_state is
      when idle =>
        -- Clear registers
        v.cl_dirty := '0';
        v.snp_dbid := (others => '0');
        v.snpdata  := (others => '0');
        -- assert the snp linkcredit valid signal for one pulse
        if r.snp_lcr_sts /= '1' then
          snp_lcrdv     := '1';
          v.snp_lcr_sts := '1';
        end if;
        -- Check if rnf_in.rxsnp.flit_v is '1' This indicates a valid snoop
        -- request coming from HNF to RNF
        if rnf_in.rxsnp.flit_v = '1' then
          -- extract the address from the rnf_in.rxsnp.flit and send a snoop
          -- request for that address to L2C-lite
          decode_snp(rnf_in.rxsnp.flit, snp_addr, snp_opc);
          snp_valid   := '1';
          v.snp_dbid  := (others => '0');  -- for snoop without datapull -- TODO
                                        -- add a check here
          v.snp_state := wait_l2_resp;
        end if;
        
      when wait_l2_resp =>
        -- Wait for the snoop response from the L2C-lite

        -- L2C-lite should assert c_snp_done signal only when the action
        -- demanded by snoop is performed in both L2 and also in L1. For
        -- example, for snoops with type 'invalidate', c_snp_done signal
        -- shall be asserted only when the invalidation of the snooped cache
        -- line in both L2 and also in L1. This signal must be asserted untill the dirty data and
        -- valid signal is sent out from L2C-lite
        if inp.c_snp_done = '1' then

          -- Register dirty data if present :
          -- If the cache line is dirty, L2C-lite must assert the following
          -- signals in a timely manner.
          -- cl_status_l2 : This signal indicates that the cache line snooped is
          -- dirty,clean or invalid. This signal must be asserted untill the dirty data and
          -- valid signal is sent out from L2C-lite
          -- cl_status_l2  "00" -> Indicates cache line is invalid
          -- cl_status_l2  "01" -> Not used at the moment. May be use for
          -- shared status in the future extensions. sends invalid resp anyway
          -- cl_status_l2  "10" -> Indicates cache line is clean
          -- cl_status_l2  "11" -> Indicates cache line is dirty          
          -- dirty_d_v   : Signal to indicate the presence of a valid dirty data
          -- snp_data_l2 : Dirty data from L2C-lite

          -- if invalid 
          if inp.cl_status_l2(1) = '0' then
            v.snpdata    := (others => '0');
            txrsp_flit_p := '1';
            v.snp_state  := rnf_snp_resp;
          else
            -- if cache line is valid and dirty
            if inp.cl_status_l2(0) = '1' then
              v.cl_dirty := '1';
              if inp.dirty_d_v = '1' then
                v.snpdata     := inp.snp_data_l2;
                txdata_flit_p := '1';
                txrsp_flit_p  := '1';
                v.snp_state   := rnf_snp_resp;
              end if;
            else -- if cache line is valid and clean
              v.snpdata    := (others => '0');
              txrsp_flit_p := '1';
              v.snp_state  := rnf_snp_resp;
            end if;
          end if;
          
        end if;

      when rnf_snp_resp =>
        -- Generate snoop response with or without data based on the cache line
        -- status reported by L2C-lite
        snp_resp_opc := conv_std_logic_vector(SnpResp, RSP_OPC_LN);
        if r.cl_dirty = '1' then
          -- create and send out data flit with dirty snoop data from L2C lite
          if conv_integer(r.txd_lc_cnt) /= 0 then
            -- Dirty data flit
            -- Set snoop response type as SNP_IPD to indicate dirty data.
            snp_rsp_type   := conv_std_logic_vector(SNP_IPD, D_RSP_LN);            
            snp_data_opc   := conv_std_logic_vector(SnpRespData, D_OPC_LN);
            txdata_flit    := create_data_flit(r.snpdata, snp_data_opc, r.snp_dbid, snp_rsp_type);
            snpdata_flit_v := '1';
            txdata_flit_v  := '1';
            v.txd_lc_cnt   := r.txd_lc_cnt - 1;

            -- snoop transaction completed
            v.snp_state    := idle;
            v.snp_lcr_sts  := '0';
          end if;

        else
          -- Set snoop response type as SNP_I to indicate that the cache line
          -- is clean. This logic is common for an invalid cache line as well
          snp_rsp_type := conv_std_logic_vector(SNP_I, RSP_RSP_LN);
          -- create and send out response flit with dirty snoop data from L2C lite
          if conv_integer(r.txrsp_lc_cnt) /= 0 then
            -- Snoop response flit
            txrsp_flit     := create_resp_flit(snp_resp_opc, r.snp_dbid, snp_rsp_type);
            snprsp_flit_v  := '1';
            txrsp_flit_v   := '1';
            v.txrsp_lc_cnt := r.txrsp_lc_cnt - 1;
            -- snoop transaction completed
            v.snp_state    := idle;
            v.snp_lcr_sts  := '0';
          end if;
        end if;

      when others =>
        v.snp_state   := idle;
        v.snp_lcr_sts := '0';
    end case;

    -----------------------------------------------------------------------------------
    --- RNF Controller state machine for regular reand and write transactions ---------
    -----------------------------------------------------------------------------------    

    case r.state is
      when idle =>
        -- Clearing registers
        v.txn_size := (others => '0');
        v.dbid     := (others => '0');
        if r.apb_err_flg = '0' then
          -- Latch the read access only when the read req and grant signals are high
          if inp.bmrd_req = '1' and r.rd_req_granted = '1' then
            -- READ miss in L2C
            v.rd_req_granted := '0';
            v.ongoing        := '1';
            v.tgt_addr       := inp.bmrd_addr;
            v.txn_size       := inp.bmrd_size;
            txreq_flit_p     := '1';
            v.state          := gen_rd_req;
          elsif inp.bmwr_req = '1' and r.wr_req_granted = '1' then  -- First beat of the write
              v.wr_req_granted := '0';
              v.ongoing        := '1';  -- TODO : Is this needed?
              v.tgt_addr       := inp.bmwr_addr;
              v.wr_bk_data     := inp.bmwr_data;
              v.txn_size       := inp.bmwr_size + 1;
              txreq_flit_p     := '1';
              v.state          := gen_wr_req;
          end if;
        else
          v.rd_req_granted := '0';
          v.wr_req_granted := '0';
          -- TODO error propogation
        end if;
        
      when gen_rd_req =>  -- TODO combine read and write req generation??
        -- Expects read address in r.tgt_addr
        -- Expects remaining size of data to be read in r.txn_size
        -- Creates a request flit for READ shared request
        -- Asserts request flit valid
        -- Asserts rx data link credit valid to allow NoC to send data to RNF
        -- Decrement request link credit valid counter        
        if conv_integer(r.req_lc_cnt) /= 0 then
          req_type     := conv_std_logic_vector(RDSHARED, RQ_OPC_LN);  -- Opcode : ReadShared
          find_size(r.txn_size, curr_size);  
          txreq_flit   := create_req_flit(r.tgt_addr, req_type, curr_size);
          txreq_flit_v := '1';
          v.req_lc_cnt := r.req_lc_cnt - 1;
          v.state      := wait_data;
          rxd_lcrdv    := '1';
        end if;

      when wait_data =>
        -- Waits for the data flit from the NoC
        -- Extracts the data and dbid from the data flit 
        -- Assert rd_valid to indicate the presence of valid data to L2C
        -- if all data is read, rd_done also should be asserted towards L2C
        if rnf_in.rxdata.flit_v = '1' then
          decode_data(rnf_in.rxdata.flit, v.dbid, rxdata);
          rd_valid := '1';
          -- TODO : check expcompack to see if a response should be generated
          -- read operation is completed
          rd_done := '1';          
          txrsp_flit_p := '1';
          v.state      := gen_resp;
        end if;

      when gen_wr_req =>
        -- Expects write address in r.tgt_addr
        -- Expects remaining size of data to be written in r.txn_size
        -- Creates a request flit for writeback full request
        -- Asserts request flit valid
        -- Asserts rx response link credit valid to allow NoC to respond to the writeback full request
        -- Decrement request link credit valid counter
        if conv_integer(r.req_lc_cnt) /= 0 then
          req_type     := conv_std_logic_vector(WRBACK_FULL, RQ_OPC_LN);  -- Opcode : WritebackFull
          find_size(r.txn_size, curr_size);
          txreq_flit   := create_req_flit(r.tgt_addr, req_type, curr_size);
          txreq_flit_v := '1';
          v.req_lc_cnt := r.req_lc_cnt - 1;
          rxrsp_lcrdv  := '1';
          v.state      := wait_resp;
        end if;
       
      when wait_resp =>
        -- Waits for the response flit from the NoC
        -- Decodes RSP filt to get response opcode and DBID(In the case of
        -- CompDBID response)
        -- Asserts txdata_flit_p as data flit will be sent out in the next cycle
        if rnf_in.rxrsp.flit_v = '1' then
          decode_resp(rnf_in.rxrsp.flit, rsp_opc, v.dbid);
          if conv_integer(rsp_opc) = CompDBIDResp then
              txdata_flit_p := '1';
              v.state       := gen_data;
          end if;
        end if;

      when gen_data =>
        -- Check if there is a snoop response with data is using the data
        -- channel. If yes, give priority to snoop response with data txn. Send
        -- the data flit in the next cycle, keep the txdata_flit_p asserted.
        if snpdata_flit_v = '1' then
          txdata_flit_p := '1';
        else                            -- Data channel is free to use
          -- Expects DBID in r.dbid
          -- Creates a data flit for with r.dbid as transaction ID
          -- Asserts data flit valid
          -- Decrements tx data link credit valid counter
          -- Expects the write data in r.wr_bk_data
          -- TODO : Add the resptype to show status of the data?
          if conv_integer(r.txd_lc_cnt) /= 0 then
            wr_data_opc   := conv_std_logic_vector(CopyBackWrData, D_OPC_LN);
            txdata_flit   := create_data_flit(r.wr_bk_data, wr_data_opc, r.dbid, (others => '0'));
            txdata_flit_v := '1';
            v.txd_lc_cnt  := r.txd_lc_cnt - 1;
            -- After the data is sent out,the writebackfull transaction is finished and move to idle.
            v.ongoing        := '0';
            v.state          := idle;
            v.wr_req_granted := '1';
            wr_done          := '1';               
          end if;       
        end if;
        
      when gen_resp =>
        -- Check if there is a snoop response is using the response
        -- channel. If yes, give priority to snoop response txn. Send
        -- the regular response flit in the next cycle, keep the txrsp_flit_p asserted.
        if snprsp_flit_v = '1' then
          txrsp_flit_p := '1';
        else  -- Response channel is free to use        
          -- Expects the dbid recieved from the HN in r.dbid and it needs to be
          -- used as transaction Id in the response flit.
          -- Asserts response flit valid
          -- Checks if the total size of data is handled. If not, go to
          -- gen_rd_req
          -- If yes, the read transaction is completed and goes back to idle
          if conv_integer(r.txrsp_lc_cnt) /= 0 then
            -- opcode 0x2 for Compack
            -- resp_type[2] is 0
            -- resp_type[1:0] is 0b001(SC) for Comp responses
            rsp_opc        := conv_std_logic_vector(COMPACK, RSP_OPC_LN);
            rsp_type       := conv_std_logic_vector(SHARED_CLEAN, RSP_RSP_LN);
            txrsp_flit     := create_resp_flit(rsp_opc, r.dbid, rsp_type);
            txrsp_flit_v   := '1';
            v.txrsp_lc_cnt := r.txrsp_lc_cnt - 1;
            -- read operation is completed 
            v.state          := idle;
            v.rd_req_granted := '1';
            v.ongoing        := '0';            
          end if;
        end if;
        
      when others =>
        v.rd_req_granted := '1';
        v.wr_req_granted := '1';
        v.ongoing        := '0';
        v.state          := idle;
        
    end case;

    ----------------------
    -- Signal update --
    ----------------------
    rin                         <= v;
    rnf_out.txreq.flit_p        <= txreq_flit_p;
    rnf_out.txreq.flit_v        <= txreq_flit_v;
    rnf_out.txreq.flit          <= txreq_flit;
    rnf_out.txdata.flit_v       <= txdata_flit_v;
    rnf_out.txdata.flit_p       <= txdata_flit_p;
    rnf_out.txdata.flit         <= txdata_flit;
    rnf_out.txrsp.flit_p        <= txrsp_flit_p;
    rnf_out.txrsp.flit_v        <= txrsp_flit_v;
    rnf_out.txrsp.flit          <= txrsp_flit;
    rnf_out.rx_lcrdv.rsp_lcrdv  <= rxrsp_lcrdv;
    rnf_out.rx_lcrdv.data_lcrdv <= rxd_lcrdv;
    rnf_out.rx_lcrdv.snp_lcrdv  <= snp_lcrdv;

    sts_out.error   <= r.error;
    sts_out.ongoing <= r.ongoing;


        -- READ SIGNALS
    bmrd_req_granted <= r.rd_req_granted;
    bmrd_data        <= rxdata;
    bmrd_valid       <= rd_valid;
    bmrd_done        <= rd_done;
    bmrd_error       <= r.rd_error;
    -- WRITE SIGNALS
    bmwr_full        <= wr_full;
    bmwr_done        <= wr_done;
    bmwr_error       <= r.wr_error;
    bmwr_req_granted <= r.wr_req_granted;

    -- Debug signals -- :TODO : remove later?
    qos        <= txreq_flit(3 downto 0);
    tgtid      <= txreq_flit(9 downto 4);
    srcid      <= txreq_flit(15 downto 10);
    txnid      <= txreq_flit(27 downto 16);
    return_nid <= txreq_flit(33 downto 28);
    stashid_v  <= txreq_flit(34);
    rtn_txnid  <= txreq_flit(46 downto 35);
    opc        <= txreq_flit(53 downto 47);
    size       <= txreq_flit(56 downto 54);
    addr       <= txreq_flit(88 downto 57);
    padding    <= txreq_flit(104 downto 89);
    ns         <= txreq_flit(105);
    nse        <= txreq_flit(106);
    lklysh     <= txreq_flit(107);
    allwretry  <= txreq_flit(108);
    order      <= txreq_flit(110 downto 109);
    pcrdtype   <= txreq_flit(114 downto 111);
    memattr    <= txreq_flit(118 downto 115);
    snpattr    <= txreq_flit(119);
    pgroupid   <= txreq_flit(127 downto 120);
    excl       <= txreq_flit(128);
    expcompack <= txreq_flit(129);
    tagop      <= txreq_flit(131 downto 130);
    tracetag   <= txreq_flit(132);

    -----------------------------------------
    -- SNOOP signal updates
    -----------------------------------------    
    l2_snp_addr  <= snp_addr;
    l2_snp_valid <= snp_valid;
  end process comb;

  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------  
  seq : process (clk, rstn)
  begin
    if (rstn = '0' and ASYNC_RST) then
      r <= CTRL_REG_RST;
    elsif rising_edge(clk) then
      if rstn = '0' or apb_ctrl.rst = '1' then
        r <= CTRL_REG_RST;
      else
        r <= rin;
      end if;
    end if;
  end process seq;
  

end architecture rtl;
