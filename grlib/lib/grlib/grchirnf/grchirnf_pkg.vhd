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
-- Package:     grchirnf_pkg
-- File:        grchirnf_pkg.vhd
-- Author:      Krishna K R - Frontgrade Gaisler AB
-- Description: Internal package for the grchirnf core types and functions
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
library grlib;
use grlib.amba.all;
use grlib.stdlib.all;
use grlib.devices.all;

package grchirnf_pkg is
  -- Constants
  constant HN_ID  : integer := 16;
  constant RNF_ID : integer := 0;

  -- Request types
  constant RDSHARED    : integer := 1;
  constant WRBACK_FULL : integer := 16#1B#;

  -- Response types
  constant SHARED_CLEAN : integer := 1;
  constant COMPACK      : integer := 2;
  constant SNP_I        : integer := 0;
  constant SNP_IPD      : integer := 4;

  -- Data channel opcodes
  constant CopyBackWrData : integer := 2;
  constant SnpRespData    : integer := 1;

  -- Response channel opcodes
  constant SnpResp      : integer := 1;
  constant CompDBIDResp : integer := 5;

  -- Snoop channel opcodes
  constant SnpCleanInvalid : integer := 9;

  constant QOS_W      : integer                  := 4;
  constant NID_W      : integer                  := 6;
  constant TXNID_W    : integer                  := 12;
  constant SNP_W      : integer                  := 24;
  constant ADDR_W     : integer                  := 32;
  constant CHI_ADDR   : integer                  := 48;
  constant ADDR_PAD   : integer                  := CHI_ADDR - ADDR_W;
  constant PCRDTYPE_W : integer                  := 4;
  constant TAGOP_W    : integer                  := 2;
  constant DATA_W     : integer range 128 to 512 := 512;

  -- Request flit fields
  constant RQ_QOS_LN      : integer := QOS_W;
  constant RQ_TGTID_LN    : integer := NID_W;
  constant RQ_SRCID_LN    : integer := NID_W;
  constant RQ_TXNID_LN    : integer := TXNID_W;
  constant RQ_RTNID_LN    : integer := NID_W;
  constant RQ_STSID_LN    : integer := 1;
  constant RQ_RTXNID_LN   : integer := TXNID_W;
  constant RQ_OPC_LN      : integer := 7;
  constant RQ_SIZE_LN     : integer := 3;
  constant RQ_ADDR_LN     : integer := CHI_ADDR;
  constant RQ_NS_LN       : integer := 1;
  constant RQ_NSE_LN      : integer := 1;
  constant RQ_LKLSH_LN    : integer := 1;
  constant RQ_RTRY_LN     : integer := 1;
  constant RQ_ORDR_LN     : integer := 2;
  constant RQ_PCRD_LN     : integer := PCRDTYPE_W;
  constant RQ_MATR_LN     : integer := 4;
  constant RQ_SNPATR_LN   : integer := 1;
  constant RQ_PGRP_LN     : integer := 8;
  constant RQ_EXCL_LN     : integer := 1;
  constant RQ_EXCMPACK_LN : integer := 1;
  constant RQ_TAGOP_LN    : integer := TAGOP_W;
  constant RQ_TRACTG_LN   : integer := 1;

  --constant REQFLT_W : integer := 133;
  constant REQFLT_W : integer := RQ_QOS_LN + RQ_TGTID_LN + RQ_SRCID_LN + RQ_TXNID_LN + RQ_RTNID_LN + RQ_STSID_LN + RQ_RTXNID_LN + RQ_OPC_LN + RQ_SIZE_LN + RQ_ADDR_LN + RQ_NS_LN + RQ_NSE_LN + RQ_LKLSH_LN + RQ_RTRY_LN + RQ_ORDR_LN + RQ_PCRD_LN + RQ_MATR_LN + RQ_SNPATR_LN + RQ_PGRP_LN + RQ_EXCL_LN + RQ_EXCMPACK_LN + RQ_TAGOP_LN + RQ_TRACTG_LN;

  -- Data flit fields
  constant D_QOS_LN    : integer := QOS_W;
  constant D_TGTID_LN  : integer := NID_W;
  constant D_SRCID_LN  : integer := NID_W;
  constant D_TXNID_LN  : integer := TXNID_W;
  constant D_HNID_LN   : integer := NID_W;
  constant D_OPC_LN    : integer := 4;
  constant D_RSPERR_LN : integer := 2;
  constant D_RSP_LN    : integer := 3;
  constant D_DSRC_LN   : integer := 5;
  constant D_CBSY_LN   : integer := 3;
  constant D_DBID_LN   : integer := TXNID_W;
  constant D_CCID_LN   : integer := 2;
  constant D_DID_LN    : integer := 2;
  constant D_TAGOP_LN  : integer := TAGOP_W;
  constant D_TAG_LN    : integer := (DATA_W/32);
  constant D_TU_LN     : integer := (DATA_W/128);
  constant D_TRACTG_LN : integer := 1;
  constant D_CAH_LN    : integer := 1;
  constant D_BE_LN     : integer := (DATA_W/8);
  constant D_DATA_LN   : integer := DATA_W;

  --constant DATAFLT_W : integer := 667;
  constant DATAFLT_W     : integer := D_QOS_LN + D_TGTID_LN + D_SRCID_LN + D_TXNID_LN + D_HNID_LN + D_OPC_LN + D_RSPERR_LN + D_RSP_LN + D_DSRC_LN + D_CBSY_LN + D_DBID_LN + D_CCID_LN + D_DID_LN + D_TAGOP_LN + D_TAG_LN + D_TU_LN + D_TRACTG_LN + D_CAH_LN + D_BE_LN + D_DATA_LN;
  -- Response flit fields
  constant RSP_QOS_LN    : integer := QOS_W;
  constant RSP_TGTID_LN  : integer := NID_W;
  constant RSP_SRCID_LN  : integer := NID_W;
  constant RSP_TXNID_LN  : integer := TXNID_W;
  constant RSP_OPC_LN    : integer := 5;
  constant RSP_RSPERR_LN : integer := 2;
  constant RSP_RSP_LN    : integer := 3;
  constant RSP_FWDS_LN   : integer := 3;
  constant RSP_CBSY_LN   : integer := 3;
  constant RSP_DBID_LN   : integer := TXNID_W;
  constant RSP_PCRD_LN   : integer := PCRDTYPE_W;
  constant RSP_TAGOP_LN  : integer := TAGOP_W;
  constant RSP_TRACTG_LN : integer := 1;

  --constant RESPFLT_W : integer := 63;
  constant RESPFLT_W : integer := RSP_QOS_LN + RSP_TGTID_LN + RSP_SRCID_LN + RSP_TXNID_LN + RSP_OPC_LN + RSP_RSPERR_LN + RSP_RSP_LN + RSP_FWDS_LN + RSP_CBSY_LN + RSP_DBID_LN + RSP_PCRD_LN + RSP_TAGOP_LN + RSP_TRACTG_LN;

  -- Snoop flit fields
  constant SNP_QOS_LN    : integer := QOS_W;
  constant SNP_SRCID_LN  : integer := NID_W;
  constant SNP_TXNID_LN  : integer := TXNID_W;
  constant SNP_FNID_LN   : integer := NID_W;
  constant SNP_FTXNID_LN : integer := TXNID_W;
  constant SNP_OPC_LN    : integer := 5;
  constant SNP_ADDR_LN   : integer := CHI_ADDR - 3; -- CTH NoC interface package uses 45 bits for snoop
                                                    -- addresses for partial
                                                    -- snoops(8 byte blocks.
                                                    -- byte level snoop is not
                                                    -- possible in CHI STD)
  constant SNP_NS_LN     : integer := 1;
  constant SNP_NSE_LN    : integer := 1;
  constant SNP_DNGSD_LN  : integer := 1;
  constant SNP_RSRC_LN   : integer := 1;
  constant SNP_TRACTG_LN : integer := 1;

  --constant SNPFLT_W : integer := 94;
  constant SNPFLT_W : integer := SNP_QOS_LN + SNP_SRCID_LN + SNP_TXNID_LN + SNP_FNID_LN + SNP_FTXNID_LN + SNP_OPC_LN + SNP_ADDR_LN + SNP_NS_LN + SNP_NSE_LN + SNP_DNGSD_LN + SNP_RSRC_LN + SNP_TRACTG_LN;

  constant max_size_l : integer := 512;
  -- Types

  type req_ch_type is record
    flit_p : std_ulogic;                             -- Flit pending
    flit_v : std_ulogic;                             -- Flit valid
    flit   : std_logic_vector(REQFLT_W-1 downto 0);  -- Flit 
  end record;

  constant REQ_RES : req_ch_type := (
    flit_p => '0',
    flit_v => '0',
    flit   => (others => '0')
    );

  type data_ch_type is record
    flit_p : std_ulogic;                              -- Flit pending
    flit_v : std_ulogic;                              -- Flit valid
    flit   : std_logic_vector(DATAFLT_W-1 downto 0);  -- Flit 
  end record;

  constant DATA_RES : data_ch_type := (
    flit_p => '0',
    flit_v => '0',
    flit   => (others => '0')
    );

  type resp_ch_type is record
    flit_p : std_ulogic;                              -- Flit pending
    flit_v : std_ulogic;                              -- Flit valid
    flit   : std_logic_vector(RESPFLT_W-1 downto 0);  -- Flit 
  end record;

  constant RESP_RES : resp_ch_type := (
    flit_p => '0',
    flit_v => '0',
    flit   => (others => '0')
    );

  type snoop_ch_type is record
    flit_p : std_ulogic;                     -- Flit pending
    flit_v : std_ulogic;                     -- Flit valid
    flit   : std_logic_vector(SNPFLT_W-1 downto 0);  -- Flit 
  end record;

  constant SNOOP_RES : snoop_ch_type := (
    flit_p => '0',
    flit_v => '0',
    flit   => (others => '0')
    );  

  type rx_lcrdv_type is record
    rsp_lcrdv  : std_ulogic;  -- Link credit valid output for RX response channel
    data_lcrdv : std_ulogic;  -- Link credit valid output for RX data channel
    snp_lcrdv  : std_ulogic;  -- Link credit valid output for snoop channel
  end record;

  constant RXLCRDV_RES : rx_lcrdv_type := (
    rsp_lcrdv  => '0',
    data_lcrdv => '0',
    snp_lcrdv  => '0'
    );

  type tx_lcrdv_type is record
    req_lcrdv  : std_ulogic;  -- Link credit valid input for TX response channel
    data_lcrdv : std_ulogic;  -- Link credit valid input for TX data channel
    rsp_lcrdv  : std_ulogic;  -- Link credit valid input for TX response channel
  end record;

  constant TXLCRDV_RES : tx_lcrdv_type := (
    req_lcrdv  => '0',
    data_lcrdv => '0',
    rsp_lcrdv  => '0'
    );

  type rnf_out_type is record
    txreq    : req_ch_type;             -- TX Request channel
    txdata   : data_ch_type;            -- TX data channel
    txrsp    : resp_ch_type;            -- TX response channel
    rx_lcrdv : rx_lcrdv_type;           -- Link credit valid signals sent out
  --txsactive : std_ulogic; -- TXSACTIVE signal
  --tx_ln_actreq : std_ulogic; -- TXLINKACTIVEREQ signal    
  end record;

  constant RNF_OUT_RST : rnf_out_type := (
    txreq    => REQ_RES,
    txdata   => DATA_RES,
    txrsp    => RESP_RES,
    rx_lcrdv => RXLCRDV_RES
   --txsactive  =>'0',
   --tx_ln_actreq => '0'
    );

  type rnf_in_type is record
    rxsnp    : snoop_ch_type;           -- Snoop channel
    rxdata   : data_ch_type;            -- RX data channel
    rxrsp    : resp_ch_type;            --RX response channel
    tx_lcrdv : tx_lcrdv_type;  -- Link credit valid signals recieved    
  --rxsactive : std_ulogic;  -- RX active signal
  --tx_ln_actack : std_ulogic -- TXLINKACTIVEACK signal
  end record;

  constant RNF_IN_RST : rnf_in_type := (
    rxsnp    => SNOOP_RES,
    rxdata   => DATA_RES,
    rxrsp    => RESP_RES,
    tx_lcrdv => TXLCRDV_RES
   --rxsactive  =>'0',
   --tx_ln_actack => '0'
    );

  -- Main APB registers for configuration of GRCHIRNF

  -- Control Register
  type grchirnf_ctrl_type is record
    rst : std_ulogic;
  end record;

  constant GRCHIRNF_CTRL_RST : grchirnf_ctrl_type := (
    rst => '0'
    );

  -- Status Register
  type grchirnf_sts_type is record
    ongoing : std_ulogic;
    error   : std_ulogic;
  end record;

  constant GRCHIRNF_STS_RST : grchirnf_sts_type := (
    ongoing => '0',
    error   => '0'
    );

  -- Capability Register - read only register
  -- Revision
  -- dbits

  type grchirnf_reg_type is record
    ctrl : grchirnf_ctrl_type;
    sts  : grchirnf_sts_type;
  end record;

  constant GRCHIRNF_REG_RST : grchirnf_reg_type := (
    ctrl => GRCHIRNF_CTRL_RST,
    sts  => GRCHIRNF_STS_RST
    );

  -- Register for communicating status from grchirnf_ctrl module to APB reg interface
  type ctrlmod_sts_type is record
    ongoing : std_ulogic;
    error   : std_ulogic;
  end record;

  constant CTRLMOD_STS_RST : ctrlmod_sts_type := (
    ongoing => '0',
    error   => '0'
    );

  -- Components

  -- GRCHIRNF APB interface
  component grchirnf_apb is
    generic (
      pindex : integer                      := 0;  --APB configuartion slave index
      paddr  : integer                      := 0;  -- APB configuartion slave address
      pmask  : integer                      := 16#FF8#;  -- APB configuartion slave mask
      pirq   : integer range 0 to NAHBIRQ-1 := 0;  -- APB configuartion slave irq
      dbits  : integer range 128 to 512     := 128  -- Data bus width allowed values : 128, 256 and 512 bits    

      );
    port (
      rstn         : in  std_ulogic;    -- Reset
      clk          : in  std_ulogic;    -- Clock
      sts_in       : in  ctrlmod_sts_type;  -- status signal from grchirnf_ctrl showing an ongoing transaction
      apbi         : in  apb_slv_in_type;   -- APB slave input
      apbo         : out apb_slv_out_type;  -- APB slave output
      apb_ctrl_out : out grchirnf_ctrl_type;  -- Control configuration signals
      err_status   : out std_ulogic
      );
  end component;

  -- GRCHIRNF control module
  component grchirnf_ctrl is
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
      -- READ SIGNALS
      bmrd_addr        : in  std_logic_vector(addr_width-1 downto 0);
      bmrd_size        : in  std_logic_vector(log2ext(max_size_l)-1 downto 0);  -- Max size 256
      bmrd_req         : in  std_logic;
      -- WRITE SIGNALS
      bmwr_addr        : in  std_logic_vector(addr_width-1 downto 0);
      bmwr_size        : in  std_logic_vector(log2ext(max_size_l)-1 downto 0);  -- Max size 256
      bmwr_req         : in  std_logic;
      bmwr_data        : in  std_logic_vector(dbits-1 downto 0);  --TODO FIXME : Max data width the L2c_lite support is 128 bit. 
      -- Signals to the l2c backend
      -- READ SIGNALS
      bmrd_req_granted : out std_logic;
      bmrd_data        : out std_logic_vector(dbits-1 downto 0);  --TODO FIXME : Max data width the L2c_lite support is 128 bit. 
      bmrd_valid       : out std_logic;
      bmrd_done        : out std_logic;
      bmrd_error       : out std_logic;
      -- WRITE SIGNALS
      bmwr_full        : out std_logic;
      bmwr_done        : out std_logic;
      bmwr_error       : out std_logic;
      bmwr_req_granted : out std_logic;
      -- Snoop signals between RNF and L2C-lite
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
  end component;

  -- GRCHIRNF core
  component grchirnf_core is
    generic (
      -- APB configuration  
      pindex     : integer                      := 0;  -- APB configuartion slave index
      paddr      : integer                      := 0;  -- APB configuartion slave address
      pmask      : integer                      := 16#FF8#;  -- APB configuartion slave mask
      pirq       : integer range 0 to NAHBIRQ-1 := 0;  -- APB configuartion slave irq
      -- Bus master configuration
      dbits      : integer range 128 to 512     := DATA_W;  --Data bus width allowed values : 128, 256 and 512 bits
      addr_width : integer range 32 to 52       := ADDR_W
      );
    port (
      rstn             : in  std_ulogic;   -- Reset
      clk              : in  std_ulogic;   -- Clock
      -- APB interface signals
      apbi             : in  apb_slv_in_type;          -- APB slave input
      apbo             : out apb_slv_out_type;         -- APB slave output
      -- Signals from the l2c backend
      -- READ SIGNALS
      bmrd_addr        : in  std_logic_vector(addr_width-1 downto 0);
      bmrd_size        : in  std_logic_vector(log2ext(max_size_l)-1 downto 0);  -- Max size 256
      bmrd_req         : in  std_logic;
      -- WRITE SIGNALS
      bmwr_addr        : in  std_logic_vector(addr_width-1 downto 0);
      bmwr_size        : in  std_logic_vector(log2ext(max_size_l)-1 downto 0);  -- Max size 256
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
      -- Snoop signals between RNF and L2C-lite
      c_snp_done       : in  std_logic;
      cl_status_l2     : in std_logic_vector(1 downto 0);
      dirty_d_v        : in  std_logic;
      snp_data_l2      : in  std_logic_vector(dbits-1 downto 0);
      l2_snp_valid     : out std_logic;
      l2_snp_addr      : out std_logic_vector(addr_width-1 downto 0);
      -- RNF signals towards HNF      
      rnf_in           : in  rnf_in_type;  -- Signals from home node
      rnf_out          : out rnf_out_type  -- Signals to the home node    
      );
  end component;

  -- Functions
  function create_req_flit(tgt_addr  : in std_logic_vector(ADDR_W-1 downto 0);
                           req_type  : in std_logic_vector(RQ_OPC_LN-1 downto 0);
                           data_size : in std_logic_vector(RQ_SIZE_LN-1 downto 0))
    return std_logic_vector;

  function create_resp_flit(opcode    : in std_logic_vector(RSP_OPC_LN-1 downto 0);
                            resp_dbid : in std_logic_vector(TXNID_W-1 downto 0);
                            resp_type : in std_logic_vector(RSP_RSP_LN-1 downto 0)
                            )
    return std_logic_vector;

  procedure decode_data(rxd_flit : in  std_logic_vector(DATAFLT_W-1 downto 0);
                     dbid_out    : out std_logic_vector(D_DBID_LN-1 downto 0);
                     data_out    : out std_logic_vector(D_DATA_LN-1 downto 0);
                     src_id_out  : out std_logic_vector(D_SRCID_LN-1 downto 0);
                     tgt_id_out  : out std_logic_vector(D_TGTID_LN-1 downto 0);
                     hn_id_out   : out std_logic_vector(D_HNID_LN-1 downto 0)
                     );

  procedure decode_data(rxd_flit : in  std_logic_vector(DATAFLT_W-1 downto 0);
                     dbid_out    : out std_logic_vector(D_DBID_LN-1 downto 0);
                     data_out    : out std_logic_vector(D_DATA_LN-1 downto 0)
                     );

  procedure find_size(total_size    : in  std_logic_vector(log2ext(max_size_l)-1 downto 0);
                      flit_size_out : out std_logic_vector(RQ_SIZE_LN-1 downto 0)
                      );

  procedure decode_resp(rsp_flit : in  std_logic_vector(RESPFLT_W-1 downto 0);
                     opc_out     : out std_logic_vector(RSP_OPC_LN-1 downto 0);
                     dbid_out    : out std_logic_vector(RSP_DBID_LN-1 downto 0)
                            );
  function create_data_flit(data_in   : in std_logic_vector(D_DATA_LN-1 downto 0);
                            opcode_in : in std_logic_vector(D_OPC_LN-1 downto 0);
                            txnid_in  : in std_logic_vector(D_DBID_LN-1 downto 0);
                            resp_type : in std_logic_vector(D_RSP_LN-1 downto 0)
                     ) return std_logic_vector;

  procedure decode_snp(snp_flit   : in  std_logic_vector(SNPFLT_W-1 downto 0);
                     snp_addr_out : out std_logic_vector(ADDR_W-1 downto 0);
                     snp_opc_out  : out std_logic_vector(SNP_OPC_LN-1 downto 0)
                            );  


end;

package body grchirnf_pkg is

--------------------- Subprograms ---------------------

  -- Function : create_req_flit
  -- Creates the request flit
  -- function create_req_flit is minimal at the moment. Most of the flit fields
  -- are hardcoded
  function create_req_flit(tgt_addr  : in std_logic_vector(ADDR_W-1 downto 0);
                           req_type  : in std_logic_vector(RQ_OPC_LN-1 downto 0);
                           data_size : in std_logic_vector(RQ_SIZE_LN-1 downto 0))
    return std_logic_vector is
    variable qos        : std_logic_vector(RQ_QOS_LN-1 downto 0);
    variable tgtid      : std_logic_vector(RQ_TGTID_LN-1 downto 0);
    variable srcid      : std_logic_vector(RQ_SRCID_LN-1 downto 0);
    variable txnid      : std_logic_vector(RQ_TXNID_LN-1 downto 0);
    variable return_nid : std_logic_vector(RQ_RTNID_LN-1 downto 0);
    variable stashid_v  : std_ulogic;
    variable rtn_txnid  : std_logic_vector(RQ_RTXNID_LN-1 downto 0);
    variable opc        : std_logic_vector(RQ_OPC_LN-1 downto 0);
    variable size       : std_logic_vector(RQ_SIZE_LN-1 downto 0);
    variable addr       : std_logic_vector(ADDR_W-1 downto 0);
    variable ns         : std_ulogic;
    variable nse        : std_ulogic;
    variable lklysh     : std_ulogic;
    variable allwretry  : std_ulogic;
    variable order      : std_logic_vector(RQ_ORDR_LN-1 downto 0);
    variable pcrdtype   : std_logic_vector(RQ_PCRD_LN-1 downto 0);
    variable memattr    : std_logic_vector(RQ_MATR_LN-1 downto 0);
    variable snpattr    : std_ulogic;
    variable pgroupid   : std_logic_vector(RQ_PGRP_LN-1 downto 0);
    variable excl       : std_ulogic;
    variable expcompack : std_ulogic;
    variable tagop      : std_logic_vector(RQ_TAGOP_LN-1 downto 0);
    variable tracetag   : std_ulogic;
    variable req_flit   : std_logic_vector(REQFLT_W-1 downto 0);
    variable padding    : std_logic_vector(ADDR_PAD-1 downto 0);
  --PBHA -- not used
  --MPAM -- not used
  --RSVC -- not used    
  begin
    qos        := (others => '0');
    tgtid      := conv_std_logic_vector(HN_ID, tgtid'length);  -- Home node HN-F
    srcid      := conv_std_logic_vector(RNF_ID, srcid'length);  -- Request node RN-F
    txnid      := conv_std_logic_vector(1, txnid'length);
    return_nid := (others => '0');
    stashid_v  := '0';
    rtn_txnid  := (others => '0');
    opc        := req_type;
    size       := data_size;
    addr       := tgt_addr;
    ns         := '0';
    nse        := '0';
    lklysh     := '0';
    allwretry  := '0';
    order      := (others => '0');
    pcrdtype   := (others => '0');
    memattr    := (others => '0');
    snpattr    := '0';
    pgroupid   := (others => '0');
    excl       := '0';
    expcompack := '1';
    tagop      := (others => '0');
    tracetag   := '0';
    padding    := (others => '0');

    req_flit := tracetag & tagop & expcompack & excl & pgroupid & snpattr & memattr & pcrdtype
                & order & allwretry & lklysh & nse & ns & padding & addr & size & opc & rtn_txnid
                & stashid_v & return_nid & txnid & srcid & tgtid & qos;
    return req_flit;
  end function;

  -- Procedure : decode_data
  -- Extract data, source ID, target ID and home node ID from the incoming data flit
  -- The function can be extended to check the Data Check field, Byte Enable
  -- field and Poison field, before extracting data. At the moment it blindly
  -- takes the data, dbid and returns it
  -- Assuming that the data is in little endian. This can be taken as an
  -- argument to the procedure
  procedure decode_data(rxd_flit : in  std_logic_vector(DATAFLT_W-1 downto 0);
                     dbid_out    : out std_logic_vector(D_DBID_LN-1 downto 0);
                     data_out    : out std_logic_vector(D_DATA_LN-1 downto 0);
                     src_id_out  : out std_logic_vector(D_SRCID_LN-1 downto 0);
                     tgt_id_out  : out std_logic_vector(D_TGTID_LN-1 downto 0);
                     hn_id_out   : out std_logic_vector(D_HNID_LN-1 downto 0)
                     ) is
    --variable qos : std_logic_vector(QOS_W-1 downto 0); 
    --variable tgtid : std_logic_vector(NID_W-1 downto 0);
    --variable srcid : std_logic_vector(NID_W-1 downto 0);
    --variable txnid : std_logic_vector(TXNID_W-1 downto 0);
    --variable hnid  : std_logic_vector(NID_W-1 downto 0);
    --variable opcode : std_logic_vector(D_OPC_LN-1 downto 0);
    --variable resperr  : std_logic_vector(D_RSPERR_LN-1 downto 0);
    --variable resp : std_logic_vector(D_RSP_LN-1 downto 0);
    --variable data_src : std_logic_vector(D_DSRC_LN-1 downto 0);
    --variable cbusy : std_logic_vector(D_CBSY_LN-1 downto 0);
    --variable dbid := std_logic_vector(D_DBID_LN-1 downto 0);
    --variable ccid :  std_logic_vector(D_CCID_LN-1 downto 0);
    --variable dataid :  std_logic_vector(D_DID_LN downto 0);
    --variable tag_op   : std_logic_vector(D_TAGOP_LN-1 downto 0);
    --variable tag   : std_logic_vector(D_TAG_LN-1 downto 0);
    --variable tu   : std_logic_vector(D_TU_LN-1 downto 0);
    --variable tracetag : std_ulogic;
    --variable cah : std_ulogic;     
    ----RSVCD -- not used
    --variable be : std_logic_vector((DATA_W)/8)-1 downto 0);
    --variable data : std_logic_vector(DATA_W)-1 downto 0);    
    --datacheck -- not used
    --poison  -- not used
    variable data_lsb  : integer;
    variable data_msb  : integer;
    variable dbid_lsb  : integer;
    variable dbid_msb  : integer;
    variable srcid_lsb : integer;
    variable srcid_msb : integer;
    variable tgtid_lsb : integer;
    variable tgtid_msb : integer;
    variable hnid_lsb  : integer;
    variable hnid_msb  : integer;
  begin
    tgtid_lsb := D_QOS_LN;
    tgtid_msb := D_QOS_LN + D_TGTID_LN -1;
    srcid_lsb := D_QOS_LN + D_TGTID_LN;
    srcid_msb := srcid_lsb + D_SRCID_LN - 1;
    hnid_lsb  := D_QOS_LN + D_TGTID_LN + D_SRCID_LN + D_TXNID_LN;
    hnid_msb  := hnid_lsb + D_HNID_LN -1;
    dbid_lsb  := D_QOS_LN + D_TGTID_LN + D_SRCID_LN + D_TXNID_LN + D_HNID_LN + D_OPC_LN + D_RSPERR_LN + D_RSP_LN + D_DSRC_LN + D_CBSY_LN;
    dbid_msb  := dbid_lsb + D_DBID_LN -1;
    data_lsb  := DATAFLT_W - D_DATA_LN;
    data_msb  := DATAFLT_W -1;

    data_out   := rxd_flit(data_msb downto data_lsb);
    dbid_out   := rxd_flit(dbid_msb downto dbid_lsb);
    src_id_out := rxd_flit(srcid_msb downto srcid_lsb);
    tgt_id_out := rxd_flit(tgtid_msb downto tgtid_lsb);
    hn_id_out  := rxd_flit(hnid_msb downto hnid_lsb);
  end procedure;

  -- Procedure : decode_data
  -- Extract data from the incoming data flit
  -- The function can be extended to check the Data Check field, Byte Enable
  -- field and Poison field, before extracting data. At the moment it blindly
  -- takes the data, dbid and returns it
  -- Assuming that the data is in little endian. This can be taken as an
  -- argument to the procedure
  procedure decode_data(rxd_flit : in  std_logic_vector(DATAFLT_W-1 downto 0);
                     dbid_out    : out std_logic_vector(D_DBID_LN-1 downto 0);
                     data_out    : out std_logic_vector(D_DATA_LN-1 downto 0)
                     ) is
    variable data_lsb : integer;
    variable data_msb : integer;
    variable dbid_lsb : integer;
    variable dbid_msb : integer;
  begin
    dbid_lsb := D_QOS_LN + D_TGTID_LN + D_SRCID_LN + D_TXNID_LN + D_HNID_LN + D_OPC_LN + D_RSPERR_LN + D_RSP_LN + D_DSRC_LN + D_CBSY_LN;
    dbid_msb := dbid_lsb + D_DBID_LN -1;
    data_lsb := DATAFLT_W - D_DATA_LN;
    data_msb := DATAFLT_W -1;

    data_out := rxd_flit(data_msb downto data_lsb);
    dbid_out := rxd_flit(dbid_msb downto dbid_lsb);
  end procedure;

  -- Function : create_resp_flit
  -- Creates the response flit
  -- function create_resp_flit is minimal at the moment. Most of the flit fields
  -- are hardcoded
  function create_resp_flit(opcode    : in std_logic_vector(RSP_OPC_LN-1 downto 0);
                            resp_dbid : in std_logic_vector(TXNID_W-1 downto 0);
                            resp_type : in std_logic_vector(RSP_RSP_LN-1 downto 0)
                            )
    return std_logic_vector is
    variable qos       : std_logic_vector(RSP_QOS_LN-1 downto 0);
    variable tgtid     : std_logic_vector(RSP_TGTID_LN-1 downto 0);
    variable srcid     : std_logic_vector(RSP_SRCID_LN-1 downto 0);
    variable txnid     : std_logic_vector(RSP_TXNID_LN-1 downto 0);
    variable opc       : std_logic_vector(RSP_OPC_LN-1 downto 0);
    variable resp_err  : std_logic_vector(RSP_RSPERR_LN-1 downto 0);
    variable resp      : std_logic_vector(RSP_RSP_LN-1 downto 0);
    variable fwd_state : std_logic_vector(RSP_FWDS_LN-1 downto 0);
    variable cbusy     : std_logic_vector(RSP_CBSY_LN-1 downto 0);
    variable dbid      : std_logic_vector(RSP_DBID_LN-1 downto 0);
    variable pcrdtype  : std_logic_vector(PCRDTYPE_W-1 downto 0);
    variable tagop     : std_logic_vector(RSP_TAGOP_LN-1 downto 0);
    variable tracetag  : std_ulogic;
    variable resp_flit : std_logic_vector(RESPFLT_W-1 downto 0);
  begin
    qos       := (others => '0');
    tgtid     := conv_std_logic_vector(HN_ID, tgtid'length);  -- Home node HN-F
    srcid     := conv_std_logic_vector(RNF_ID, srcid'length);  -- Request node RN-F
    txnid     := resp_dbid;  -- The transaction ID in a read request response is the
    -- DBID recieved in the COMPDATA
    opc       := opcode;
    resp_err  := (others => '0');
    resp      := resp_type;
    fwd_state := (others => '0');
    cbusy     := (others => '0');
    dbid      := (others => '0');
    pcrdtype  := (others => '0');
    tagop     := (others => '0');
    tracetag  := '0';

    resp_flit := tracetag & tagop & pcrdtype & dbid & cbusy & fwd_state & resp & resp_err & opc & txnid & srcid & tgtid & qos;
    return resp_flit;
  end function;

  -- Procedure : find_size
  -- Procedure to find the current transaction size. CHI allows the maximum data
  -- that can be transferred in a filt to 64 bytes. If a read or write
  -- transaction has more than 64 bytes, multiple flits are needed to be
  -- sent out for handling the entire size of data. Also the size is encoded as
  -- log2ext(actual size) in the CHI flit
  procedure find_size(total_size    : in  std_logic_vector(log2ext(max_size_l)-1 downto 0);
                      flit_size_out : out std_logic_vector(RQ_SIZE_LN-1 downto 0)
                      ) is
    variable temp       : integer;
    variable burst_size : std_logic_vector(10 downto 0);
  begin
    -- GSL L2C valid line size values are 32 bytes or 64 bytes only
    if conv_integer(total_size) = 64 then
      flit_size_out := conv_std_logic_vector(6, RQ_SIZE_LN);  --"110" is 64 bytes      
    else
      flit_size_out := conv_std_logic_vector(5, RQ_SIZE_LN);  --"101" is 32 bytes 
    end if;    
  end procedure find_size;

  -- Procedure : decode_resp
  -- Procedure to extract the response opcode from the response flit  
  procedure decode_resp(rsp_flit : in  std_logic_vector(RESPFLT_W-1 downto 0);
                     opc_out     : out std_logic_vector(RSP_OPC_LN-1 downto 0);
                     dbid_out    : out std_logic_vector(RSP_DBID_LN-1 downto 0)
                            ) is
    --variable qos       : std_logic_vector(RSP_QOS_LN-1 downto 0);
    --variable tgtid     : std_logic_vector(RSP_TGTID_LN-1 downto 0);
    --variable srcid     : std_logic_vector(RSP_SRCID_LN-1 downto 0);
    --variable txnid     : std_logic_vector(RSP_TXNID_LN-1 downto 0);
    --variable opc       : std_logic_vector(RSP_OPC_LN-1 downto 0);
    --variable resp_err  : std_logic_vector(RSP_RSPERR_LN-1 downto 0);
    --variable resp      : std_logic_vector(RSP_RSP_LN-1 downto 0);
    --variable fwd_state : std_logic_vector(RSP_FWDS_LN-1 downto 0);
    --variable cbusy     : std_logic_vector(RSP_CBSY_LN-1 downto 0);
    --variable dbid      : std_logic_vector(RSP_DBID_LN-1 downto 0);
    --variable pcrdtype  : std_logic_vector(PCRDTYPE_W-1 downto 0);
    --variable tagop     : std_logic_vector(RSP_TAGOP_LN-1 downto 0);
    --variable tracetag  : std_ulogic;
    --variable resp_flit : std_logic_vector(RESPFLT_W-1 downto 0);
    variable opc_lsb  : integer;
    variable opc_msb  : integer;
    variable dbid_lsb : integer;
    variable dbid_msb : integer;
  begin
    opc_lsb  := RSP_QOS_LN + RSP_TGTID_LN + RSP_SRCID_LN + RSP_TXNID_LN;
    opc_msb  := opc_lsb + RSP_OPC_LN - 1;
    dbid_lsb := RSP_QOS_LN + RSP_TGTID_LN + RSP_SRCID_LN + RSP_TXNID_LN + RSP_OPC_LN + RSP_RSPERR_LN + RSP_RSP_LN + RSP_FWDS_LN + RSP_CBSY_LN;
    dbid_msb := dbid_lsb + RSP_DBID_LN -1;
    opc_out  := rsp_flit(opc_msb downto opc_lsb);
    dbid_out := rsp_flit(dbid_msb downto dbid_lsb);
  end procedure;

  -- function : create_data_flit
  -- Create the data flit with the data and the DBID recieved from the homenode
  -- as the transaction ID
  -- For snoop transactions the DBID/TXNID is not relevant but the resp type is.
  function create_data_flit(data_in   : in std_logic_vector(D_DATA_LN-1 downto 0);
                            opcode_in : in std_logic_vector(D_OPC_LN-1 downto 0);
                            txnid_in  : in std_logic_vector(D_DBID_LN-1 downto 0);
                            resp_type : in std_logic_vector(D_RSP_LN-1 downto 0)
                     ) return std_logic_vector is
    variable qos       : std_logic_vector(QOS_W-1 downto 0);
    variable tgtid     : std_logic_vector(NID_W-1 downto 0);
    variable srcid     : std_logic_vector(NID_W-1 downto 0);
    variable txnid     : std_logic_vector(TXNID_W-1 downto 0);
    variable hnid      : std_logic_vector(NID_W-1 downto 0);
    variable opcode    : std_logic_vector(D_OPC_LN-1 downto 0);
    variable resperr   : std_logic_vector(D_RSPERR_LN-1 downto 0);
    variable resp      : std_logic_vector(D_RSP_LN-1 downto 0);
    variable data_src  : std_logic_vector(D_DSRC_LN-1 downto 0);
    variable cbusy     : std_logic_vector(D_CBSY_LN-1 downto 0);
    variable dbid      : std_logic_vector(D_DBID_LN-1 downto 0);
    variable ccid      : std_logic_vector(D_CCID_LN-1 downto 0);
    variable dataid    : std_logic_vector(D_DID_LN downto 0);
    variable tag_op    : std_logic_vector(D_TAGOP_LN-1 downto 0);
    variable tag       : std_logic_vector(D_TAG_LN-1 downto 0);
    variable tu        : std_logic_vector(D_TU_LN-1-1 downto 0);
    variable tracetag  : std_ulogic;
    variable cah       : std_ulogic;
    --RSVCD -- not used
    variable be        : std_logic_vector(D_BE_LN-1 downto 0);
    variable data      : std_logic_vector(D_DATA_LN-1 downto 0);
    --datacheck -- not used
    --poison  -- not used
    variable data_flit : std_logic_vector(DATAFLT_W-1 downto 0);
  begin
    qos      := (others => '0');
    tgtid    := conv_std_logic_vector(HN_ID, tgtid'length);   -- Home node HN-F
    srcid    := conv_std_logic_vector(RNF_ID, srcid'length);  -- Request node RN-F
    txnid    := txnid_in;  -- This is the DBID recieved from the home node 
    hnid     := conv_std_logic_vector(HN_ID, hnid'length);    -- Home node HN-F
    opcode   := opcode_in;
    resperr  := (others => '0');
    resp     := resp_type;
    data_src := (others => '0');
    cbusy    := (others => '0');
    dbid     := (others => '0');
    ccid     := (others => '0');
    dataid   := (others => '0');
    tag_op   := (others => '0');
    tag      := (others => '0');
    tu       := (others => '0');
    tracetag := '0';
    cah      := '0';
    be       := (others => '0');
    data     := data_in;

    data_flit := data & be & cah & tracetag & tu & tag & tag_op & dataid & ccid & dbid & cbusy & data_src & resp & resperr & opcode & hnid & txnid & srcid & tgtid & qos;
    return data_flit;
  end function;

  -- Procedure : decode_snp
  -- Extract Snoop address, snoop opcode,
  -- and snoop opcode
  procedure decode_snp(snp_flit   : in  std_logic_vector(SNPFLT_W-1 downto 0);
                     snp_addr_out : out std_logic_vector(ADDR_W-1 downto 0);
                     snp_opc_out  : out std_logic_vector(SNP_OPC_LN-1 downto 0)
                     ) is
    --variable qos : std_logic_vector(QOS_W-1 downto 0); 
    --variable srcid : std_logic_vector(NID_W-1 downto 0);
    --variable txnid : std_logic_vector(TXNID_W-1 downto 0);
    --variable fnid  : std_logic_vector(NID_W-1 downto 0);
    --variable ftxnid : std_logic_vector(TXNID_W-1 downto 0);    
    --variable opcode : std_logic_vector(SNP_OPC_LN-1 downto 0);
    --variable addr  : std_logic_vector(SNP_ADDR_LN-1 downto 0);
    --variable ns : std_ulogic;
    --variable nse : std_ulogic;
    --variable dngsd : std_ulogic;
    --variable rsrc := std_ulogic;
    --variable tracetag : std_ulogic;

    variable addr_lsb : integer;
    variable addr_msb : integer;
    variable opc_lsb  : integer;
    variable opc_msb  : integer;
    variable addr_tmp : std_logic_vector(SNP_ADDR_LN-1 downto 0);
    variable addr_tmp1 : std_logic_vector(CHI_ADDR-1 downto 0);    
  begin
    addr_lsb := SNP_QOS_LN + SNP_SRCID_LN + SNP_TXNID_LN + SNP_FNID_LN + SNP_FTXNID_LN + SNP_OPC_LN;
    addr_msb := addr_lsb + SNP_ADDR_LN -1;
    opc_lsb  := SNP_QOS_LN + SNP_SRCID_LN + SNP_TXNID_LN + SNP_FNID_LN + SNP_FTXNID_LN;
    opc_msb  := opc_lsb + SNP_OPC_LN - 1;

    addr_tmp := snp_flit(addr_msb downto addr_lsb);
    addr_tmp1 := addr_tmp(SNP_ADDR_LN-1 downto 3) & "000000";
    snp_addr_out := addr_tmp1(ADDR_W-1 downto 0);
    snp_opc_out  := snp_flit(opc_msb downto opc_lsb);
  end procedure;
  
end;


