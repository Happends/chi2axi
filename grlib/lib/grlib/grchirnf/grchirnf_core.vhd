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
-- Entity:      grchirnf_core
-- File:        grchirnf_core.vhd
-- Author:      Krishna K R - Cobham Gaisler AB
-- Description: GRCHIRNF core entity.
------------------------------------------------------------------------------ 
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library grlib;
use grlib.stdlib.all;
use grlib.amba.all;
use grlib.config_types.all;
use grlib.config.all;
use grlib.grchirnf_pkg.all;


-----------------------------------------------------------------------------
-- GRCHIRNF core
-- This is the core layer which integrates GRCHIRNF modules like grchirnf_ctrl and grchirnf_apb.
-----------------------------------------------------------------------------

entity grchirnf_core is
  generic (
    -- APB configuration  
    pindex     : integer                      := 0;  -- APB configuartion slave index
    paddr      : integer                      := 0;  -- APB configuartion slave address
    pmask      : integer                      := 16#FF8#;  -- APB configuartion slave mask
    pirq       : integer range 0 to NAHBIRQ-1 := 0;  -- APB configuartion slave irq
    -- Bus master configuration
    dbits      : integer range 128 to 512     := DATA_W;  --Data bus width allowed values : 128, 256 and 512 bits
    addr_width : integer range 32 to 52       := ADDR_W);    
  port (
    rstn             : in  std_ulogic;  -- Reset
    clk              : in  std_ulogic;  -- Clock
    --l2c_miss : in std_ulogic;
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
    cl_status_l2     : in std_logic_vector(1 downto 0);
    dirty_d_v        : in  std_logic;
    snp_data_l2      : in  std_logic_vector(dbits-1 downto 0);
    l2_snp_valid     : out std_logic;
    l2_snp_addr      : out std_logic_vector(addr_width-1 downto 0);
    -- RNF signals towards HNF    
    rnf_in           : in  rnf_in_type;  -- Signals from home node
    rnf_out          : out rnf_out_type  -- Signals to the home node    
  );
end entity grchirnf_core;

------------------------------------------------------------------------------
-- Architecture of grchirnf
------------------------------------------------------------------------------

architecture rtl of grchirnf_core is
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";
  -- Constant for bit - byte manipulation

  -----------------------------------------------------------------------------
  -- Records and types
  -----------------------------------------------------------------------------


  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------  
  signal ctrl_mod_sts   : ctrlmod_sts_type;
  signal apb_ctrl_reg   : grchirnf_ctrl_type;
  signal apb_err_status : std_ulogic;

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin  -- rtl

  -----------------
  -- Assignments --
  -----------------
  -----------------------------------------------------------------------------
  -- Signal assignments
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

  -- APB interface
  apb : grchirnf_apb
    generic map (
      pindex => pindex,
      paddr  => paddr,
      pmask  => pmask,
      pirq   => pirq,
      dbits  => dbits)
    port map (
      rstn         => rstn,
      clk          => clk,
      sts_in       => ctrl_mod_sts,
      apbi         => apbi,
      apbo         => apbo,
      apb_ctrl_out => apb_ctrl_reg,
      err_status   => apb_err_status
      );

  -- Control module
  ctrl : grchirnf_ctrl
    generic map (
      dbits => dbits
      )  
    port map (
      rstn             => rstn,
      clk              => clk,
      --c_miss       => l2c_miss,
      sts_out          => ctrl_mod_sts,
      apb_ctrl         => apb_ctrl_reg,
      err_status       => apb_err_status,
      -- Signals from the l2c backend
      -- READ SIGNALS
      bmrd_addr        => bmrd_addr,
      bmrd_size        => bmrd_size,
      bmrd_req         => bmrd_req,
      -- WRITE SIGNALS
      bmwr_addr        => bmwr_addr,
      bmwr_size        => bmwr_size,
      bmwr_req         => bmwr_req,
      bmwr_data        => bmwr_data,
      -- Signals to the l2c backend
      -- READ SIGNALS
      bmrd_req_granted => bmrd_req_granted,
      bmrd_data        => bmrd_data,
      bmrd_valid       => bmrd_valid,
      bmrd_done        => bmrd_done,
      bmrd_error       => bmrd_error,
      -- WRITE SIGNALS
      bmwr_full        => bmwr_full,
      bmwr_done        => bmwr_done,
      bmwr_error       => bmwr_error,
      bmwr_req_granted => bmwr_req_granted,
      -- Snoop signals between RNF and L2C-lite
      c_snp_done       => c_snp_done,
      cl_status_l2      => cl_status_l2,
      dirty_d_v        => dirty_d_v,
      snp_data_l2      => snp_data_l2,
      l2_snp_valid     => l2_snp_valid,
      l2_snp_addr      => l2_snp_addr,
      -- RNF signals towards HNF
      rnf_in           => rnf_in,
      rnf_out          => rnf_out
      );  

end architecture rtl;
