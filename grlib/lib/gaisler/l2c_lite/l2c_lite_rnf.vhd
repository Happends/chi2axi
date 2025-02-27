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
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library gaisler;
use gaisler.l2c_lite.all;

library grlib;
use grlib.stdlib.all;
use grlib.amba.all;
use grlib.devices.all;
use grlib.grchirnf_pkg.all;

library techmap;
use techmap.gencomp.all;

entity l2c_lite_rnf is
    generic (
        tech        : integer                      := 0;
        pindex      : integer                      := 0;  -- APB configuartion slave index
        paddr       : integer                      := 0;  -- APB configuartion slave address
        pmask       : integer                      := 16#FF8#;  -- APB configuartion slave mask
        pirq        : integer range 0 to NAHBIRQ-1 := 0;  -- APB configuartion slave irq
        hsindex     : integer                      := 0;
        inv_hmindex : integer                      := 0;
        ways        : integer                      := 2;
        waysize     : integer                      := 64;
        linesize    : integer                      := 64;
        repl        : integer                      := 0;
        haddr       : integer                      := 16#000#;
        hmask       : integer                      := 16#FFF#;
        ioaddr      : integer                      := 16#000#;
        bioaddr     : integer                      := 16#000#;
        biomask     : integer                      := 16#FFF#;
        cached      : integer                      := 16#FFFF#;
        incl        : integer                      := 0;
        be_dw       : integer range 128 to 512     := DATA_W;
        addr_width  : integer range 32 to 52       := ADDR_W);
    port (
        rstn : in std_ulogic;
        clk  : in std_ulogic;

        -- TODO : May need to remove the APB interface. TBD after internal discussion
        apbi : in  apb_slv_in_type;     -- APB slave input
        apbo : out apb_slv_out_type;    -- APB slave output

        ---- CACHE FRONTEND ----
        ahbsi : in  ahb_slv_in_type;
        ahbso : out ahb_slv_out_type;

        ---- INVAL INTERFACE
        inv_ahbmi : in  ahb_mst_in_type;
        inv_ahbmo : out ahb_mst_out_type;

        ---- CACHE BACKEND ----
        rnf_in  : in  rnf_in_type;      -- Signals from home node
        rnf_out : out rnf_out_type      -- Signals to the home node

    );

end entity l2c_lite_rnf;

architecture rtl of l2c_lite_rnf is

  signal bmrd_req_granted : std_logic;
  signal bmrd_data        : std_logic_vector(be_dw - 1 downto 0);
  signal bmrd_valid       : std_logic;
  signal bmrd_done        : std_logic;
  signal bmrd_error       : std_logic;
  signal bmwr_full        : std_logic;
  signal bmwr_done        : std_logic;
  signal bmwr_error       : std_logic;
  signal bmwr_req_granted : std_logic;
  signal bmrd_addr        : std_logic_vector(addr_width - 1 downto 0);
  signal bmrd_size        : std_logic_vector(log2ext(max_size) - 1 downto 0);
  signal bmrd_req         : std_logic;
  signal bmwr_addr        : std_logic_vector(addr_width - 1 downto 0);
  signal bmwr_size        : std_logic_vector(log2ext(max_size) - 1 downto 0);
  signal bmwr_req         : std_logic;
  signal bmwr_data        : std_logic_vector(be_dw - 1 downto 0);
  signal c_snp_done       : std_logic;
  signal cl_status_l2     : std_logic_vector(1 downto 0);
  signal dirty_d_v        : std_logic;
  signal snp_data_l2      : std_logic_vector(be_dw - 1 downto 0);
  signal l2_snp_valid     : std_logic;
  signal l2_snp_addr      : std_logic_vector(addr_width-1 downto 0);

  signal endian_out : std_ulogic;
  signal excl_err   : std_logic_vector(1 downto 0);
  signal excl_done  : std_ulogic;

begin

  ctrl : l2c_lite_core
    generic map(
      tech       => tech,
      hsindex    => hsindex,
      indexinv   => indexinv,
      haddr      => haddr,
      hmask      => hmask,
      ioaddr     => ioaddr,
      bioaddr    => bioaddr,
      biomask    => biomask,
      waysize    => waysize,
      linesize   => linesize,
      cached     => cached,
      repl       => repl,
      ways       => ways,
      incl       => incl,
      bm_dw_l    => be_dw,
      addr_width => addr_width)
    port map(
      rstn             => rstn,
      clk              => clk,
      ahbsi            => ahbsi,
      ahbso            => ahbso,
      ahbmi            => inv_ahbmi,
      ahbmo            => inv_ahbmo,
      --Bus master domain signals
      --Read Channel
      bmrd_addr        => bmrd_addr,
      bmrd_size        => bmrd_size,
      bmrd_req         => bmrd_req,
      bmrd_req_granted => bmrd_req_granted,
      bmrd_data        => bmrd_data,
      bmrd_valid       => bmrd_valid,
      bmrd_done        => bmrd_done,
      bmrd_error       => bmrd_error,
       --Write Channel
      bmwr_addr        => bmwr_addr,
      bmwr_size        => bmwr_size,
      bmwr_req         => bmwr_req,
      bmwr_req_granted => bmwr_req_granted,
      bmwr_data        => bmwr_data,
      bmwr_full        => bmwr_full,
      bmwr_done        => bmwr_done,
      bmwr_error       => bmwr_error,
       -- snoop signals between l2c_lite and RNF
      l2_snp_valid     => l2_snp_valid,
      l2_snp_addr      => l2_snp_addr,
      c_snp_done       => c_snp_done,
      cl_status_l2     => cl_status_l2,
      dirty_d_v        => dirty_d_v,
      snp_data_l2      => snp_data_l2);

  rnf_core : grchirnf_core
    generic map (
      pindex     => pindex,
      paddr      => paddr,
      pmask      => pmask,
      pirq       => pirq,
      dbits      => be_dw,
      addr_width => addr_width)
    port map (
      rstn             => rstn,
      clk              => clk,
      apbi             => apbi,
      apbo             => apbo,
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
      cl_status_l2     => cl_status_l2,
      dirty_d_v        => dirty_d_v,
      snp_data_l2      => snp_data_l2,
      l2_snp_valid     => l2_snp_valid,
      l2_snp_addr      => l2_snp_addr,
      -- RNF signals towards HNF
      rnf_in           => rnf_in,
      rnf_out          => rnf_out);

    assert not ((ways mod 2 /= 0) and (waysize = 1))
    report "L2 Cache configuration error: pLRU replacement policy requires ways to be power of 2."
        severity failure;

end architecture rtl;
