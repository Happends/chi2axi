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
-- Entity:      grchirnf_apb
-- File:        grchirnf_apb.vhd
-- Author:      Krishna K R - Frontgrade Gaisler AB
-- Description: APB register interface for GRCHIRNF.
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
-- Entity to read and write APB registers for GRCHIRNF.
-----------------------------------------------------------------------------

entity grchirnf_apb is
  generic (
    pindex : integer                      := 0;  --APB configuartion slave index
    paddr  : integer                      := 0;  -- APB configuartion slave address
    pmask  : integer                      := 16#FF8#;  -- APB configuartion slave mask
    pirq   : integer range 0 to NAHBIRQ-1 := 0;  -- APB configuartion slave irq
    dbits  : integer range 128 to 512     := 128  -- Data bus width allowed values : 128, 256 and 512 bits    

    );
  port (
    rstn         : in  std_ulogic;           -- Reset
    clk          : in  std_ulogic;           -- Clock
    sts_in       : in  ctrlmod_sts_type;     -- status signal
                                             -- from
                                             -- grchirnf_ctrl
                                             -- showing an
                                             -- ongoing transaction
    apbi         : in  apb_slv_in_type;      -- APB slave input
    apbo         : out apb_slv_out_type;     -- APB slave output
    apb_ctrl_out : out grchirnf_ctrl_type;  -- Control configuration signals
    err_status   : out std_ulogic
    );
end entity grchirnf_apb;

------------------------------------------------------------------------------
-- Architecture of grchirnf_apb
------------------------------------------------------------------------------

architecture rtl of grchirnf_apb is
  attribute sync_set_reset         : string;
  attribute sync_set_reset of rstn : signal is "true";
  -----------------------------------------------------------------------------
  -- Constant declaration
  -----------------------------------------------------------------------------

  -- Reset configuration

  constant ASYNC_RST : boolean := GRLIB_CONFIG_ARRAY(grlib_async_reset_enable) = 1;

  constant REVISION : integer := 0;
  -- Plug and Play Information (APB interface)
  constant pconfig : apb_config_type := (
    --0 => ahb_device_reg (VENDOR_GAISLER, GAISLER_GRCHIRNF, 0, REVISION, to_integer(to_unsigned(pirq, 8))),
    0 => ahb_device_reg (VENDOR_GAISLER, 16#0D8#, 0, REVISION, to_integer(to_unsigned(pirq, 8))),
    1 => apb_iobar(paddr, pmask));

  -----------------------------------------------------------------------------
  -- Signal declaration
  -----------------------------------------------------------------------------

  signal r, rin : grchirnf_reg_type;

  -----------------------------------------------------------------------------
  -- Function/procedure declaration
  -----------------------------------------------------------------------------
  
begin  -- rtl

  -----------------------------------------------------------------------------
  -- Assignments
  -----------------------------------------------------------------------------
  
  apbo.pindex  <= pindex;
  apbo.pconfig <= pconfig;

  -----------------------------------------------------------------------------
  -- Combinational process
  -----------------------------------------------------------------------------

  comb : process (apbi, r, sts_in)

    variable v      : grchirnf_reg_type;
    variable prdata : std_logic_vector (31 downto 0);
    
  begin
    -- Initialization
    v      := r;
    prdata := (others => '0');

    ----------------------
    -- core status logic
    ----------------------

    -- Core Status update
    v.sts.ongoing := sts_in.ongoing;

    -- Error updates
    if sts_in.error = '1' then
      v.sts.error := '1';
    end if;

    ----------------------  
    -- APB address decode  
    ----------------------
    ---- Read accesses ----
    if (apbi.psel(pindex) and apbi.penable and not apbi.pwrite) = '1' then
      case apbi.paddr(7 downto 2) is
        when "000000" =>                --0x00 GRCHIRNF control register
          prdata(0) := r.ctrl.rst;
        when "000001" =>                --0x04 GRCHIRNF status register. 
          prdata(0) := r.sts.ongoing;
          prdata(1) := r.sts.error;
        when "000010" =>                --0x08 GRCHIRNF capability register
          prdata(3 downto 0) := conv_std_logic_vector(REVISION, 4);
          case dbits is
            when 128 =>
              prdata(4) := '0';
            when 256 =>
              prdata(4) := '1';
            when others =>              -- 32 bits
               null;
          end case;
        when others =>
          null;
      end case;
    end if;

    ---- Write accesses ----
    if (apbi.psel(pindex) and apbi.penable and apbi.pwrite) = '1' then
      case apbi.paddr(7 downto 2) is
        when "000000" =>                --0x00 GRCHIRNF control register
          v.ctrl.rst := apbi.pwdata(0);
        when "000001" =>                --0x04 GRCHIRNF status register.
          v.sts.error := r.sts.error and not(apbi.pwdata(2));  -- Errors are cleared on write              
        when others =>
          null;
      end case;
    end if;

    ----------------------
    -- Signal update --
    ----------------------
    if r.ctrl.rst = '1' then
      v := GRCHIRNF_REG_RST;
    end if;
    rin          <= v;
    apbo.prdata  <= prdata;
    apbo.pirq    <= (others => '0');
    --  control signals
    apb_ctrl_out <= r.ctrl;
    err_status   <= r.sts.error;
  end process comb;

  -----------------------------------------------------------------------------
  -- Sequential process
  -----------------------------------------------------------------------------

  seq : process (clk, rstn)
  begin
    if (rstn = '0' and ASYNC_RST) then  -- Asynchronous reset
      r <= GRCHIRNF_REG_RST;
    elsif rising_edge(clk) then
      if rstn = '0' or r.ctrl.rst = '1' then
        r <= GRCHIRNF_REG_RST;
      else
        r <= rin;
      end if;
    end if;
  end process seq;

  -----------------------------------------------------------------------------
  -- Component instantiation
  -----------------------------------------------------------------------------

end architecture rtl;



