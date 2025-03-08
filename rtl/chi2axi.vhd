



entity chi2axi()
component ahbm2axi4 is                                                                            
generic (                                                                                       
aximid          : integer range 0 to 15                := 0;                                  
wbuffer_num     : integer range 1 to axi4_max_n(AXIDW) := 8;                                  
rprefetch_num   : integer range 1 to axi4_max_n(AXIDW) := 8;                                  
always_secure   : integer range 0 to 1                 := 1;                                  
endianness_mode : integer range 0 to 1                 := 0;                                  
ahb_endianness  : integer range 0 to 1                 := GRLIB_CONFIG_ARRAY(grlib_little_endi
);                                                                                                
--scantest                                                                                    
scantest        : integer                              := 0                                   
);                                                                                            
port (                                                                                          
rstn  : in  std_logic;                                                                        
clk   : in  std_logic;                                                                        
ahbsi : in  ahb_slv_in_type;                                                                  
ahbso : out ahb_slv_out_type;                                                                 
aximi : in  axi_somi_type;                                                                    
aximo : out axi4_mosi_type                                                                    
);                                                                                            
end component;                                                                                    

