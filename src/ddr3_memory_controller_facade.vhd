library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ddr3_memory_controller_facade is generic (
    SIM_BYPASS_INIT_CAL   : string  := "OFF";
                                  -- # = "OFF" -  Complete memory init &
                                  --              calibration sequence
                                  -- # = "SKIP" - Not supported
                                  -- # = "FAST" - Complete memory init & use
                                  --              abbreviated calib sequence

    SIMULATION            : string  := "FALSE";
                                  -- Should be TRUE during design simulations and
                                  -- FALSE during implementations
    USE_FAKE_RAM          : string  := "FALSE"
                                  -- If TRUE, don't use a DDR3 model but instead use a RAM-backed model
                                  -- that trades accuracy and realism for simulation speed. Useful for functional verification.                                   
); port (
    
   -- Inouts
   dq                        : inout std_logic_vector(63 downto 0);
   dqs_p                     : inout std_logic_vector(7 downto 0);
   dqs_n                     : inout std_logic_vector(7 downto 0);

   -- Outputs
   addr                      : out   std_logic_vector(13 downto 0);
   ba                        : out   std_logic_vector(2 downto 0);
   ras_n                     : out   std_logic;
   cas_n                     : out   std_logic;
   we_n                      : out   std_logic;
   reset_n                   : out   std_logic;
   ck_p                      : out   std_logic_vector(0 downto 0);
   ck_n                      : out   std_logic_vector(0 downto 0);
   cke                       : out   std_logic_vector(0 downto 0);
   cs_n                      : out   std_logic_vector(0 downto 0);
   dm                        : out   std_logic_vector(7 downto 0);
   odt                       : out   std_logic_vector(0 downto 0);

   -- Inputs
   -- Differential system clocks
   sys_clk_p                      : in    std_logic;
   sys_clk_n                      : in    std_logic;
   
   -- user interface signals
   app_addr             : in    std_logic_vector(27 downto 0);
   app_cmd              : in    std_logic_vector(2 downto 0);
   app_en               : in    std_logic;
   app_wdf_data         : in    std_logic_vector(511 downto 0);
   app_wdf_end          : in    std_logic;
   app_wdf_mask         : in    std_logic_vector(63 downto 0)  ;
   app_wdf_wren         : in    std_logic;
   app_rd_data          : out   std_logic_vector(511 downto 0);
   app_rd_data_end      : out   std_logic;
   app_rd_data_valid    : out   std_logic;
   app_rdy              : out   std_logic;
   app_wdf_rdy          : out   std_logic;
   app_sr_active        : out   std_logic;
   app_ref_ack          : out   std_logic;
   app_zq_ack           : out   std_logic;
   ui_clk               : out   std_logic;
   ui_clk_sync_rst      : out   std_logic;
   init_calib_complete  : out std_logic;
      sys_rst                     : in    std_logic
); end entity ddr3_memory_controller_facade;

architecture Behavioural of ddr3_memory_controller_facade is
component ddr3_memory_controller   generic
(


 --***************************************************************************
 -- The following parameters refer to width of various ports
 --***************************************************************************
 BANK_WIDTH            : integer := 3;
                                   -- # of memory Bank Address bits.
 CK_WIDTH              : integer := 1;
                                   -- # of CK/CK# outputs to memory.
 COL_WIDTH             : integer := 10;
                                   -- # of memory Column Address bits.
 CS_WIDTH              : integer := 1;
                                   -- # of unique CS outputs to memory.
 nCS_PER_RANK          : integer := 1;
                                   -- # of unique CS outputs per rank for phy
 CKE_WIDTH             : integer := 1;
                                   -- # of CKE outputs to memory.
 DATA_BUF_ADDR_WIDTH   : integer := 5;
 DQ_CNT_WIDTH          : integer := 6;
                                   -- = ceil(log2(DQ_WIDTH))
 DQ_PER_DM             : integer := 8;
 DM_WIDTH              : integer := 8;
                                   -- # of DM (data mask)
 DQ_WIDTH              : integer := 64;
                                   -- # of DQ (data)
 DQS_WIDTH             : integer := 8;
 DQS_CNT_WIDTH         : integer := 3;
                                   -- = ceil(log2(DQS_WIDTH))
 DRAM_WIDTH            : integer := 8;
                                   -- # of DQ per DQS
 ECC                   : string  := "OFF";
 DATA_WIDTH            : integer := 64;
 ECC_TEST              : string  := "OFF";
 PAYLOAD_WIDTH         : integer := 64;
 ECC_WIDTH             : integer := 8;
 MC_ERR_ADDR_WIDTH     : integer := 31;
 MEM_ADDR_ORDER
   : string  := "ROW_BANK_COLUMN";

    
 nBANK_MACHS           : integer := 4;
 RANKS                 : integer := 1;
                                   -- # of Ranks.
 ODT_WIDTH             : integer := 1;
                                   -- # of ODT outputs to memory.
 ROW_WIDTH             : integer := 14;
                                   -- # of memory Row Address bits.
 ADDR_WIDTH            : integer := 28;
                                   -- # = RANK_WIDTH + BANK_WIDTH
                                   --     + ROW_WIDTH + COL_WIDTH;
                                   -- Chip Select is always tied to low for
                                   -- single rank devices
 USE_CS_PORT          : integer := 1;
                                   -- # = 1, When Chip Select (CS#) output is enabled
                                   --   = 0, When Chip Select (CS#) output is disabled
                                   -- If CS_N disabled, user must connect
                                   -- DRAM CS_N input(s) to ground
 USE_DM_PORT           : integer := 1;
                                   -- # = 1, When Data Mask option is enabled
                                   --   = 0, When Data Mask option is disbaled
                                   -- When Data Mask option is disabled in
                                   -- MIG Controller Options page, the logic
                                   -- related to Data Mask should not get
                                   -- synthesized
 USE_ODT_PORT          : integer := 1;
                                   -- # = 1, When ODT output is enabled
                                   --   = 0, When ODT output is disabled
                                   -- Parameter configuration for Dynamic ODT support:
                                   -- USE_ODT_PORT = 0, RTT_NOM = "DISABLED", RTT_WR = "60/120".
                                   -- This configuration allows to save ODT pin mapping from FPGA.
                                   -- The user can tie the ODT input of DRAM to HIGH.
 PHY_CONTROL_MASTER_BANK : integer := 1;
                                   -- The bank index where master PHY_CONTROL resides,
                                   -- equal to the PLL residing bank
 MEM_DENSITY             : string  := "1Gb";
                                   -- Indicates the density of the Memory part
                                   -- Added for the sake of Vivado simulations
 MEM_SPEEDGRADE          : string  := "125";
                                   -- Indicates the Speed grade of Memory Part
                                   -- Added for the sake of Vivado simulations
 MEM_DEVICE_WIDTH        : integer := 8;
                                   -- Indicates the device width of the Memory Part
                                   -- Added for the sake of Vivado simulations

 --***************************************************************************
 -- The following parameters are mode register settings
 --***************************************************************************
 AL                    : string  := "0";
                                   -- DDR3 SDRAM:
                                   -- Additive Latency (Mode Register 1).
                                   -- # = "0", "CL-1", "CL-2".
                                   -- DDR2 SDRAM:
                                   -- Additive Latency (Extended Mode Register).
 nAL                   : integer := 0;
                                   -- # Additive Latency in number of clock
                                   -- cycles.
 BURST_MODE            : string  := "8";
                                   -- DDR3 SDRAM:
                                   -- Burst Length (Mode Register 0).
                                   -- # = "8", "4", "OTF".
                                   -- DDR2 SDRAM:
                                   -- Burst Length (Mode Register).
                                   -- # = "8", "4".
 BURST_TYPE            : string  := "SEQ";
                                   -- DDR3 SDRAM: Burst Type (Mode Register 0).
                                   -- DDR2 SDRAM: Burst Type (Mode Register).
                                   -- # = "SEQ" - (Sequential),
                                   --   = "INT" - (Interleaved).
 CL                    : integer := 11;
                                   -- in number of clock cycles
                                   -- DDR3 SDRAM: CAS Latency (Mode Register 0).
                                   -- DDR2 SDRAM: CAS Latency (Mode Register).
 CWL                   : integer := 8;
                                   -- in number of clock cycles
                                   -- DDR3 SDRAM: CAS Write Latency (Mode Register 2).
                                   -- DDR2 SDRAM: Can be ignored
 OUTPUT_DRV            : string  := "HIGH";
                                   -- Output Driver Impedance Control (Mode Register 1).
                                   -- # = "HIGH" - RZQ/7,
                                   --   = "LOW" - RZQ/6.
 RTT_NOM               : string  := "40";
                                   -- RTT_NOM (ODT) (Mode Register 1).
                                   --   = "120" - RZQ/2,
                                   --   = "60"  - RZQ/4,
                                   --   = "40"  - RZQ/6.
 RTT_WR                : string  := "OFF";
                                   -- RTT_WR (ODT) (Mode Register 2).
                                   -- # = "OFF" - Dynamic ODT off,
                                   --   = "120" - RZQ/2,
                                   --   = "60"  - RZQ/4,
 ADDR_CMD_MODE         : string  := "1T" ;
                                   -- # = "1T", "2T".
 REG_CTRL              : string  := "OFF";
                                   -- # = "ON" - RDIMMs,
                                   --   = "OFF" - Components, SODIMMs, UDIMMs.
 CA_MIRROR             : string  := "OFF";
                                   -- C/A mirror opt for DDR3 dual rank
 VDD_OP_VOLT           : string  := "150";
                                   -- # = "150" - 1.5V Vdd Memory part
                                   --   = "135" - 1.35V Vdd Memory part
 
 --***************************************************************************
 -- The following parameters are multiplier and divisor factors for PLLE2.
 -- Based on the selected design frequency these parameters vary.
 --***************************************************************************
 CLKIN_PERIOD          : integer := 5000;
                                   -- Input Clock Period
 CLKFBOUT_MULT         : integer := 8;
                                   -- write PLL VCO multiplier
 DIVCLK_DIVIDE         : integer := 1;
                                   -- write PLL VCO divisor
 CLKOUT0_PHASE         : real    := 337.5;
                                   -- Phase for PLL output clock (CLKOUT0)
 CLKOUT0_DIVIDE        : integer := 2;
                                   -- VCO output divisor for PLL output clock (CLKOUT0)
 CLKOUT1_DIVIDE        : integer := 2;
                                   -- VCO output divisor for PLL output clock (CLKOUT1)
 CLKOUT2_DIVIDE        : integer := 32;
                                   -- VCO output divisor for PLL output clock (CLKOUT2)
 CLKOUT3_DIVIDE        : integer := 8;
                                   -- VCO output divisor for PLL output clock (CLKOUT3)

 --***************************************************************************
 -- Memory Timing Parameters. These parameters varies based on the selected
 -- memory part.
 --***************************************************************************
 tCKE                  : integer := 5000;
                                   -- memory tCKE paramter in pS
 tFAW                  : integer := 30000;
                                   -- memory tRAW paramter in pS.
 tPRDI                 : integer := 1000000;
                                   -- memory tPRDI paramter in pS.
 tRAS                  : integer := 35000;
                                   -- memory tRAS paramter in pS.
 tRCD                  : integer := 13125;
                                   -- memory tRCD paramter in pS.
 tREFI                 : integer := 7800000;
                                   -- memory tREFI paramter in pS.
 tRFC                  : integer := 110000;
                                   -- memory tRFC paramter in pS.
 tRP                   : integer := 13125;
                                   -- memory tRP paramter in pS.
 tRRD                  : integer := 6000;
                                   -- memory tRRD paramter in pS.
 tRTP                  : integer := 7500;
                                   -- memory tRTP paramter in pS.
 tWTR                  : integer := 7500;
                                   -- memory tWTR paramter in pS.
 tZQI                  : integer := 128000000;
                                   -- memory tZQI paramter in nS.
 tZQCS                 : integer := 64;
                                   -- memory tZQCS paramter in clock cycles.

 --***************************************************************************
 -- Simulation parameters
 --***************************************************************************
 SIM_BYPASS_INIT_CAL   : string  := "OFF";
                                   -- # = "OFF" -  Complete memory init &
                                   --              calibration sequence
                                   -- # = "SKIP" - Not supported
                                   -- # = "FAST" - Complete memory init & use
                                   --              abbreviated calib sequence

 SIMULATION            : string  := "FALSE";
                                   -- Should be TRUE during design simulations and
                                   -- FALSE during implementations

 --***************************************************************************
 -- The following parameters varies based on the pin out entered in MIG GUI.
 -- Do not change any of these parameters directly by editing the RTL.
 -- Any changes required should be done through GUI and the design regenerated.
 --***************************************************************************
 BYTE_LANES_B0         : std_logic_vector(3 downto 0) := "1111";
                                   -- Byte lanes used in an IO column.
 BYTE_LANES_B1         : std_logic_vector(3 downto 0) := "1110";
                                   -- Byte lanes used in an IO column.
 BYTE_LANES_B2         : std_logic_vector(3 downto 0) := "1111";
                                   -- Byte lanes used in an IO column.
 BYTE_LANES_B3         : std_logic_vector(3 downto 0) := "0000";
                                   -- Byte lanes used in an IO column.
 BYTE_LANES_B4         : std_logic_vector(3 downto 0) := "0000";
                                   -- Byte lanes used in an IO column.
 DATA_CTL_B0           : std_logic_vector(3 downto 0) := "1111";
                                   -- Indicates Byte lane is data byte lane
                                   -- or control Byte lane. '1' in a bit
                                   -- position indicates a data byte lane and
                                   -- a '0' indicates a control byte lane
 DATA_CTL_B1           : std_logic_vector(3 downto 0) := "0000";
                                   -- Indicates Byte lane is data byte lane
                                   -- or control Byte lane. '1' in a bit
                                   -- position indicates a data byte lane and
                                   -- a '0' indicates a control byte lane
 DATA_CTL_B2           : std_logic_vector(3 downto 0) := "1111";
                                   -- Indicates Byte lane is data byte lane
                                   -- or control Byte lane. '1' in a bit
                                   -- position indicates a data byte lane and
                                   -- a '0' indicates a control byte lane
 DATA_CTL_B3           : std_logic_vector(3 downto 0) := "0000";
                                   -- Indicates Byte lane is data byte lane
                                   -- or control Byte lane. '1' in a bit
                                   -- position indicates a data byte lane and
                                   -- a '0' indicates a control byte lane
 DATA_CTL_B4           : std_logic_vector(3 downto 0) := "0000";
                                   -- Indicates Byte lane is data byte lane
                                   -- or control Byte lane. '1' in a bit
                                   -- position indicates a data byte lane and
                                   -- a '0' indicates a control byte lane
 PHY_0_BITLANES        : std_logic_vector(47 downto 0) := X"3FE1FF1FF2FF";
 PHY_1_BITLANES        : std_logic_vector(47 downto 0) := X"FFEF30CB4000";
 PHY_2_BITLANES        : std_logic_vector(47 downto 0) := X"3FE3FE3BF2FF";

 -- control/address/data pin mapping parameters
 CK_BYTE_MAP
   : std_logic_vector(143 downto 0) := X"000000000000000000000000000000000011";
 ADDR_MAP
   : std_logic_vector(191 downto 0) := X"00000013213613513313912413112913713413A12813813B";
 BANK_MAP   : std_logic_vector(35 downto 0) := X"12512A12B";
 CAS_MAP    : std_logic_vector(11 downto 0) := X"115";
 CKE_ODT_BYTE_MAP : std_logic_vector(7 downto 0) := X"00";
 CKE_MAP    : std_logic_vector(95 downto 0) := X"000000000000000000000117";
 ODT_MAP    : std_logic_vector(95 downto 0) := X"000000000000000000000112";
 CS_MAP     : std_logic_vector(119 downto 0) := X"000000000000000000000000000114";
 PARITY_MAP : std_logic_vector(11 downto 0) := X"000";
 RAS_MAP    : std_logic_vector(11 downto 0) := X"11A";
 WE_MAP     : std_logic_vector(11 downto 0) := X"11B";
 DQS_BYTE_MAP
   : std_logic_vector(143 downto 0) := X"000000000000000000002021222303020100";
 DATA0_MAP  : std_logic_vector(95 downto 0) := X"009000003001007006005002";
 DATA1_MAP  : std_logic_vector(95 downto 0) := X"014018010011017016012013";
 DATA2_MAP  : std_logic_vector(95 downto 0) := X"021022025020027023026028";
 DATA3_MAP  : std_logic_vector(95 downto 0) := X"033039031035032038034037";
 DATA4_MAP  : std_logic_vector(95 downto 0) := X"231238237236233232234239";
 DATA5_MAP  : std_logic_vector(95 downto 0) := X"226227225229221222224228";
 DATA6_MAP  : std_logic_vector(95 downto 0) := X"214215210218217213219212";
 DATA7_MAP  : std_logic_vector(95 downto 0) := X"207203204206202201205209";
 DATA8_MAP  : std_logic_vector(95 downto 0) := X"000000000000000000000000";
 DATA9_MAP  : std_logic_vector(95 downto 0) := X"000000000000000000000000";
 DATA10_MAP : std_logic_vector(95 downto 0) := X"000000000000000000000000";
 DATA11_MAP : std_logic_vector(95 downto 0) := X"000000000000000000000000";
 DATA12_MAP : std_logic_vector(95 downto 0) := X"000000000000000000000000";
 DATA13_MAP : std_logic_vector(95 downto 0) := X"000000000000000000000000";
 DATA14_MAP : std_logic_vector(95 downto 0) := X"000000000000000000000000";
 DATA15_MAP : std_logic_vector(95 downto 0) := X"000000000000000000000000";
 DATA16_MAP : std_logic_vector(95 downto 0) := X"000000000000000000000000";
 DATA17_MAP : std_logic_vector(95 downto 0) := X"000000000000000000000000";
 MASK0_MAP  : std_logic_vector(107 downto 0) := X"000200211223235036024015004";
 MASK1_MAP  : std_logic_vector(107 downto 0) := X"000000000000000000000000000";

 SLOT_0_CONFIG         : std_logic_vector(7 downto 0) := "00000001";
                                   -- Mapping of Ranks.
 SLOT_1_CONFIG         : std_logic_vector(7 downto 0) := "00000000";
                                   -- Mapping of Ranks.

 --***************************************************************************
 -- IODELAY and PHY related parameters
 --***************************************************************************
 IBUF_LPWR_MODE        : string  := "OFF";
                                   -- to phy_top
 DATA_IO_IDLE_PWRDWN   : string  := "ON";
                                   -- # = "ON", "OFF"
 BANK_TYPE             : string  := "HP_IO";
                                   -- # = "HP_IO", "HPL_IO", "HR_IO", "HRL_IO"
 DATA_IO_PRIM_TYPE     : string  := "HP_LP";
                                   -- # = "HP_LP", "HR_LP", "DEFAULT"
 CKE_ODT_AUX           : string  := "FALSE";
 USER_REFRESH          : string  := "OFF";
 WRLVL                 : string  := "ON";
                                   -- # = "ON" - DDR3 SDRAM
                                   --   = "OFF" - DDR2 SDRAM.
 ORDERING              : string  := "STRICT";
                                   -- # = "NORM", "STRICT", "RELAXED".
 CALIB_ROW_ADD         : std_logic_vector(15 downto 0) := X"0000";
                                   -- Calibration row address will be used for
                                   -- calibration read and write operations
 CALIB_COL_ADD         : std_logic_vector(11 downto 0) := X"000";
                                   -- Calibration column address will be used for
                                   -- calibration read and write operations
 CALIB_BA_ADD          : std_logic_vector(2 downto 0) := "000";
                                   -- Calibration bank address will be used for
                                   -- calibration read and write operations
 TCQ                   : integer := 100;
 IODELAY_GRP           : string  := "IODELAY_MIG";
                                   -- It is associated to a set of IODELAYs with
                                   -- an IDELAYCTRL that have same IODELAY CONTROLLER
                                   -- clock frequency.
 SYSCLK_TYPE           : string  := "DIFFERENTIAL";
                                   -- System clock type DIFFERENTIAL, SINGLE_ENDED,
                                   -- NO_BUFFER
 REFCLK_TYPE           : string  := "USE_SYSTEM_CLOCK";
                                   -- Reference clock type DIFFERENTIAL, SINGLE_ENDED
                                   -- NO_BUFFER, USE_SYSTEM_CLOCK
 SYS_RST_PORT          : string  := "FALSE";
                                   -- "TRUE" - if pin is selected for sys_rst
                                   --          and IBUF will be instantiated.
                                   -- "FALSE" - if pin is not selected for sys_rst
    
 CMD_PIPE_PLUS1        : string  := "ON";
                                   -- add pipeline stage between MC and PHY
 DRAM_TYPE             : string  := "DDR3";
 CAL_WIDTH             : string  := "HALF";
 STARVE_LIMIT          : integer := 2;
                                   -- # = 2,3,4.

 --***************************************************************************
 -- Referece clock frequency parameters
 --***************************************************************************
 REFCLK_FREQ           : real    := 200.0;
                                   -- IODELAYCTRL reference clock frequency
 DIFF_TERM_REFCLK      : string  := "TRUE";
                                   -- Differential Termination for idelay
                                   -- reference clock input pins
 --***************************************************************************
 -- System clock frequency parameters
 --***************************************************************************
 tCK                   : integer := 1250;
                                   -- memory tCK paramter.
                                   -- # = Clock Period in pS.
 nCK_PER_CLK           : integer := 4;
                                   -- # of memory CKs per fabric CLK
 DIFF_TERM_SYSCLK      : string  := "FALSE";
                                   -- Differential Termination for System
                                   -- clock input pins

 --***************************************************************************
 -- Debug parameters
 --***************************************************************************
 DEBUG_PORT            : string  := "OFF";
                                   -- # = "ON" Enable debug signals/controls.
                                   --   = "OFF" Disable debug signals/controls.

 --***************************************************************************
 -- Temparature monitor parameter
 --***************************************************************************
 TEMP_MON_CONTROL         : string  := "INTERNAL";
                                   -- # = "INTERNAL", "EXTERNAL"
    
 RST_ACT_LOW           : integer := 0
                                   -- =1 for active low reset,
                                   -- =0 for active high.
 );
port
(

 -- Inouts
 ddr3_dq                        : inout std_logic_vector(DQ_WIDTH-1 downto 0);
 ddr3_dqs_p                     : inout std_logic_vector(DQS_WIDTH-1 downto 0);
 ddr3_dqs_n                     : inout std_logic_vector(DQS_WIDTH-1 downto 0);

 -- Outputs
 ddr3_addr                      : out   std_logic_vector(ROW_WIDTH-1 downto 0);
 ddr3_ba                        : out   std_logic_vector(BANK_WIDTH-1 downto 0);
 ddr3_ras_n                     : out   std_logic;
 ddr3_cas_n                     : out   std_logic;
 ddr3_we_n                      : out   std_logic;
 ddr3_reset_n                   : out   std_logic;
 ddr3_ck_p                      : out   std_logic_vector(CK_WIDTH-1 downto 0);
 ddr3_ck_n                      : out   std_logic_vector(CK_WIDTH-1 downto 0);
 ddr3_cke                       : out   std_logic_vector(CKE_WIDTH-1 downto 0);
 ddr3_cs_n                      : out   std_logic_vector(CS_WIDTH*nCS_PER_RANK-1 downto 0);
 ddr3_dm                        : out   std_logic_vector(DM_WIDTH-1 downto 0);
 ddr3_odt                       : out   std_logic_vector(ODT_WIDTH-1 downto 0);

 -- Inputs
 -- Differential system clocks
 sys_clk_p                      : in    std_logic;
 sys_clk_n                      : in    std_logic;
 
 -- user interface signals
 app_addr             : in    std_logic_vector(ADDR_WIDTH-1 downto 0);
 app_cmd              : in    std_logic_vector(2 downto 0);
 app_en               : in    std_logic;
 app_wdf_data         : in    std_logic_vector((nCK_PER_CLK*2*PAYLOAD_WIDTH)-1 downto 0);
 app_wdf_end          : in    std_logic;
 app_wdf_mask         : in    std_logic_vector((nCK_PER_CLK*2*PAYLOAD_WIDTH)/8-1 downto 0)  ;
 app_wdf_wren         : in    std_logic;
 app_rd_data          : out   std_logic_vector((nCK_PER_CLK*2*PAYLOAD_WIDTH)-1 downto 0);
 app_rd_data_end      : out   std_logic;
 app_rd_data_valid    : out   std_logic;
 app_rdy              : out   std_logic;
 app_wdf_rdy          : out   std_logic;
 app_sr_active        : out   std_logic;
 app_ref_ack          : out   std_logic;
 app_zq_ack           : out   std_logic;
 ui_clk               : out   std_logic;
 ui_clk_sync_rst      : out   std_logic;
 
    
 
 init_calib_complete  : out std_logic;
 
    

 -- System reset - Default polarity of sys_rst pin is Active Low.
 -- System reset polarity will change based on the option 
 -- selected in GUI.
    sys_rst                     : in    std_logic
); end component ddr3_memory_controller;

   -- Inouts
   signal ddr3_dq                        :  std_logic_vector(63 downto 0);
   signal ddr3_dqs_p                     :  std_logic_vector(7 downto 0);
   signal ddr3_dqs_n                     :  std_logic_vector(7 downto 0);

   -- Outputs
   signal ddr3_addr                      :    std_logic_vector(13 downto 0);
   signal ddr3_ba                        :    std_logic_vector(2 downto 0);
   signal ddr3_ras_n                     :    std_logic;
   signal ddr3_cas_n                     :    std_logic;
   signal ddr3_we_n                      :    std_logic;
   signal ddr3_reset_n                   :    std_logic;
   signal ddr3_ck_p                      :    std_logic_vector(0 downto 0);
   signal ddr3_ck_n                      :    std_logic_vector(0 downto 0);
   signal ddr3_cke                       :    std_logic_vector(0 downto 0);
   signal ddr3_cs_n                      :    std_logic_vector(0 downto 0);
   signal ddr3_dm                        :    std_logic_vector(7 downto 0);
   signal ddr3_odt                       :    std_logic_vector(0 downto 0);

component ddr3_sodimm_sim port (
    rst_n: in std_logic;
    ck: in std_logic;
    ck_n: in std_logic;
    cke: in std_logic;
    cs_n: in std_logic;
    ras_n: in std_logic;
    cas_n: in std_logic;
    we_n: in std_logic;
    dm: in std_logic_vector(7 downto 0);
    ba: in std_logic_vector(2 downto 0);
    addr: in std_logic_vector(13 downto 0);
    dq: inout std_logic_vector(63 downto 0);
    dqs: inout std_logic_vector(7 downto 0);
    dqs_n: inout std_logic_vector(7 downto 0);
    odt: in std_logic;
    init_calib_complete: in std_logic
); end component ddr3_sodimm_sim;
signal calib_done: std_logic;

begin

REAL_DDR3: if (USE_FAKE_RAM = "FALSE") generate

REAL_MEMORY_CONTROLLER: ddr3_memory_controller generic map (
    SIM_BYPASS_INIT_CAL => SIM_BYPASS_INIT_CAL,
    SIMULATION => SIMULATION
) port map (
-- Inouts
ddr3_dq                        => ddr3_dq,
ddr3_dqs_p                     => ddr3_dqs_p,
ddr3_dqs_n                     => ddr3_dqs_n,

-- Outputs
ddr3_addr                      => ddr3_addr,
ddr3_ba                        => ddr3_ba,
ddr3_ras_n                     => ddr3_ras_n,
ddr3_cas_n                     => ddr3_cas_n,
ddr3_we_n                      => ddr3_we_n,
ddr3_reset_n                   => ddr3_reset_n,
ddr3_ck_p                      => ddr3_ck_p,
ddr3_ck_n                      => ddr3_ck_n,
ddr3_cke                       => ddr3_cke,
ddr3_cs_n                      => ddr3_cs_n,
ddr3_dm                        => ddr3_dm,
ddr3_odt                       => ddr3_odt,

-- Inputs
-- Differential system clocks
sys_clk_p                      => sys_clk_p,
sys_clk_n                      => sys_clk_n,

-- user interface signals
app_addr             => app_addr,
app_cmd              => app_cmd,
app_en               => app_en,
app_wdf_data         => app_wdf_data,
app_wdf_end          => app_wdf_end,
app_wdf_mask         => app_wdf_mask,
app_wdf_wren         => app_wdf_wren,
app_rd_data          => app_rd_data,
app_rd_data_end      => app_rd_data_end,
app_rd_data_valid    => app_rd_data_valid,
app_rdy              => app_rdy,
app_wdf_rdy          => app_wdf_rdy,
app_sr_active        => app_sr_active,
app_ref_ack          => app_ref_ack,
app_zq_ack           => app_zq_ack,
ui_clk               => ui_clk,
ui_clk_sync_rst      => ui_clk_sync_rst,

   

init_calib_complete  => calib_done,

   

-- System reset - Default polarity of sys_rst pin is Active Low.
-- System reset polarity will change based on the option 
-- selected in GUI.
   sys_rst            => sys_rst  
);
init_calib_complete <= calib_done;

DDR3_SIMULATION_MODEL: if (SIMULATION = "TRUE") generate
SODIMM: ddr3_sodimm_sim port map (
    rst_n => ddr3_reset_n,
    ck => ddr3_ck_p(0),
    ck_n => ddr3_ck_n(0),
    cke => ddr3_cke(0),
    cs_n => ddr3_cs_n(0),
    ras_n => ddr3_ras_n,
    cas_n => ddr3_cas_n,
    we_n => ddr3_we_n,
    dm => ddr3_dm,
    ba => ddr3_ba,
    addr => ddr3_addr,    
    dq => ddr3_dq,
    dqs => ddr3_dqs_p,
    dqs_n => ddr3_dqs_n,
    odt => ddr3_odt(0),
    init_calib_complete => calib_done
);
end generate DDR3_SIMULATION_MODEL;

end generate REAL_DDR3;

FAKE_DDR3: if (USE_FAKE_RAM = "TRUE") generate
end generate FAKE_DDR3;

end architecture Behavioural;