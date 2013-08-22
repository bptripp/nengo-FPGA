library ieee;
use ieee.std_logic_1164.all;

entity vc707_nengo_rt is port (
    SYSCLK_P: in std_logic;
    SYSCLK_N: in std_logic;
    SGMIICLK_P: in std_logic; -- 125 MHz
    SGMIICLK_N: in std_logic;    
    
    SGMII_TXP : out std_logic;
    SGMII_TXN : out std_logic;
    SGMII_RXP : in std_logic; 
    SGMII_RXN : in std_logic;
    PHY_RESET: out std_logic;
    
    RST: in std_logic;
    
    GPIO_DIP: in std_logic_vector(7 downto 0);
    GPIO_LED: out std_logic_vector(7 downto 0)
); end entity vc707_nengo_rt;

architecture TOPLEVEL of vc707_nengo_rt is

attribute mark_debug : string;

constant station_mac: std_logic_vector(47 downto 0) := X"000A35028FC0";

component sgmii_clock_module port (
SYSCLK_P: in std_logic; -- 200 MHz
SYSCLK_N: in std_logic;
SGMIICLK_P: in std_logic; -- 125 MHz
SGMIICLK_N: in std_logic;
txoutclk: in std_logic; -- 62.5 MHz    

mmcm_reset: in std_logic;
mmcm_locked: out std_logic;

clk_200: out std_logic; -- 200 MHz
clk_125: out std_logic; -- 125 MHz
independent_clock: out std_logic; -- 200 MHz
mgtrefclk: out std_logic; -- 125 MHz
userclk: out std_logic; -- 62.5 MHz
userclk2: out std_logic -- 125 MHz
); end component sgmii_clock_module;
signal txoutclk: std_logic;
signal mmcm_reset: std_logic;
signal mmcm_locked: std_logic;
signal clk_200: std_logic;
signal clk_125: std_logic;
signal independent_clock: std_logic;
signal mgtrefclk: std_logic;
signal userclk: std_logic;
signal userclk2: std_logic;

component sgmii_vc707 generic
(
    EXAMPLE_SIMULATION                      : integer   := 0          -- Set to 1 for simulation
);
      port(
      -- Transceiver Interface
      ---------------------
      drpaddr_in          : in   std_logic_vector(8 downto 0);
      drpclk_in           : in   std_logic;
      drpdi_in            : in   std_logic_vector(15 downto 0);
      drpdo_out           : out  std_logic_vector(15 downto 0);
      drpen_in            : in   std_logic;
      drprdy_out          : out  std_logic;
      drpwe_in            : in   std_logic;
      gtrefclk             : in std_logic;                     -- Very high quality 125MHz clock for GT transceiver.
      txp                  : out std_logic;                    -- Differential +ve of serial transmission from PMA to PMD.
      txn                  : out std_logic;                    -- Differential -ve of serial transmission from PMA to PMD.
      rxp                  : in std_logic;                     -- Differential +ve for serial reception from PMD to PMA.
      rxn                  : in std_logic;                     -- Differential -ve for serial reception from PMD to PMA.

      txoutclk             : out std_logic;                    -- txoutclk from GT transceiver (62.5MHz)
      resetdone            : out std_logic;                    -- The GT transceiver has completed its reset cycle
      mmcm_locked          : in std_logic;                     -- Locked indication from MMCM
      userclk              : in std_logic;                     -- 62.5MHz global clock.
      userclk2             : in std_logic;                     -- 125MHz global clock.
      independent_clock_bufg : in std_logic;                   -- 200MHz independent cloc,
      pma_reset            : in std_logic;                     -- transceiver PMA reset signal

      -- GMII Interface
      -----------------
      sgmii_clk_r            : out std_logic;                    -- Clock for client MAC (125Mhz, 12.5MHz or 1.25MHz).
      sgmii_clk_f            : out std_logic;                    -- Clock for client MAC (125Mhz, 12.5MHz or 1.25MHz).
      sgmii_clk_en         : out std_logic;                    -- Clock enable for client MAC
      gmii_txd             : in std_logic_vector(7 downto 0);  -- Transmit data from client MAC.
      gmii_tx_en           : in std_logic;                     -- Transmit control signal from client MAC.
      gmii_tx_er           : in std_logic;                     -- Transmit control signal from client MAC.
      gmii_rxd             : out std_logic_vector(7 downto 0); -- Received Data to client MAC.
      gmii_rx_dv           : out std_logic;                    -- Received control signal to client MAC.
      gmii_rx_er           : out std_logic;                    -- Received control signal to client MAC.
      gmii_isolate         : out std_logic;                    -- Tristate control to electrically isolate GMII.

      -- Management: Alternative to MDIO Interface
      --------------------------------------------

      configuration_vector : in std_logic_vector(4 downto 0);  -- Alternative to MDIO interface.


      an_interrupt         : out std_logic;                    -- Interrupt to processor to signal that Auto-Negotiation has completed
      an_adv_config_vector : in std_logic_vector(15 downto 0); -- Alternate interface to program REG4 (AN ADV)
      an_restart_config    : in std_logic;                     -- Alternate signal to modify AN restart bit in REG0
      link_timer_value     : in std_logic_vector(8 downto 0);  -- Programmable Auto-Negotiation Link Timer Control

      -- Speed Control
      ----------------
      speed_is_10_100      : in std_logic;                     -- Core should operate at either 10Mbps or 100Mbps speeds
      speed_is_100         : in std_logic;                      -- Core should operate at 100Mbps speed

      -- General IO's
      ---------------
      status_vector        : out std_logic_vector(15 downto 0); -- Core status.
      reset                : in std_logic;                     -- Asynchronous reset for entire core.
      signal_detect        : in std_logic                      -- Input from PMD to indicate presence of optical input.

      ); end component sgmii_vc707;
    signal resetdone: std_logic;
    signal sgmii_clk_r: std_logic;
    signal sgmii_clk_f: std_logic;
    signal sgmii_clk_en: std_logic;
    signal gmii_txd: std_logic_vector(7 downto 0) := X"00"; -- FIXME transmit path disabled for now
    signal gmii_tx_en: std_logic := '0';
    signal gmii_tx_er: std_logic := '0';
    signal gmii_rxd: std_logic_vector(7 downto 0);
    signal gmii_rx_dv: std_logic;
    signal gmii_rx_er: std_logic;
    signal status_vector: std_logic_vector(15 downto 0);
    signal signal_detect: std_logic := '1'; -- optical only
    signal speed_is_10_100: std_logic;
    signal speed_is_100: std_logic;

component ethernet_rx_channel port (
    clk: in std_logic;
    rst: in std_logic; -- must be an OFF-BOARD reset
    station_mac: in std_logic_vector(47 downto 0);    
    -- from Ethernet PHY
    rx_data: in std_logic_vector(7 downto 0);
    rx_en: in std_logic;
    rx_err: in std_logic;
    -- to RX FIFO
    fifo_data: out std_logic_vector(7 downto 0);
    fifo_first: out std_logic; -- FIFO bit 8
    fifo_we: out std_logic
); end component ethernet_rx_channel;

signal rx_fifo_din: std_logic_vector(8 downto 0); 
signal rx_fifo_we: std_logic; 

component ethernet_rx_fifo PORT (
    rst : IN STD_LOGIC;
    wr_clk : IN STD_LOGIC;
    rd_clk : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(8 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC
  ); end component ethernet_rx_fifo;
  signal rx_fifo_re: std_logic; 
  signal rx_fifo_dout: std_logic_vector(8 downto 0); 
  signal rx_fifo_full: std_logic;
  signal rx_fifo_empty: std_logic;

component ethernet_rx_handler port (
    clk: in std_logic;
    rst: in std_logic; -- must be an OFF-BOARD reset
    station_mac: in std_logic_vector(47 downto 0);    
    
    -- Ethernet RX FIFO interface
    fifo_data: in std_logic_vector(8 downto 0);
    fifo_empty: in std_logic;
    fifo_re: out std_logic;
    
    -- Nengo programming and control interface
    prog_addr: out std_logic_vector(23 downto 0);
    prog_we: out std_logic;
    prog_data: out std_logic_vector(39 downto 0);
    
    page_block_addr: out std_logic_vector(5 downto 0);
    page_word_addr: out std_logic_vector(10 downto 0);
    page_we: out std_logic;
    page_lock: out std_logic;
    page_data: out std_logic_vector(11 downto 0);
    
    prog_ok: in std_logic;
    prog_ack: in std_logic;
    prog_nyet: in std_logic;
    
    system_reset: out std_logic;
    sim_start: out std_logic;
    sim_pause: out std_logic
); end component ethernet_rx_handler;
signal prog_addr: std_logic_vector(23 downto 0); 
signal prog_we: std_logic; 
signal prog_data: std_logic_vector(39 downto 0); 
signal prog_ok: std_logic; 
signal prog_ack: std_logic; 
signal prog_nyet: std_logic;
signal page_block_addr: std_logic_vector(5 downto 0);
signal page_word_addr: std_logic_vector(10 downto 0);
signal page_we: std_logic;
signal page_lock: std_logic;
signal page_data: std_logic_vector(11 downto 0);
signal system_reset: std_logic;
signal sim_start: std_logic;
signal sim_pause: std_logic;


component nengo_rt_tl generic (
    SIMULATION: string := "FALSE"
); port (
    clk_125: in std_logic;
    rst: in std_logic;    
    prog_addr: in std_logic_vector(23 downto 0); 
    prog_we: in std_logic; -- active HIGH pulse; ignored while prog_ok is LOW.
    prog_data: in std_logic_vector(39 downto 0);
    prog_ack: out std_logic;
    prog_nyet: out std_logic;
    prog_error: out std_logic;
    prog_ok: out std_logic; -- HIGH when programming is allowed, i.e. after system reset and before run start
    page_block_addr: in std_logic_vector(5 downto 0);
    page_word_addr: in std_logic_vector(10 downto 0);
    page_we: in std_logic;
    page_lock: in std_logic;
    page_data: in std_logic_vector(11 downto 0);
    start: in std_logic; -- Pulse HIGH to begin execution. Ignored while prog_ok is LOW.
    pause: in std_logic; -- Pulse HIGH to pause execution after current timestep. If start also asserted
                         -- on same timestep, single-step the simulation.
    running: out std_logic;
    timestep_overflow: out std_logic -- Strobed HIGH when a timeout has occurred.
    ); end component nengo_rt_tl;
    signal sim_running: std_logic; 
    signal timestep_overflow: std_logic; 

begin

CLOCK_MODULE: sgmii_clock_module port map (
    SYSCLK_P => SYSCLK_P,
    SYSCLK_N => SYSCLK_N,
    SGMIICLK_P => SGMIICLK_P,
    SGMIICLK_N => SGMIICLK_N,
    txoutclk => txoutclk,
    mmcm_reset => mmcm_reset,
    mmcm_locked => mmcm_locked,
    clk_200 => clk_200,
    clk_125 => clk_125,
    independent_clock => independent_clock,
    mgtrefclk => mgtrefclk,
    userclk => userclk,
    userclk2 => userclk2
);
mmcm_reset <= RST or not resetdone;
PHY_RESET <= not RST; -- active_low


SGMII: sgmii_vc707 generic map ( EXAMPLE_SIMULATION => 0) port map (
    drpaddr_in => "000000000",
    drpclk_in => '0',
    drpdi_in => X"0000",
    drpdo_out => open,
    drpen_in => '0',
    drprdy_out => open,
    drpwe_in => '0',
    
    gtrefclk => mgtrefclk,
    txp => SGMII_TXP,
    txn => SGMII_TXN,
    rxp => SGMII_RXP,
    rxn => SGMII_RXN,
    
    txoutclk => txoutclk,
    resetdone => resetdone,
    mmcm_locked => mmcm_locked,
    userclk => userclk,
    userclk2 => userclk2,
    independent_clock_bufg => independent_clock,
    pma_reset => RST,
      
    sgmii_clk_r => sgmii_clk_r,
    sgmii_clk_f => sgmii_clk_f,
    sgmii_clk_en => sgmii_clk_en,
    gmii_txd => gmii_txd,
    gmii_tx_en => gmii_tx_en,
    gmii_tx_er => gmii_tx_er,
    gmii_rxd => gmii_rxd,
    gmii_rx_dv => gmii_rx_dv,
    gmii_rx_er => gmii_rx_er,
    gmii_isolate => open,
    
    configuration_vector => "10000",
    an_interrupt => open,
    an_adv_config_vector => X"4001",
    an_restart_config => '0',
    link_timer_value => "000110010",
    speed_is_10_100 => speed_is_10_100,
    speed_is_100 => speed_is_100,
    status_vector => status_vector,
    reset => RST,
    signal_detect => signal_detect          
);
    speed_is_10_100 <= not status_vector(11);
    speed_is_100 <= not status_vector(11) and status_vector(10);

RX_CHANNEL: ethernet_rx_channel port map (
    clk => userclk2, -- FIXME clock enable
    rst => RST,
    station_mac => station_mac, -- FIXME on-board configuration??
    rx_data => gmii_rxd,
    rx_en => gmii_rx_dv,
    rx_err => gmii_rx_er,
    fifo_data => rx_fifo_din(7 downto 0),
    fifo_first => rx_fifo_din(8),
    fifo_we => rx_fifo_we
);

RX_FIFO: ethernet_rx_fifo port map (
    rst => RST,
    wr_clk => userclk2,
    rd_clk => clk_125,
    din => rx_fifo_din,
    wr_en => rx_fifo_we,
    rd_en => rx_fifo_re,
    dout => rx_fifo_dout,
    full => rx_fifo_full,
    empty => rx_fifo_empty
);

RX_HANDLER: ethernet_rx_handler port map (
    clk => clk_125,
    rst => RST,
    station_mac => station_mac,
    fifo_data => rx_fifo_dout,
    fifo_empty => rx_fifo_empty,
    fifo_re => rx_fifo_re,
    prog_addr => prog_addr,
    prog_we => prog_we,
    prog_data => prog_data,
    page_block_addr => page_block_addr,
    page_word_addr => page_word_addr,
    page_we => page_we,
    page_lock => page_lock,
    page_data => page_data,
    prog_ok => prog_ok,
    prog_ack => prog_ack,
    prog_nyet => prog_nyet,
    system_reset => system_reset,
    sim_start => sim_start,
    sim_pause => sim_pause
);

NENGO: nengo_rt_tl generic map (SIMULATION => "FALSE") port map (
    clk_125 => clk_125,
    rst => system_reset,
    prog_addr => prog_addr,
    prog_we => prog_we,
    prog_data => prog_data,
    prog_ack => prog_ack,
    prog_nyet => prog_nyet,
    prog_error => open, -- FIXME programming module can't handle this yet
    prog_ok => prog_ok,
    page_block_addr => page_block_addr,
    page_word_addr => page_word_addr,
    page_we => page_we,
    page_lock => page_lock,
    page_data => page_data,
    start => sim_start,
    pause => sim_pause,
    running => sim_running,
    timestep_overflow => timestep_overflow
);

GPIO_LED(0) <= prog_ok;
GPIO_LED(1) <= '0';
GPIO_LED(2) <= '0';
GPIO_LED(3) <= '0';
GPIO_LED(4) <= '0';
GPIO_LED(5) <= '0';
GPIO_LED(6) <= '0';
GPIO_LED(7) <= sim_running;

end architecture TOPLEVEL;