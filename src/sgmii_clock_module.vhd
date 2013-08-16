library ieee;
use ieee.std_logic_1164.all;

library UNISIM;
use UNISIM.VComponents.all;

entity sgmii_clock_module is port (
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
    
); end entity sgmii_clock_module;

architecture SevenSeries of sgmii_clock_module is
    signal clk_200_buf: std_logic;
    signal clk_125_buf: std_logic;
    signal clkout0: std_logic;
    signal clkout1: std_logic;
    signal clkfbout: std_logic;
begin

SYSCLKBUF: IBUFGDS port map (
    I => SYSCLK_P,
    IB => SYSCLK_N,
    O => clk_200_buf
);
clk_200 <= clk_200_buf;

INDEPENDENT_CLOCKBUF: BUFG port map (
    I => clk_200_buf,
    O => independent_clock
);

SGMIICLKBUF: IBUFDS_GTE2 port map (
    CEB => '0',
    I => SGMIICLK_P,
    IB => SGMIICLK_N,
    O => mgtrefclk,
    ODIV2 => open
);

SGMII_MMCM: MMCME2_ADV generic map (
    BANDWIDTH => "OPTIMIZED",
    CLKOUT4_CASCADE => false,
    COMPENSATION => "ZHOLD",
    STARTUP_WAIT => false,
    DIVCLK_DIVIDE => 1,
    CLKFBOUT_MULT_F => 16.000,
    CLKFBOUT_PHASE => 0.000,
    CLKFBOUT_USE_FINE_PS => false,
    CLKOUT0_DIVIDE_F => 8.000,
    CLKOUT0_PHASE => 0.000,
    CLKOUT0_DUTY_CYCLE => 0.5,
    CLKOUT0_USE_FINE_PS => false,
    CLKOUT1_DIVIDE => 16,
    CLKOUT1_PHASE => 0.000,
    CLKOUT1_DUTY_CYCLE => 0.5,
    CLKOUT1_USE_FINE_PS => false,
    CLKIN1_PERIOD => 16.0,
    REF_JITTER1 => 0.010
) port map (
    CLKFBOUT => clkfbout,
    CLKFBOUTB => open,
    CLKOUT0 => clkout0,
    CLKOUT0B => open,
    CLKOUT1 => clkout1,
    CLKOUT1B => open,
    CLKOUT2 => open,
    CLKOUT2B => open,
    CLKOUT3 => open,
    CLKOUT3B => open,
    CLKOUT4 => open,
    CLKOUT5 => open,
    CLKOUT6 => open,
    CLKFBIN => clkfbout,
    CLKIN1 => txoutclk,
    CLKIN2 => '0',
    CLKINSEL => '1',
    DADDR => "0000000",
    DCLK => '0',
    DEN => '0',
    DI => X"0000",
    DO => open,
    DRDY => open,
    DWE => '0',
    PSCLK => '0',
    PSEN => '0',
    PSINCDEC => '0',
    PSDONE => open,
    LOCKED => mmcm_locked,
    CLKINSTOPPED => open,
    CLKFBSTOPPED => open,
    PWRDWN => '0',
    RST => mmcm_reset
);

USERCLKBUF: BUFG port map (
    I => clkout1,
    O => userclk
);

USERCLK2BUF: BUFG port map (
    I => clkout0,
    O => clk_125_buf
);
userclk2 <= clk_125_buf;
clk_125 <= clk_125_buf;

end architecture SevenSeries;