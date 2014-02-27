library ieee;
use ieee.std_logic_1164.all;

entity ethernet_tx_handler_tb is
end entity;

architecture sim of ethernet_tx_handler_tb is

constant CLOCK_PERIOD: time := 5 ns;
signal clk: std_logic;
signal rst: std_logic;

component output_channel port (
	  clk: in std_logic;
	  rst: in std_logic;
	  
	  start: in std_logic;
	  dv_addr: out std_logic_vector(18 downto 0);
	  dv_port: out std_logic;
	  dv_data: in std_logic_vector(11 downto 0);
	  channel_data: out std_logic_vector(11 downto 0);
	  channel_we: out std_logic;
	  done: out std_logic;

	  prog_ok: in std_logic;
	  prog_we: in std_logic;
	  prog_data: in std_logic_vector(35 downto 0)
); end component;
signal start: std_logic;
signal done: std_logic;
signal prog_ok: std_logic;
signal prog_we: std_logic;
signal prog_data: std_logic_vector(35 downto 0);

component output_channel_fifo port (
	clk: in std_logic;
  rst: in std_logic;
  din: in std_logic_vector(11 downto 0);
  wr_en: in std_logic;
  rd_en: in std_logic;
  dout: out std_logic_vector(11 downto 0);
  full: out std_logic;
  empty: out std_logic;
  data_count: out std_logic_vector(10 downto 0);
  prog_full: out std_logic
); end component;

signal fifo_din: std_logic_vector(11 downto 0);
signal fifo_we: std_logic;
signal fifo_re: std_logic;
signal fifo_dout: std_logic_vector(11 downto 0);
signal fifo_full: std_logic;
signal fifo_empty: std_logic;
signal fifo_data_count: std_logic_vector(10 downto 0);
signal fifo_prog_full: std_logic;

component ethernet_tx_handler port (
  clk: in std_logic;
  rst: in std_logic;
  station_mac: in std_logic_vector(47 downto 0);

  -- channel side
  ch_fifo_data: in std_logic_vector(11 downto 0);
  ch_fifo_re: out std_logic;
  ch_fifo_count: in std_logic_vector(10 downto 0);
  ch_fifo_frame_ready: in std_logic;    -- programmable full flag = 512
  ch_done: in std_logic;
  -- TX FIFO side
  tx_fifo_data: out std_logic_vector(7 downto 0);
  tx_fifo_last: out std_logic;
  tx_fifo_we: out std_logic;
  tx_fifo_full: in std_logic
); end component;

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

component ethernet_tx_channel port (
	clk: in std_logic;
	rst: in std_logic;
	data: in std_logic_vector(7 downto 0);
	tx_en: in std_logic; -- to TX FIFO !empty
	tx_last: in std_logic;
	rd_en: out std_logic; -- to TX FIFO read enable
	MAC_TD: out std_logic_vector(7 downto 0);
	MAC_TX_EN: out std_logic;
	MAC_TX_ERR: out std_logic
); end component;

signal station_mac: std_logic_vector(47 downto 0) := X"000A35028FC0";
signal tx_fifo_data: std_logic_vector(7 downto 0);
signal tx_fifo_last: std_logic;
signal tx_fifo_we: std_logic;
signal tx_fifo_full: std_logic;

signal tx_chan_data: std_logic_vector(7 downto 0);
signal tx_chan_last: std_logic;
signal tx_chan_re: std_logic;
signal tx_fifo_empty: std_logic;

signal tx_en: std_logic;
signal gmii_txd: std_logic_vector(7 downto 0);
signal gmii_tx_en: std_logic;
signal gmii_tx_er: std_logic;

begin

CLKGEN: process
begin
	clk <= '0';
	loop
		clk <= '0';
		wait for CLOCK_PERIOD/2;
		clk <= '1';
		wait for CLOCK_PERIOD/2;
	end loop;
end process CLKGEN;

OUTCHAN: output_channel port map (
	clk => clk,
	rst => rst,
	start => start,
	dv_addr => open,
	dv_port => open,
	dv_data => X"ABC",
	channel_data => fifo_din,
	channel_we => fifo_we,
	done => done,
	prog_ok => prog_ok,
	prog_we => prog_we,
	prog_data => prog_data
);

OUTPUT_FIFO: output_channel_fifo port map (
	clk => clk,
	rst => rst,
	din => fifo_din,
	wr_en => fifo_we,
	rd_en => fifo_re,
	dout => fifo_dout,
	full => fifo_full,
	empty => fifo_empty,
	data_count => fifo_data_count,
	prog_full => fifo_prog_full
);

uut: ethernet_tx_handler port map (
	clk => clk,
	rst => rst,
	station_mac => station_mac,
	ch_fifo_data => fifo_dout,
	ch_fifo_re => fifo_re,
	ch_fifo_count => fifo_data_count,
	ch_fifo_frame_ready => fifo_prog_full,
	ch_done => done,
	tx_fifo_data => tx_fifo_data,
	tx_fifo_last => tx_fifo_last,
	tx_fifo_we => tx_fifo_we,
	tx_fifo_full => tx_fifo_full
);

TX_FIFO: ethernet_rx_fifo port map (
	rst => rst,
	wr_clk => clk,
	rd_clk => clk,
	din(8) => tx_fifo_last,
	din(7 downto 0) => tx_fifo_data,
	wr_en => tx_fifo_we,
	rd_en => tx_chan_re,
	dout(8) => tx_chan_last,
	dout(7 downto 0) => tx_chan_data,
	full => tx_fifo_full,
	empty => tx_fifo_empty
);
tx_en <= not tx_fifo_empty;

TX_CHANNEL: ethernet_tx_channel port map (
	clk => clk,
	rst => rst,
	data => tx_chan_data,
	tx_en => tx_en,
	tx_last => tx_chan_last,
	rd_en => tx_chan_re,
	MAC_TD => gmii_txd,
	MAC_TX_EN => gmii_tx_en,
	MAC_TX_ERR => gmii_tx_er
);

tb: process
begin
	rst <= '1';
	start <= '0';	
	prog_ok <= '0';
	prog_we <= '0';
	prog_data <= (others=>'0');	
	wait for CLOCK_PERIOD*5;
	rst <= '0';
	wait for CLOCK_PERIOD*3;
	
	wait until falling_edge(clk);
	prog_ok <= '1';
	
	prog_we <= '1';
	-- read 12 words from DV buffer 0 port 0, starting at address 4
	--prog_data <= "1" & "0" & "00000000" & "00000000100" & "00000001011" & "0000";
	--wait for CLOCK_PERIOD;
	
	prog_data <= "001100000000000000000000000000000000";
	wait for CLOCK_PERIOD;
	prog_data <= "100000000010000000000000000000000000";
	wait for CLOCK_PERIOD;
	
	prog_we <= '0';
	prog_ok <= '0';
	
	wait for CLOCK_PERIOD*3;
	
	loop
		start <= '1';
		wait for CLOCK_PERIOD;
		start <= '0';
		wait until tx_fifo_last = '1';
		wait for CLOCK_PERIOD*3;
	end loop;
	
	wait;
end process tb;

end architecture sim;
