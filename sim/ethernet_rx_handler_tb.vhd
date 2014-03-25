library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

library std;
use std.textio.all;

entity ethernet_rx_handler_tb is
end entity;

architecture sim of ethernet_rx_handler_tb is

signal clk_125: std_logic;
constant CLOCK125_PERIOD: time := 8 ns;

constant station_mac: std_logic_vector(47 downto 0) := X"000A35028FC0";

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
    
    prog_ok: in std_logic;
    prog_ack: in std_logic;
    prog_nyet: in std_logic;
    
    page_block_addr: out std_logic_vector(5 downto 0);
    page_word_addr: out std_logic_vector(10 downto 0);
    page_we: out std_logic;
    page_lock: out std_logic;
    page_data: out std_logic_vector(11 downto 0);
    
    system_reset: out std_logic;
    sim_start: out std_logic;
    sim_pause: out std_logic
); end component ethernet_rx_handler;

signal rst: std_logic;
signal fifo_data: std_logic_vector(8 downto 0);
signal fifo_empty: std_logic;
signal fifo_re: std_logic;

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
signal rx_din: std_logic_vector(8 downto 0);
signal rx_we: std_logic;

begin

CLK125GEN: process
begin
    clk_125 <= '0';
    loop
        clk_125 <= '0';
        wait for CLOCK125_PERIOD/2;
        clk_125 <= '1';
        wait for CLOCK125_PERIOD/2;
    end loop;
end process CLK125GEN;

uut: ethernet_rx_handler port map (
	clk => clk_125,
	rst => rst,
	station_mac => station_mac,
	
	fifo_data => fifo_data,
	fifo_empty => fifo_empty,
	fifo_re => fifo_re,
	
	prog_addr => prog_addr,
	prog_we => prog_we,
	prog_data => prog_data,
	
	prog_ok => prog_ok,
	prog_ack => prog_ack,
	prog_nyet => prog_nyet,
	
	page_block_addr => page_block_addr,
	page_word_addr => page_word_addr,
	page_we => page_we,
	page_lock => page_lock,
	page_data => page_data,
	
	system_reset => system_reset,
	sim_start => sim_start,
	sim_pause => sim_pause
);

rx_fifo: ethernet_rx_fifo port map (
	rst => rst,
	wr_clk => clk_125,
	rd_clk => clk_125,
	din => rx_din,
	wr_en => rx_we,
	rd_en => fifo_re,
	dout => fifo_data,
	full => open,
	empty => fifo_empty
);

tb: process
begin
	rst <= '1';
	prog_ok <= '0';
	prog_ack <= '0';
	prog_nyet <= '0';
	rx_din <= (others=>'0');
	rx_we <= '0';
	
	wait for CLOCK125_PERIOD*10;
	rst <= '0';
	wait for CLOCK125_PERIOD*10;
	prog_ok <= '1';
	
	wait until falling_edge(clk_125);
	rx_we <= '1';
	rx_din(8) <= '1'; -- mark first bit
	-- 6 octets DST, 6 octets SRC, 2 octets type (0x88b5)
	rx_din(7 downto 0) <= X"00";  wait for CLOCK125_PERIOD;
	rx_din(8) <= '0';
	rx_din(7 downto 0) <= X"0a";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"35";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"02";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"8f";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"c0";  wait for CLOCK125_PERIOD;
	
	rx_din(7 downto 0) <= X"11";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"22";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"33";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"44";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"55";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"66";  wait for CLOCK125_PERIOD;
	
	rx_din(7 downto 0) <= X"88";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"b5";  wait for CLOCK125_PERIOD;
	-- 1 octet tag
	rx_din(7 downto 0) <= X"42";  wait for CLOCK125_PERIOD;
	-- 1 octet opcode
	rx_din(7 downto 0) <= X"01";  wait for CLOCK125_PERIOD;

	-- PAGE WRITE: 2 octets address, 1 octet pair count
	rx_din(7 downto 0) <= X"00";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"00";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"00";  wait for CLOCK125_PERIOD;
	
	-- 3*pair_count octets of data
	rx_din(7 downto 0) <= X"00";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"00";  wait for CLOCK125_PERIOD;
	rx_din(7 downto 0) <= X"00";  wait for CLOCK125_PERIOD;
	
	rx_we <= '0';

	wait;
end process tb;

end architecture sim;
