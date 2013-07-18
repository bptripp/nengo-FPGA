library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity nengo_rt_tb is
end entity;

architecture sim of nengo_rt_tb is
signal clk: std_logic;
constant CLOCK_PERIOD: time := 8 ns;

component nengo_rt_tl port (
    clk: in std_logic;
    rst: in std_logic;    
    prog_addr: in std_logic_vector(22 downto 0); 
    prog_we: in std_logic; -- active HIGH pulse; ignored while prog_ok is LOW.
    prog_data: in std_logic_vector(39 downto 0);
    prog_ok: out std_logic; -- HIGH when programming is allowed, i.e. after system reset and before run start
    start: in std_logic -- Pulse HIGH to begin execution. Ignored while prog_ok is LOW.
    ); end component;

signal rst: std_logic;
signal prog_addr: std_logic_vector(22 downto 0);
signal prog_we: std_logic;
signal prog_data: std_logic_vector(39 downto 0);
signal prog_ok: std_logic;
signal start: std_logic;

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

uut: nengo_rt_tl port map (
    clk => clk,
    rst => rst,
    prog_addr => prog_addr,
    prog_we => prog_we,
    prog_data => prog_data,
    prog_ok => prog_ok,
    start => start
);

tb: process
begin
    rst <= '1';
    prog_addr <= (others=>'0');
    prog_we <= '0';
    prog_data <= (others=>'0');
    start <= '0';
    
    wait for CLOCK_PERIOD*3;
    rst <= '0';
    wait until prog_ok = '1';
    
    wait;
end process tb;

end architecture sim;