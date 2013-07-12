----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 07/11/2013 04:57:22 PM
-- Design Name: 
-- Module Name: vanderpol_tl - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

library UNISIM;
use UNISIM.VComponents.all;

entity vanderpol_tl is
    Port (
        SYSCLK_P : in STD_LOGIC;
        SYSCLK_N : in STD_LOGIC;
        SYS_RESET : in STD_LOGIC;
        LED : out STD_LOGIC_VECTOR (7 downto 0);
        SW : in std_logic_vector(7 downto 0);
        BUTTON: in std_logic_vector(4 downto 0)
    );
end vanderpol_tl;

architecture Behavioral of vanderpol_tl is
    signal clk: std_logic;

    component clk_pll
    port
     (-- Clock in ports
      clk_in1_p         : in     std_logic;
      clk_in1_n         : in     std_logic;
      -- Clock out ports
      clk_out1          : out    std_logic
     ); end component;
     
     component vanderpol
      Port ( clk : in STD_LOGIC;
            rst : in STD_LOGIC;
            x1_out : out STD_LOGIC_VECTOR (11 downto 0);
            x2_out : out STD_LOGIC_VECTOR (11 downto 0);
            valid: out std_logic;
            
            pc_prog_sel: in std_logic_vector(3 downto 0);
            pc_prog_addr: in std_logic_vector(9 downto 0);
            pc_prog_we: in std_logic;
            pc_prog_data: in std_logic_vector(11 downto 0)
     ); end component;
     
     signal rst: std_logic;
     signal x1_out: std_logic_vector(11 downto 0);
     signal x2_out: std_logic_vector(11 downto 0);
     signal valid: std_logic;
     
     signal pc_prog_sel: std_logic_vector(3 downto 0);
     signal pc_prog_addr: std_logic_vector(9 downto 0);
     signal pc_prog_we: std_logic;
     signal pc_prog_data: std_logic_vector(11 downto 0);
     
     attribute mark_debug: string;
     attribute mark_debug of rst: signal is "true";
     attribute mark_debug of x1_out: signal is "true";
     attribute mark_debug of x2_out: signal is "true";
     attribute mark_debug of valid: signal is "true";
     
begin

    CLKBUF: clk_pll port map (
        clk_in1_p => SYSCLK_P,
        clk_in1_n => SYSCLK_N,
        clk_out1 => clk
    );
    
    SYNC_RESET: process(clk, SYS_RESET)
    begin
        if(rising_edge(clk)) then
            rst <= SYS_RESET;
        end if;
    end process SYNC_RESET;
    
    SYNC_PROG: process(clk, SW, BUTTON)
    begin
        if(rising_edge(clk)) then
            pc_prog_sel(3) <= BUTTON(2);
            pc_prog_sel(2 downto 0) <= BUTTON(2 downto 0);
            pc_prog_addr(9 downto 2) <= SW;
            pc_prog_addr(1) <= BUTTON(3);
            pc_prog_addr(0) <= '0';
            pc_prog_we <= BUTTON(4);
            pc_prog_data <= X"000";
        end if;
    end process SYNC_PROG;
    
    LED <= x1_out(11 downto 4); -- drop four lowest bits
    
    uut: vanderpol port map (
        clk => clk,
        rst => rst,
        x1_out => x1_out,
        x2_out => x2_out,
        valid => valid,
        pc_prog_sel => pc_prog_sel,
        pc_prog_addr => pc_prog_addr,
        pc_prog_we => pc_prog_we,
        pc_prog_data => pc_prog_data
    );

end Behavioral;
