----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/06/2013 10:25:10 AM
-- Design Name: 
-- Module Name: single_population_tl - Behavioral
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

entity single_population_tl is
    Port ( SYSCLK_P : in STD_LOGIC;
           SYSCLK_N : in STD_LOGIC;
           SYS_RESET : in STD_LOGIC;
           LED : out STD_LOGIC_VECTOR (7 downto 0);
           SW : in std_logic_vector(7 downto 0);
           BUTTON: in std_logic_vector(4 downto 0)
           );
end single_population_tl;

architecture Behavioral of single_population_tl is

    signal clk: std_logic;

    component clk_pll
    port
     (-- Clock in ports
      clk_in1_p         : in     std_logic;
      clk_in1_n         : in     std_logic;
      -- Clock out ports
      clk_out1          : out    std_logic
     ); end component;

    component single_population Port ( 
               clk : in STD_LOGIC;
               rst : in STD_LOGIC;
               ready: in std_logic;
               next_u : in STD_LOGIC_VECTOR (11 downto 0);
               y : out STD_LOGIC_VECTOR (11 downto 0);
               valid: out std_logic;
               
               pc_prog_sel: in std_logic_vector(2 downto 0);
               pc_prog_addr: in std_logic_vector(9 downto 0);
               pc_prog_we: in std_logic;
               pc_prog_data: in std_logic_vector(11 downto 0)
               ); end component;
    signal rst: std_logic;
    signal ready: std_logic;
    signal next_u: std_logic_vector(11 downto 0);
    
    type NinetySixOutputs is array(0 to 95) of std_logic_vector(11 downto 0);
    --signal y: std_logic_vector(11 downto 0);
    signal y: NinetySixOutputs;
    --signal valid: std_logic;
    signal valid: std_logic_vector(95 downto 0);
        
    signal pc_prog_sel: std_logic_vector(2 downto 0);
    signal pc_prog_addr: std_logic_vector(9 downto 0);
    signal pc_prog_we: std_logic;
    signal pc_prog_data: std_logic_vector(11 downto 0);
    
    attribute mark_debug: string;
    attribute mark_debug of rst: signal is "true";
    attribute mark_debug of next_u: signal is "true";
    attribute mark_debug of y: signal is "true";
    attribute mark_debug of valid: signal is "true";
    
    component single_population_constant_driver     
    generic (
        output: std_logic_vector(11 downto 0) := std_logic_vector(to_sfixed(0.5, 1,-10))
    );
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           u : out STD_LOGIC_VECTOR (11 downto 0);
           ready : out STD_LOGIC;
           valid : in STD_LOGIC                     
           ); end component;

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
            pc_prog_sel <= BUTTON(2 downto 0);
            pc_prog_addr(9 downto 2) <= SW;
            pc_prog_addr(1) <= BUTTON(3);
            pc_prog_addr(0) <= '0';
            pc_prog_we <= BUTTON(4);
            pc_prog_data <= X"000";
        end if;
    end process SYNC_PROG;
    
    LED <= y(0)(11 downto 4); -- drop four lowest bits

    NINETY_SIX_INTEGRATORS: for I in 0 to 95 generate
        uut: single_population port map (
            clk => clk,
            rst => rst,
            ready => ready,
            next_u => next_u,
            y => y(I),
            valid => valid(I),
                    
            pc_prog_sel => pc_prog_sel,
            pc_prog_addr => pc_prog_addr,
            pc_prog_we => pc_prog_we,
            pc_prog_data => pc_prog_data
        );
    end generate;
    
    driver: single_population_constant_driver generic map (
        output => std_logic_vector(to_sfixed(0.5, 1,-10))
    ) port map (
        clk => clk,
        rst => rst,
        u => next_u,
        ready => ready,
        valid => valid(0) -- good enough
    );

end Behavioral;
