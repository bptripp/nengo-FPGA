library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity single_population_tb is
end entity;

architecture sim of single_population_tb is
    constant CLOCK_PERIOD: time := 5 ns;
    signal clk: std_logic;
    
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
    signal y: std_logic_vector(11 downto 0);
    signal valid: std_logic;
    
    component single_population_constant_driver     
    generic (
        output: std_logic_vector(11 downto 0) := std_logic_vector(to_sfixed(0.5, 1,-10))
    );
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           u : out STD_LOGIC_VECTOR (11 downto 0);
           ready : out STD_LOGIC;
           valid : in STD_LOGIC); end component;
    
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
    
    uut: single_population port map (
        clk => clk,
        rst => rst,
        ready => ready,
        next_u => next_u,
        y => y,
        valid => valid,
        pc_prog_sel => "000",
        pc_prog_addr => "0000000000",
        pc_prog_we => '0',
        pc_prog_data => X"000"
    );
    
    driver: single_population_constant_driver generic map (
        output => std_logic_vector(to_sfixed(0.5, 1,-10))
    ) port map (
        clk => clk,
        rst => rst,
        u => next_u,
        ready => ready,
        valid => valid
    );
    
    tb: process
    begin
        rst <= '1';
        --ready <= '0';
        --next_u <= std_logic_vector(to_sfixed(0.5, 1,-10));
        wait for CLOCK_PERIOD*5;
        rst <= '0';
        
--        wait for CLOCK_PERIOD*3;
        
--        loop
--            wait until falling_edge(clk);
--            ready <= '1';
--            wait for CLOCK_PERIOD;
--            ready <= '0';
--            wait until valid = '1';
--            wait for CLOCK_PERIOD;
--        end loop;
        
        wait;
    end process tb;
    
end architecture sim;