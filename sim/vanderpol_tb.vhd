library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library IEEE_proposed;
use IEEE_proposed.fixed_pkg.all;

entity vanderpol_tb is
end entity;

architecture sim of vanderpol_tb is
    signal clk: std_logic;
    constant CLOCK_PERIOD: time := 5 ns;

    component vanderpol Port ( clk : in STD_LOGIC;
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
end process;

uut: vanderpol port map (
    clk => clk,
    rst => rst,
    x1_out => x1_out,
    x2_out => x2_out,
    valid => valid,
    pc_prog_sel => X"0",
    pc_prog_addr => "0000000000",
    pc_prog_we => '0',
    pc_prog_data => X"000"
);

tb: process
begin
    rst <= '1';
    wait for 50 ns;
    rst <= '0';    
    wait;
end process;

end architecture sim;