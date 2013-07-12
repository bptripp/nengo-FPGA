----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/23/2013 02:21:37 PM
-- Design Name: 
-- Module Name: delayed_shift_register_tb - Behavioral
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
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity delayed_shift_register_tb is
end delayed_shift_register_tb;

architecture Behavioral of delayed_shift_register_tb is
    constant CLOCK_PERIOD: time := 5 ns;
    signal clk: std_logic;
    
    component delayed_shift_register     
    generic (
        N: integer; -- output width
        T: integer -- number of shifts to ignore
    );
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           din : in STD_LOGIC;
           shift : in STD_LOGIC;
           dout : out STD_LOGIC_VECTOR (N-1 downto 0));
   end component;
   
   signal rst: std_logic;
   signal din: std_logic;
   signal shift: std_logic;
   signal dout: std_logic_vector(7 downto 0);
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
    
    uut: delayed_shift_register
    generic map (
        N => 8,
        T => 8
    ) port map (
        clk => clk,
        rst => rst,
        din => din,
        shift => shift,
        dout => dout
    );
    
    tb: process
    begin
        rst <= '1';
        din <= '0';
        shift <= '0';
        wait for 100 ns;
        rst <= '0';
        wait for CLOCK_PERIOD*3;
        -- now, the next eight shifts should be ignored and the eight shifts after that should happen
        wait until rising_edge(clk);
        rst <= '0';
        shift <= '1';
        wait for CLOCK_PERIOD*8;
        din <= '1';
        wait for CLOCK_PERIOD*8;
        -- all remaining shifts should be ignored
        din <= '0' after 1 ns;
        wait;
    end process tb;

end Behavioral;
