----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06/05/2013 03:21:54 PM
-- Design Name: 
-- Module Name: single_population_constant_driver - Behavioral
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

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity single_population_constant_driver is
    generic (
        output: std_logic_vector(11 downto 0) := std_logic_vector(to_sfixed(0.5, 1,-10))
    );
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           u : out STD_LOGIC_VECTOR (11 downto 0);
           ready : out STD_LOGIC;
           valid : in STD_LOGIC);
end single_population_constant_driver;

architecture Behavioral of single_population_constant_driver is
    signal pulse: std_logic := '0';
    signal resetting: std_logic := '0';
begin

    u <= output;
    ready <= pulse;
    
    -- if we're asserting "ready", deassert it (so it remains asserted for one cycle total);
    -- if we're not asserting it, assert it the cycle after "valid" is asserted
    process(clk, rst, valid, pulse, resetting)
    begin
        if(rising_edge(clk)) then
            if(rst = '1') then
                pulse <= '0';
                resetting <= '1';
            elsif(resetting = '1') then
                resetting <= '0';
                pulse <= '1'; 
            elsif(pulse = '1') then
                pulse <= '0';
                resetting <= '0';
            elsif(valid = '1') then            
                pulse <= '1';
                resetting <= '0';
            else
                pulse <= '0';
                resetting <= '0';
            end if;
        end if;
    end process;

end Behavioral;
