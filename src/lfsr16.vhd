----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/01/2013 01:51:18 PM
-- Design Name: 
-- Module Name: lfsr16 - Behavioral
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

entity lfsr16 is
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           q : out STD_LOGIC_VECTOR (15 downto 0));
end lfsr16;

architecture Behavioral of lfsr16 is
    signal r: std_logic_vector(15 downto 0) := X"0000";
begin

    lfsr: process(clk)
        variable next_bit: std_logic;
    begin
        -- for a 16-bit LFSR, the XNOR taps are at positions 16, 15, 13, 4
        next_bit := r(15) xnor r(14) xnor r(12) xnor r(3);
        if(rising_edge(clk)) then
            if(rst = '1') then
                r <= X"0000";
            else
                r <= r(14 downto 0) & next_bit;
            end if;
        end if;
    end process lfsr;
    
    q <= r;

end Behavioral;
