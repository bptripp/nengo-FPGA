----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/02/2013 01:31:15 PM
-- Design Name: 
-- Module Name: lfsr32 - Behavioral
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

entity lfsr32 is
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           load : in STD_LOGIC_VECTOR (31 downto 0);
           q : out STD_LOGIC_VECTOR (31 downto 0));
end lfsr32;

architecture Behavioral of lfsr32 is
    signal r: std_logic_vector(31 downto 0) := X"00000000";
begin

    lfsr: process(clk, r)
        variable next_bit: std_logic;
    begin
        -- for a 32-bit LFSR, the XNOR taps are at positions 32, 22, 2, 1
        next_bit := r(31) xnor r(21) xnor r(1) xnor r(0);
        if(rising_edge(clk)) then
            if(rst = '1') then
                r <= load;
            else
                r <= r(30 downto 0) & next_bit;
            end if;
        end if;
    end process lfsr;
    
    q <= r;

end Behavioral;
