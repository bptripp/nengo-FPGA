----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/06/2013 09:59:51 AM
-- Design Name: 
-- Module Name: normal - Behavioral
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
use ieee.numeric_std.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity normal is
    Port ( clk : in STD_LOGIC;
           rst : in STD_LOGIC;
           q : out signed (15 downto 0));
end normal;

architecture Behavioral of normal is
    component lfsr32 Port ( 
          clk : in STD_LOGIC;
          rst : in STD_LOGIC;
          load : in STD_LOGIC_VECTOR (31 downto 0);
          q : out STD_LOGIC_VECTOR (31 downto 0));
     end component;
     
     signal x1: std_logic_vector(31 downto 0);
     signal x2: std_logic_vector(31 downto 0);
     signal x3: std_logic_vector(31 downto 0);
     signal x4: std_logic_vector(31 downto 0);
     
     signal y1: signed(14 downto 0);
     signal y2: signed(14 downto 0);
     
begin

    gen1: lfsr32 port map (
        clk => clk,
        rst => rst,
        load => X"7782691f",
        q => x1
    );
    gen2: lfsr32 port map (
        clk => clk,
        rst => rst,
        load => X"deadbeef",
        q => x2 
    );
    gen3: lfsr32 port map (
        clk => clk,
        rst => rst,
        load => X"abadd00d",
        q => x3 
    );
    gen4: lfsr32 port map (
        clk => clk,
        rst => rst,
        load => X"d0cf11e0",
        q => x4 
    );
    
    y1 <= signed(x1(13) & x1(13 downto 0)) + signed(x2(13 downto 0));
    y2 <= signed(x3(13) & x3(13 downto 0)) + signed(x4(13 downto 0));
    q <= (y1(14) & y1) + y2;

end Behavioral;
