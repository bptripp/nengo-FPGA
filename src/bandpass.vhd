----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    09:28:20 04/30/2013 
-- Design Name: 
-- Module Name:    bandpass - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
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
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

library IEEE_proposed;
use IEEE_proposed.FIXED_PKG.ALL;
use IEEE_proposed.fixed_float_types.all; -- for fixed_truncate


entity bandpass is
    Port ( clk : in  STD_LOGIC;
           valid : in  STD_LOGIC;
           u : in  SFIXED (1 downto -14);
           y : out  SFIXED (1 downto -14)
			 );
end bandpass;

architecture Behavioral of bandpass is

	type ci_type is record
		x1 : sfixed (1 downto -14);
		x2 : sfixed (1 downto -14);
		y  : sfixed (1 downto -14);
	end record;

	signal reg : ci_type := (
		x1 => to_sfixed(0, 1,-14),
		x2 => to_sfixed(0, 1,-14),
		y => to_sfixed(0, 1,-14)
	);

	signal ci_next : ci_type;

begin

COMB: process(reg, valid, u)
	variable ci : ci_type;
	--constant a1 : sfixed (1 downto -14) := to_sfixed(0.5916, 1,-14);
	--constant a2 : sfixed (1 downto -14) := to_sfixed(-0.5289, 1,-14);
	--constant c1 : sfixed (1 downto -14) := to_sfixed(0.4643, 1,-14);
	--constant c2 : sfixed (1 downto -14) := to_sfixed(-1.1970, 1,-14);
	--constant d : sfixed (1 downto -14) := to_sfixed(0.7848, 1,-14);
	
	constant a1 : sfixed (1 downto -14) := to_sfixed(0.3791, 1,-14);
	constant a2 : sfixed (1 downto -14) := to_sfixed(-0.0221, 1,-14);
	constant c1 : sfixed (1 downto -14) := to_sfixed(-0.4512, 1,-14);
	constant c2 : sfixed (1 downto -14) := to_sfixed(-0.1690, 1,-14);
	constant d : sfixed (1 downto -14) := to_sfixed(1.0, 1,-14);
begin
	ci := reg;
	if(valid = '1') then
		ci.x1 := resize(a1*reg.x1 + a2*reg.x2 + u, ci.x1, fixed_saturate, fixed_truncate);
		ci.x2 := reg.x1;
		ci.y := resize(c1*reg.x1 + c2*reg.x2 + d*u, ci.y, fixed_saturate, fixed_truncate);
	end if;
	-- latch ci
	ci_next <= ci;
	-- set outputs
	y <= reg.y;
	-- y <= to_sfixed(0.0, 1,-14); -- optimize out entire filter
end process COMB;

SEQ: process(clk)
begin
	if(rising_edge(clk)) then
		reg <= ci_next;
	end if;
end process SEQ;

end Behavioral;

