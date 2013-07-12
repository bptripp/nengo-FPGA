----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/21/2013 04:00:01 PM
-- Design Name: 
-- Module Name: multiplexed_1filter - Behavioral
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

library IEEE_proposed;
use IEEE_proposed.fixed_pkg.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity multiplexed_1filter is
    Port ( clk : in STD_LOGIC;
           rst: in std_logic;
           x : in sfixed (1 downto -16);
           u : in sfixed (1 downto -10);
           valid : in STD_LOGIC;
           x1 : out sfixed (1 downto -16);
           y : out sfixed (1 downto -10);
           ready : out STD_LOGIC; -- connect this to next stage data-valid
           ack: in std_logic; -- connect this to next stage data-acknowledge
           we: out std_logic -- connect this to FIFO acknowledge (assuming FWFT) and FIFO write-enable
           );
end multiplexed_1filter;

architecture Behavioral of multiplexed_1filter is
    -- x(n) = A * x(n-1) + B * u(n)
    -- y(n) = C * x(n-1) + D * u(n)

    -- or alternatively, x(n) = (1 - alpha) * u(n) + (alpha) * x(n-1)
    -- where alpha = e^(-DT/tau), here DT = 0.001s and tau = 0.1s

    constant A: sfixed (1 downto -10) := to_sfixed(0.99005, 1,-10);
    constant B: sfixed (1 downto -10) := to_sfixed(0.00995, 1,-10);
    constant C: sfixed (1 downto -10) := to_sfixed(0.99005, 1,-10);
    constant D: sfixed (1 downto -10) := to_sfixed(0.00995, 1,-10);
    
    signal x_state: sfixed(1 downto -16) := to_sfixed(0, 1,-16);
    signal y_out: sfixed(1 downto -10) := to_sfixed(0, 1,-10);
begin

y <= y_out;
x1 <= x_state;

process(clk, rst, x, u, valid, ack)
begin
    if(rising_edge(clk)) then
        if(ack = '1') then
            ready <= '0';
        end if;
        if(rst = '1') then
            x_state <= to_sfixed(0, 1,-16);
            y_out <= to_sfixed(0, 1,-10);
            ready <= '0';
            we <= '0';
        elsif(valid = '1') then
            x_state <= resize(A*x + B*u, x_state);
            y_out <= resize(C*x + D*u, y_out);
            ready <= '1';
            we <= '1';
        else
            we <= '0'; -- clear after 1 cycle
        end if;
    end if;
end process;

end Behavioral;
