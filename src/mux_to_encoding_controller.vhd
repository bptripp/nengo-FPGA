----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/16/2013 04:17:47 PM
-- Design Name: 
-- Module Name: mux_to_encoding_controller - Behavioral
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

use work.port_types.all;

entity mux_to_encoding_controller is
    generic(
        N: integer -- number of DV data ports
    );
    Port ( 
        clk : in STD_LOGIC;
        data: in dv_data(N-1 downto 0);
        sel: in std_logic_vector(N-1 downto 0);
        output: out std_logic_vector(11 downto 0)
    );
end mux_to_encoding_controller;

architecture Behavioral of mux_to_encoding_controller is
    signal last_selected: integer range 0 to N-1 := 0;
    
    -- FIXME this may need to be improved for high clock rates with large fan-in
    function lowest_valid (V : std_logic_vector)
        return integer is
    begin
        for I in V'range loop
            if(V(I) = '1') then
                return I;
            end if;
        end loop;
        return 0;
    end lowest_valid;
begin
    
    SELECTION: process(clk, sel)
    begin
        if(rising_edge(clk)) then
            last_selected <= lowest_valid(sel);
        end if;
    end process SELECTION;

    MUX: process(clk, data, last_selected)
    begin
        if(rising_edge(clk)) then
            output <= data(last_selected);
        end if;
    end process MUX;

end Behavioral;
