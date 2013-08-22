----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05/16/2013 10:33:34 AM
-- Design Name: 
-- Module Name: mux_to_dv_port - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

use work.port_types.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity mux_to_dv_port is
    generic (
        N: integer; -- number of encoders
        -- assuming we are using 256 DV blocks, we have 512 DV ports and therefore need 9 bits to identify a DV port uniquely (port# + dv block#)
        -- (and the remaining 11 bits to identify an address within that DV)
        decode: std_logic_vector(8 downto 0) := "000000000"
    );
    Port ( 
        clk : in STD_LOGIC;
        data: in encoder_addresses(0 to N-1);
        output: out std_logic_vector(10 downto 0);
        selected: out std_logic_vector(N-1 downto 0)
    );
end mux_to_dv_port;

architecture Behavioral of mux_to_dv_port is
    signal valid: std_logic_vector(N-1 downto 0);
    
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
    
    function lowest_valid_one_hot(V: std_logic_vector)
        return std_logic_vector is
        variable retval: std_logic_vector(N-1 downto 0);
    begin
        retval := (others=>'0');
        retval(lowest_valid(V)) := '1';
        -- FIXME this is a temporary hotfix
        if(lowest_valid(V) = 0 and V(0) = '0') then
            retval(0) := '0';
        end if;
        return retval;
    end lowest_valid_one_hot;
begin

    IS_VALID: for I in 0 to N-1 generate
        valid(I) <= '1' when data(I)(19 downto 11) = decode else '0';
    end generate IS_VALID;

    MUX: process (clk, data, valid)
    begin
        if(rising_edge(clk)) then
            output <= data(lowest_valid(valid))(10 downto 0);
            selected <= lowest_valid_one_hot(valid);
        end if;
    end process MUX;

end Behavioral;
