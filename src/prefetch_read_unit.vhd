library ieee;
use ieee.std_logic_1164.all;

entity prefetch_read_unit is port (
    clk: in std_logic;
    invalidate: in std_logic;            
    ddr3_read_data: in std_logic_vector(511 downto 0);
    ddr3_read_valid: in std_logic;
    rx_read: out std_logic;    
    fifo_we: out std_logic;
    fifo_data: out std_logic_vector(511 downto 0)
); end entity prefetch_read_unit;

architecture rtl of prefetch_read_unit is
begin

READ_REG: process(clk, ddr3_read_data, ddr3_read_valid, invalidate)
begin
    if(rising_edge(clk)) then
        if(ddr3_read_valid = '1' and invalidate = '0') then
            rx_read <= '1';
            fifo_we <= '1';
            fifo_data <= ddr3_read_data;
        else
            rx_read <= '0';
            fifo_we <= '0';
            fifo_data <= (others=>'0');
        end if;
    end if;
end process READ_REG;

end architecture rtl;
