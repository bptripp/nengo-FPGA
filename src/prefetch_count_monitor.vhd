library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity prefetch_count_monitor is port (
    clk: in std_logic;
    rst: in std_logic;
    issue_read: in std_logic;
    rx_read: in std_logic;
    count: out unsigned(5 downto 0)
); end entity prefetch_count_monitor;

architecture rtl of prefetch_count_monitor is
    signal counter: unsigned(5 downto 0) := (others=>'0');
begin

count <= counter;

COUNT_MONITOR: process(clk, rst, issue_read, rx_read)
begin
    if(rising_edge(clk)) then
        if(rst = '1') then
            counter <= (others=>'0');
        elsif(issue_read = '1' and rx_read = '0') then
            counter <= counter + X"1";
        elsif(rx_read = '1' and issue_read = '0') then
            counter <= counter - X"1";
        end if;
    end if;
end process COUNT_MONITOR;

end architecture rtl;
