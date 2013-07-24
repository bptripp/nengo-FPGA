library ieee;
use ieee.std_logic_1164.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity pipelined_adder is port (
    clk: in std_logic;
    rst: in std_logic;
    
    a: in sfixed(1 downto -10);
    a_valid: in std_logic;
    b: in sfixed(1 downto -10);
    b_valid: in std_logic;
    sum: out sfixed(1 downto -10);
    sum_ready: out std_logic;
    sum_ack: in std_logic
); end entity;

architecture rtl of pipelined_adder is
    signal wait_for_ack: std_logic := '0';
begin

process(clk, rst, a, a_valid, b, b_valid, sum_ack, wait_for_ack)
    variable sum_v: sfixed(1 downto -10);
begin
    sum_v := resize(a + b, sum_v);
    if(rising_edge(clk)) then
        sum <= sum_v;            
        if(rst = '1') then
            sum_ready <= '0';
            wait_for_ack <= '0';
        else
            if(wait_for_ack = '1') then
                if(sum_ack = '1') then
                    sum_ready <= '0';
                    wait_for_ack <= '0';
                end if;
            else
                if(a_valid = '1' and b_valid = '1') then
                    sum_ready <= '1';
                    wait_for_ack <= '1';
                end if;
            end if;
        end if;
    end if;
end process;

end architecture rtl;
