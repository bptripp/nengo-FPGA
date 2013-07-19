library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity filter_state_fifo_reset_controller is port (
    clk: in std_logic;
    rst: in std_logic;
    fifo_rst: out std_logic;
    fifo_full: in std_logic;
    fifo_data: out std_logic_vector(11 downto 0);
    fifo_we: out std_logic;
    
    user_data: in std_logic_vector(11 downto 0);
    user_we: in std_logic
); end entity;

architecture rtl of filter_state_fifo_reset_controller is

type state_type is (state_wait_for_rst, state_wait_for_fifo, state_init_fifo, state_idle);
signal state: state_type := state_wait_for_rst;

signal data: std_logic_vector(11 downto 0) := (others=>'0');
signal we: std_logic := '0';

signal INIT_COUNTER: unsigned(9 downto 0) := "1111111111"; -- 1023
signal counter: unsigned(9 downto 0) := (others=>'0');

begin

fifo_data <= user_data when (state = state_idle) else data;
fifo_we <= user_we when (state = state_idle) else we;

process(clk, rst, state, counter, fifo_full)
    variable next_counter: unsigned(9 downto 0);
    variable next_state: state_type;
    variable next_we: std_logic;
begin
    if(counter = "0000000000") then
        next_counter := counter;
    else
        next_counter := counter - X"1";
    end if;
    next_state := state;
    next_we := '0';
    if(rising_edge(clk)) then
        if(rst = '1') then
            fifo_rst <= '1';
            next_state := state_wait_for_fifo;
        else
            fifo_rst <= '0';
            case state is
                when state_wait_for_rst =>
                    null; -- this is handled as a reset condition.
                when state_wait_for_fifo =>
                    if(fifo_full = '0') then -- FIXME check for glitches on this deassertion, there may be latency
                        next_counter := INIT_COUNTER;
                        next_state := state_init_fifo;
                    end if;
                when state_init_fifo =>
                    next_we := '1';
                    if(counter = "0000000000") then
                        next_state := state_idle;
                    end if;
                when state_idle =>
                    null; -- muxing handled combinatorially
            end case;
        end if;
        counter <= next_counter;
        state <= next_state;
        we <= next_we;
    end if;
end process;

end architecture rtl;