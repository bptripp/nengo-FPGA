library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reset_controller is port (
    clk: in std_logic;
    rst_i: in std_logic;
    rst_o: out std_logic;
    rst_done: out std_logic
);
end entity;

architecture rtl of reset_controller is
    constant RESET_CYCLES: unsigned(10 downto 0) := to_unsigned(1030, 11);
    signal reset_counter: unsigned(10 downto 0) := RESET_CYCLES;
    type state_type is (state_por, state_resetting, state_idle);
    signal state: state_type := state_por;
begin

process(clk, state, rst_i, reset_counter)
    variable next_counter: unsigned(10 downto 0);
    variable next_state: state_type;
begin
    if(reset_counter = to_unsigned(0, 11)) then
        next_counter := reset_counter;
    else
        next_counter := reset_counter - X"1";
    end if;
    next_state := state;
    if(rising_edge(clk)) then
        if(rst_i = '1') then
            next_counter := RESET_CYCLES;
            next_state := state_por;
            rst_o <= '0';
            rst_done <= '0';
        else
            case state is
                when state_por =>
                    rst_o <= '1';
                    rst_done <= '0';
                    next_state := state_resetting;
                when state_resetting =>
                    rst_o <= '0';
                    rst_done <= '0';
                    if(reset_counter = to_unsigned(0, 11)) then
                        next_state := state_idle;
                    end if;
                when state_idle =>
                    rst_o <= '0';
                    rst_done <= '1';
            end case;
        end if;
        reset_counter <= next_counter;
        state <= next_state;
    end if;
end process;

end architecture rtl;