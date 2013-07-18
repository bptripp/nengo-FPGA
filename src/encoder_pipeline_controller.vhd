library ieee;
use ieee.std_logic_1164.all;

entity encoder_pipeline_controller is
    generic (
        N: integer -- number of encoders
    );
    port (
        clk: in std_logic;
        encoder_done: in std_logic_vector(N-1 downto 0);
        timestep: in std_logic;
        fifo_full: in std_logic_vector(N-1 downto 0);
        
        encode_next: out std_logic
    );
end entity;

architecture rtl of encoder_pipeline_controller is

function or_reduce(V: std_logic_vector)
return std_logic is
    variable result: std_logic;
begin
    for i in V'range loop
        if i = V'left then
            result := V(i);
        else
            result := result OR V(i);
        end if;
        exit when result = '1';
    end loop;
    return result;
end function or_reduce;

function and_reduce(V: std_logic_vector)
return std_logic is
    variable result: std_logic;
begin
    for i in V'range loop
        if i = V'left then
            result := V(i);
        else
            result := result AND V(i);
        end if;
        exit when result = '0';
    end loop;
    return result;
end function and_reduce;

signal all_encoders_done: std_logic;
signal no_fifos_full: std_logic;

type state_type is (state_armed, state_wait);

signal state: state_type := state_armed;

begin

all_encoders_done <= and_reduce(encoder_done);
no_fifos_full <= not or_reduce(fifo_full);

process(clk, state, timestep, all_encoders_done, no_fifos_full)
    variable encode: std_logic;
    variable next_state: state_type;
begin
    encode := '0';
    next_state := state;
    if(rising_edge(clk)) then
        case state is
            when state_armed =>
                if(timestep = '1' or (all_encoders_done = '1' and no_fifos_full = '1')) then
                    encode := '1';
                    next_state := state_wait;
                end if;
            when state_wait =>
                if(all_encoders_done = '0') then
                    next_state := state_armed;
                end if;
        end case;
        encode_next <= encode;
        state <= next_state;
    end if;
end process;

end architecture rtl;