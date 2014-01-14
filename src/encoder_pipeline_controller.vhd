library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity encoder_pipeline_controller is
    generic (
        N: integer -- number of encoders
    );
    port (
        clk: in std_logic;
        -- FIXME should probably have a reset?
        encoder_done: in std_logic_vector(N-1 downto 0);
        timestep: in std_logic;
        fifo_full: in std_logic_vector(N-1 downto 0);
        
        encode_next: out std_logic;
		  timestep_complete: out std_logic
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
signal timestep_done: std_logic := '0'; -- registered timestep_complete

type state_type is (state_next_timestep, state_armed, state_wait);
signal state: state_type := state_next_timestep;

signal encode_count: unsigned(9 downto 0) := (others=>'1');

begin

all_encoders_done <= and_reduce(encoder_done);
no_fifos_full <= not or_reduce(fifo_full);
timestep_complete <= timestep_done and all_encoders_done;

process(clk, state, timestep, all_encoders_done, no_fifos_full, encode_count, timestep_done)
    variable encode: std_logic;
    variable next_state: state_type;
    variable next_encode_count: unsigned(9 downto 0);
	 variable done: std_logic;
begin
    encode := '0';
    next_state := state;
    next_encode_count := encode_count;
	 done := timestep_done;
    if(rising_edge(clk)) then
        case state is
            when state_next_timestep =>
                if(timestep = '1') then
						  done := '0';
                    encode := '1';
                    next_state := state_wait;
                    next_encode_count := "1111111111";
                end if;
            when state_armed =>
                if(all_encoders_done = '1' and no_fifos_full = '1') then
                    next_encode_count := encode_count - X"1";
                    encode := '1';
                    next_state := state_wait;
                end if;
            when state_wait =>
                if(all_encoders_done = '0') then
                    if(encode_count = "0000000000") then
                        next_state := state_next_timestep;
								done := '1';
                    else                        
                        next_state := state_armed;
                    end if;
                end if;
        end case;
        encode_next <= encode;
        encode_count <= next_encode_count;
        state <= next_state;
		  timestep_done <= done;
    end if;
end process;

end architecture rtl;