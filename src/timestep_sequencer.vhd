library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity timestep_sequencer is generic (
    SIMULATION: string := "FALSE";
    CLOCKS_PER_TIMESTEP: positive := 125000 -- 1 millisecond * 125 MHz
); 
port (
    clk: in std_logic;
    rst: in std_logic;
    start: in std_logic;
    pause: in std_logic;
    done: in std_logic;
    
    running: out std_logic;
    timestep: out std_logic;
    timestep_overflow: out std_logic
); end entity timestep_sequencer;

architecture rtl of timestep_sequencer is

constant MAX_CLOCKS: unsigned(17 downto 0) := to_unsigned(CLOCKS_PER_TIMESTEP - 1, 18);
constant TIMER_HIGH: unsigned(17 downto 0) := (others=>'1');

type ci_type is record
    timer: unsigned(17 downto 0);
    paused: std_logic; -- i.e. paused during this timestep, so don't trigger another one
    running: std_logic;
    timestep: std_logic;
    timestep_overflow: std_logic;
end record;

constant reg_reset: ci_type := (
    timer => (others=>'0'),
    paused => '0',
    running => '0',
    timestep => '0',
    timestep_overflow => '0'
);

signal reg: ci_type := reg_reset;
signal ci_next: ci_type;

begin

running <= reg.running;
timestep <= reg.timestep;
timestep_overflow <= reg.timestep_overflow;

COMB: process(reg, rst, start, pause, done)
    variable ci: ci_type;
    variable next_timer: unsigned(17 downto 0);
begin
    ci := reg;
    ci.timestep := '0';
    if(reg.timer /= TIMER_HIGH and reg.running = '1') then
        next_timer := reg.timer + "1";
    else
        next_timer := reg.timer;
    end if;
    ci.timer := next_timer;
    
    if(rst = '1') then
        ci := reg_reset;
    else
        if(pause = '1') then
            ci.paused := '1';
        end if;
        if(reg.running = '1' and reg.timestep = '0') then
            if(SIMULATION = "TRUE" or reg.timer >= MAX_CLOCKS) then
                if(done = '1') then
                    ci.timestep_overflow := '0';
                    ci.timer := (others=>'0');
                    if(reg.paused = '1') then
                        ci.running := '0';
                    else                        
                        ci.timestep := '1';
                    end if;
                 elsif(SIMULATION = "FALSE") then
                    ci.timestep_overflow := '1';
                 end if;                    
            end if;              
        else
            if(start = '1') then
                ci.running := '1';
                ci.timestep := '1';
            end if;
        end if;
    end if;
    
    ci_next <= ci;
end process COMB;

SEQ: process(clk, ci_next)
begin
    if(rising_edge(clk)) then
        reg <= ci_next;
    end if;
end process SEQ;

end architecture rtl;
