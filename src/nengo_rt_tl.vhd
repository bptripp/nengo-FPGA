library ieee;
use ieee.std_logic_1164.all;

entity nengo_rt_tl is port (
    clk: in std_logic;
    rst: in std_logic;    
    -- 4 MSBs are target type:
    -- 0x0: Decoded Value buffers
    -- 0x1: Encoder instruction lists
    -- 0x2: PC filter characteristics and LFSR
    -- 0x3: Principal Component sample space
    -- 0x4: Decoder memory (DDR3)
    -- Addressing LSBs vary by target:
    -- Decoded Value buffers use 19: the highest 8 address an individual DV buffer, and the lowest 11 address within the buffer.
    -- Encoder instruction lists use 9: the 7 highest address a population unit, and the lowest 2 address one of (up to) four individual encoders.
    -- PC filter characteristics use X: the 7 highest address a population unit, and 
    -- Principal components use 21: the 7 highest address all PCs in one population unit, the next 4 address an individual PC, and the lowest 10 address within the PC.
    -- Decoder memory uses X:
    prog_addr: in std_logic_vector(22 downto 0); 
    prog_we: in std_logic; -- active HIGH pulse; ignored while prog_ok is LOW.
    -- Again, the important bits in this field vary depending on what's being programmed:
    -- Decoded value buffers use the lowest 12.
    -- Encoder instruction lists use all 40.
    -- PC filter characteristics use either the lowest 32 (LFSR seeds) or the lowest 12 (everything else).
    -- Principal components use the lowest 12.
    -- Decoder memory uses the lowest 32.
    prog_data: in std_logic_vector(39 downto 0);
    prog_ok: out std_logic; -- HIGH when programming is allowed, i.e. after system reset and before run start
    start: in std_logic -- Pulse HIGH to begin execution. Ignored while prog_ok is LOW.
    );
end entity;

architecture rtl of nengo_rt_tl is

component reset_controller port (
    clk: in std_logic;
    rst_i: in std_logic;
    rst_o: out std_logic;
    rst_done: out std_logic
); end component;
signal system_reset: std_logic;
signal system_reset_done: std_logic;
signal enable_programming: std_logic;
signal run_started: std_logic := '0';

begin

SYS_RST: reset_controller port map (
    clk => clk,
    rst_i => rst,
    rst_o => system_reset,
    rst_done => system_reset_done
);

enable_programming <= '1' when (system_reset_done = '1' and run_started = '0') else '0';
prog_ok <= enable_programming;

end architecture rtl;
