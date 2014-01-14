library ieee;
use ieee.std_logic_1164.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity encoder_unit is port (
    clk: in std_logic;
    rst: in std_logic;
    next_population: in std_logic;
    dv_addr: out std_logic_vector(18 downto 0);
    dv_port: out std_logic;
    dv_data: in std_logic_vector(11 downto 0);
    sum: out sfixed(1 downto -10);
    done: out std_logic;
    we: out std_logic;
    
    prog_ok: in std_logic;
    prog_we: in std_logic;
    prog_data: in std_logic_vector(39 downto 0)
);
end entity;

architecture rtl of encoder_unit is
component encoding_controller Port ( 
    clk : in STD_LOGIC;
    rst : in STD_LOGIC;
    next_population: in std_logic;
    next_insn: out std_logic;
	 no_insns: in std_logic;
    insn_data : in STD_LOGIC_VECTOR (39 downto 0);
    dv_addr : out STD_LOGIC_VECTOR (18 downto 0);
    dv_port : out STD_LOGIC;
    dv_data : in STD_LOGIC_VECTOR (11 downto 0);
    sum: out sfixed(1 downto -10);
    done: out std_logic
); end component;

component instruction_buffer PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(39 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(39 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC
  ); end component;

signal insn: std_logic_vector(39 downto 0);
signal next_insn: std_logic;
signal no_insns: std_logic;
signal encoder_done: std_logic;
signal last_done: std_logic := '0';

signal buffer_din: std_logic_vector(39 downto 0);
signal buffer_we: std_logic;

begin

-- edge detection logic
process(clk, rst, encoder_done)
begin
    if(rising_edge(clk)) then
        if(rst = '1') then
            last_done <= '0';
        else
            last_done <= encoder_done;
        end if;
    end if;
end process;
we <= '1' when (encoder_done = '1' and last_done = '0') else '0';

ENCODER: encoding_controller port map (
    clk => clk,
    rst => rst,
    next_population => next_population,
    next_insn => next_insn,
	 no_insns => no_insns,
    insn_data => insn,
    dv_addr => dv_addr,
    dv_port => dv_port,
    dv_data => dv_data,
    sum => sum,
    done => encoder_done
);
done <= encoder_done;

BUF: instruction_buffer port map (
    clk => clk,
    rst => rst,
    din => buffer_din,
    wr_en => buffer_we,
    rd_en => next_insn,
    dout => insn,
    full => open,
    empty => no_insns
);

-- mux programming input with feedback
buffer_din <= prog_data when prog_ok = '1' else insn;
buffer_we <= prog_we when prog_ok = '1' else next_insn;

end architecture rtl;
