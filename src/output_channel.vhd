library ieee;
use ieee.std_logic_1164.all;

entity output_channel is port (
  clk: in std_logic;
  rst: in std_logic;
  
  start: in std_logic;
  dv_addr: out std_logic_vector(18 downto 0);
  dv_port: out std_logic;
  dv_data: in std_logic_vector(11 downto 0);
  channel_data: out std_logic_vector(11 downto 0);
  channel_we: out std_logic;
  done: out std_logic;

  prog_ok: in std_logic;
  prog_we: in std_logic;
  prog_data: in std_logic_vector(35 downto 0)
);
end entity;

architecture rtl of output_channel is
  component output_channel_controller port (
    clk: in std_logic;
    rst: in std_logic;
    
    start: in std_logic;
    next_insn: out std_logic;
    insn_data: in std_logic_vector(35 downto 0);
	 no_insns: in std_logic;
    dv_addr: out std_logic_vector(18 downto 0);
    dv_port: out std_logic;
    dv_data: in std_logic_vector(11 downto 0);
    channel_data: out std_logic_vector(11 downto 0);
    channel_we: out std_logic;
    done: out std_logic
  ); end component;

  component output_instruction_buffer port (
    clk: in std_logic;
    rst: in std_logic;

    din: in std_logic_vector(35 downto 0);
    wr_en: in std_logic;
    full: out std_logic;

    dout: out std_logic_vector(35 downto 0);
    rd_en: in std_logic;
    empty: out std_logic
  ); end component;
  
  signal insn: std_logic_vector(35 downto 0);
  signal next_insn: std_logic;
  signal no_insns: std_logic;
  
  signal buffer_din: std_logic_vector(35 downto 0);
  signal buffer_we: std_logic;
  
begin

CONTROLLER: output_channel_controller port map (
	clk => clk,
	rst => rst,
	
	start => start,
	next_insn => next_insn,
	insn_data => insn,
	no_insns => no_insns,
	dv_addr => dv_addr,
	dv_port => dv_port,
	dv_data => dv_data,
	channel_data => channel_data,
	channel_we => channel_we,
	done => done
);

INSN_BUF: output_instruction_buffer port map (
	clk => clk,
	rst => rst,
	
	din => buffer_din,
	wr_en => buffer_we,
	full => open,
	
	dout => insn,
	rd_en => next_insn,
	empty => no_insns
);

-- mux programming input with feedback
buffer_din <= prog_data when prog_ok = '1' else insn;
buffer_we <= prog_we when prog_ok = '1' else next_insn;

end architecture rtl;

