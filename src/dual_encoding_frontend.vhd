library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity dual_encoding_frontend is port (
  clk: in std_logic;
  rst: in std_logic;

  -- encoder 0 connection to DV interconnect
  encoder0_dv_addr: out std_logic_vector(18 downto 0);
  encoder0_dv_port: out std_logic;
  encoder0_dv_data: in std_logic_vector(11 downto 0);
  -- encoder 1 connection to DV interconnect
  encoder1_dv_addr: out std_logic_vector(18 downto 0);
  encoder1_dv_port: out std_logic;
  encoder1_dv_data: in std_logic_vector(11 downto 0);
  
  -- output connection to decoder top-half
  encoded_value: out sfixed(1 downto -10);
  encoded_value_ready: out std_logic;
  encoded_value_ack: in std_logic;

  -- connection to external encoder pipeline controller
  encoder_done: out std_logic_vector(1 downto 0);
  encoder_fifo_almost_full: out std_logic_vector(1 downto 0);
  encoder_next_population: in std_logic;
  
  -- programming interface shared addr/data
  prog_ok: in std_logic;
  prog_addr: in std_logic_vector(20 downto 0);
  prog_data: in std_logic_vector(39 downto 0);
  -- programming interface target write-enable
  prog_encoder_we: in std_logic_vector(1 downto 0);
  prog_pc_filter_we: in std_logic_vector(1 downto 0)
  
); end entity dual_encoding_frontend;

architecture rtl of dual_encoding_frontend is

  component encoder_unit port (
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
    ); end component;
  signal encoder0_sum: sfixed(1 downto -10);
  signal encoder0_done: std_logic;
  signal encoder0_we: std_logic;                       
  signal encoder1_sum: sfixed(1 downto -10);
  signal encoder1_done: std_logic;
  signal encoder1_we: std_logic;     

  component encoder_fifo  PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(11 DOWNTO 0);
    full : OUT STD_LOGIC;
    almost_full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC
    ); end component;
  signal encoder0_re: std_logic;
  signal encoder0_fifo_dout: std_logic_vector(11 downto 0);
  signal encoder0_fifo_full: std_logic;
  signal encoder0_fifo_almost_full: std_logic;
  signal encoder0_fifo_empty: std_logic;
  signal encoder1_re: std_logic;
  signal encoder1_fifo_dout: std_logic_vector(11 downto 0);
  signal encoder1_fifo_full: std_logic;
  signal encoder1_fifo_almost_full: std_logic;
  signal encoder1_fifo_empty: std_logic;
  
  component first_order_filter_unit port (
    clk: in std_logic;
    rst: in std_logic;
    u: in sfixed(1 downto -10);
    valid: in std_logic;
    y: out sfixed(1 downto -10);
    ready: out std_logic;
    ready_stb: out std_logic;
    ack: in std_logic;
    
    prog_addr: in std_logic_vector(1 downto 0);
    prog_we: in std_logic;
    prog_data: in std_logic_vector(11 downto 0)
    ); end component;
  signal filter0_valid: std_logic;
  signal filter0_y: sfixed(1 downto -10);
  signal filter0_ready: std_logic;
  signal filter0_ack: std_logic;
  signal filter1_valid: std_logic;
  signal filter1_y: sfixed(1 downto -10);
  signal filter1_ready: std_logic;
  signal filter1_ack: std_logic;        

  component pipelined_adder port (
    clk: in std_logic;
    rst: in std_logic;
    
    a: in sfixed(1 downto -10);
    a_valid: in std_logic;
    b: in sfixed(1 downto -10);
    b_valid: in std_logic;
    sum: out sfixed(1 downto -10);
    sum_ready: out std_logic;
    sum_ack: in std_logic
    ); end component pipelined_adder;
  signal filter_output: sfixed(1 downto -10);
  signal filter_output_ready: std_logic;
  signal filter_output_empty: std_logic;
  signal filter_output_ack: std_logic;
  
begin

  ENCODER0: encoder_unit port map (
    clk => clk,
    rst => rst,
    next_population => encoder_next_population,
    dv_addr => encoder0_dv_addr,
    dv_port => encoder0_dv_port,
    dv_data => encoder0_dv_data,
    sum => encoder0_sum,
    done => encoder0_done,
    we => encoder0_we,
    
    prog_ok => prog_ok,
    prog_we => prog_encoder_we(0),
    prog_data => prog_data(39 downto 0)
    );
  ENCODER0_PIPE: encoder_fifo port map (
    clk => clk,
    rst => rst,
    din => to_slv(encoder0_sum),
    wr_en => encoder0_we,
    rd_en => encoder0_re,
    dout => encoder0_fifo_dout,
    full => encoder0_fifo_full,
    almost_full => encoder0_fifo_almost_full,
    empty => encoder0_fifo_empty
    );

  ENCODER1: encoder_unit port map (
    clk => clk,
    rst => rst,
    next_population => encoder_next_population,
    dv_addr => encoder1_dv_addr,
    dv_port => encoder1_dv_port,
    dv_data => encoder1_dv_data,
    sum => encoder1_sum,
    done => encoder1_done,
    we => encoder1_we,
    
    prog_ok => prog_ok,
    prog_we => prog_encoder_we(1),
    prog_data => prog_data(39 downto 0)
    );
  ENCODER1_PIPE: encoder_fifo port map (
    clk => clk,
    rst => rst,
    din => to_slv(encoder1_sum),
    wr_en => encoder1_we,
    rd_en => encoder1_re,
    dout => encoder1_fifo_dout,
    full => encoder1_fifo_full,
    almost_full => encoder1_fifo_almost_full,
    empty => encoder1_fifo_empty
    );

  FILTER0: first_order_filter_unit port map (
    clk => clk,
    rst => rst,
    u => to_sfixed(encoder0_fifo_dout, 1,-10),
    valid => filter0_valid,
    y => filter0_y,
    ready => filter0_ready,
    ready_stb => encoder0_re,
    ack => filter0_ack,
    
    prog_addr => prog_addr(1 downto 0),
    prog_we => prog_pc_filter_we(0),
    prog_data => prog_data(11 downto 0)
    );
  filter0_valid <= '1' when (encoder0_fifo_empty = '0') else '0';

  FILTER1: first_order_filter_unit port map (
    clk => clk,
    rst => rst,
    u => to_sfixed(encoder1_fifo_dout, 1,-10),
    valid => filter1_valid,
    y => filter1_y,
    ready => filter1_ready,
    ready_stb => encoder1_re,
    ack => filter1_ack,
    
    prog_addr => prog_addr(1 downto 0),
    prog_we => prog_pc_filter_we(1),
    prog_data => prog_data(11 downto 0)
    );
  filter1_valid <= '1' when (encoder1_fifo_empty = '0') else '0';

  FILTER_OUTPUT_ADDER: pipelined_adder port map (
    clk => clk,
    rst => rst,
    
    a => filter0_y,
    a_valid => filter0_ready,
    b => filter1_y,
    b_valid => filter1_ready,
    sum => encoded_value,
    sum_ready => encoded_value_ready,
    sum_ack => encoded_value_ack
    );
  filter_output_empty <= '1' when (filter_output_ready = '0') else '0';
  filter0_ack <= encoded_value_ack;
  filter1_ack <= encoded_value_ack;
  
  encoder_done(0) <= encoder0_done;
  encoder_done(1) <= encoder1_done;
  
  encoder_fifo_almost_full(0) <= encoder0_fifo_almost_full;
  encoder_fifo_almost_full(1) <= encoder1_fifo_almost_full;
  
end architecture rtl;
