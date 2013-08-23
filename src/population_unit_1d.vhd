library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity population_unit_1d is port (
	clk: in std_logic;
	rst: in std_logic;
	timestep: in std_logic;
	all_done: out std_logic; -- from decoder bottom half
	
	-- encoder 0 connection to DV interconnect
	encoder0_dv_addr: out std_logic_vector(18 downto 0);
	encoder0_dv_port: out std_logic;
	encoder0_dv_data: in std_logic_vector(11 downto 0);
	-- encoder 1 connection to DV interconnect
	encoder1_dv_addr: out std_logic_vector(18 downto 0);
	encoder1_dv_port: out std_logic;
	encoder1_dv_data: in std_logic_vector(11 downto 0);
	-- decoder 0 connection to DV block
	decoder0_dv_addr: out std_logic_vector(10 downto 0);
	decoder0_dv_we: out std_logic;
	decoder0_dv_data: out std_logic_vector(11 downto 0);
	-- decoder 1 connection to DV block
	decoder1_dv_addr: out std_logic_vector(10 downto 0);
	decoder1_dv_we: out std_logic;
	decoder1_dv_data: out std_logic_vector(11 downto 0);
	-- decoder 2 connection to DV block
	decoder2_dv_addr: out std_logic_vector(10 downto 0);
	decoder2_dv_we: out std_logic;
	decoder2_dv_data: out std_logic_vector(11 downto 0);
	-- decoder 3 connection to DV block
	decoder3_dv_addr: out std_logic_vector(10 downto 0);
	decoder3_dv_we: out std_logic;	
	decoder3_dv_data: out std_logic_vector(11 downto 0);
	-- programming interface shared addr/data
	prog_ok: in std_logic;
	prog_addr: in std_logic_vector(20 downto 0);
	prog_data: in std_logic_vector(39 downto 0);
	-- programming interface target write-enable
	prog_encoder_we: in std_logic_vector(1 downto 0);
	prog_pc_filter_we: in std_logic_vector(1 downto 0);
	prog_pc_lfsr_we: in std_logic;
	prog_pc_we: in std_logic;
	prog_decoder_memory_we: in std_logic
	
); end entity population_unit_1d;

architecture Behavioural of population_unit_1d is

component encoder_pipeline_controller     generic (
        N: integer -- number of encoders
    );
    port (
        clk: in std_logic;
        encoder_done: in std_logic_vector(N-1 downto 0);
        timestep: in std_logic;
        fifo_full: in std_logic_vector(N-1 downto 0);
        
        encode_next: out std_logic
    ); end component;
signal encoder_next_population: std_logic;

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

component decoder_unit_top_half_1d port (
    clk: in std_logic;
    rst: in std_logic;    
    encoder_fifo_u: in std_logic_vector(11 downto 0);
    encoder_fifo_empty: in std_logic;
    encoder_fifo_rd_en: out std_logic;
    pc0: out std_logic_vector(11 downto 0);
    pc1: out std_logic_vector(11 downto 0);
    pc2: out std_logic_vector(11 downto 0);
    pc3: out std_logic_vector(11 downto 0);
    pc4: out std_logic_vector(11 downto 0);
    pc5: out std_logic_vector(11 downto 0);
    pc6: out std_logic_vector(11 downto 0);
    pc7: out std_logic_vector(11 downto 0);
    pc_valid: out std_logic;
    pc_ack: in std_logic;
    
    -- programming interface
    pc_prog_addr: in std_logic_vector(13 downto 0); -- top bit unused (due to only 7 PCs); 12 downto 10 chooses a PC; 9 downto 0 addresses in PC
    pc_prog_we: in std_logic;
    pc_prog_data: in std_logic_vector(11 downto 0);
    
    normal_prog_addr: in std_logic_vector(1 downto 0);
    normal_prog_we: in std_logic;
    normal_prog_data: in std_logic_vector(31 downto 0)
); end component;
signal decoder_empty: std_logic;
signal decoder_pc0: std_logic_vector(11 downto 0);
signal decoder_pc1: std_logic_vector(11 downto 0);
signal decoder_pc2: std_logic_vector(11 downto 0);
signal decoder_pc3: std_logic_vector(11 downto 0);
signal decoder_pc4: std_logic_vector(11 downto 0);
signal decoder_pc5: std_logic_vector(11 downto 0);
signal decoder_pc6: std_logic_vector(11 downto 0);
signal decoder_pc7: std_logic_vector(11 downto 0);
signal decoder_valid: std_logic;
signal decoder_ack: std_logic;      
                                   
component decoder_unit_bottom_half_1d generic (
    shift: integer := 0;
    skip_count: integer -- for shift register
); port (
	clk: in std_logic;
	rst: in std_logic;

	-- from top-half
	pc0: in std_logic_vector(11 downto 0);
	pc1: in std_logic_vector(11 downto 0);
	pc2: in std_logic_vector(11 downto 0);
	pc3: in std_logic_vector(11 downto 0);
	pc4: in std_logic_vector(11 downto 0);
	pc5: in std_logic_vector(11 downto 0);
	pc6: in std_logic_vector(11 downto 0);
	pc7: in std_logic_vector(11 downto 0);
	pc_ready: in std_logic;
	pc_ack: out std_logic;

	prog_ok: in std_logic;
	prog_addr: in std_logic_vector(5 downto 0); -- top 2 bits select which of the 4 DVs are being decoded; bottom 4 choose a decoder buffer for that DV
	prog_we: in std_logic;
	prog_data: in std_logic_vector(11 downto 0);

	-- DV write ports
	dv0_addr: out std_logic_vector(10 downto 0);
	dv0_we: out std_logic;
	dv0_data: out std_logic_vector(11 downto 0);
	dv1_addr: out std_logic_vector(10 downto 0);
	dv1_we: out std_logic;
	dv1_data: out std_logic_vector(11 downto 0);
	dv2_addr: out std_logic_vector(10 downto 0);
	dv2_we: out std_logic;
	dv2_data: out std_logic_vector(11 downto 0);
	dv3_addr: out std_logic_vector(10 downto 0);
	dv3_we: out std_logic;
	dv3_data: out std_logic_vector(11 downto 0);

	timestep: in std_logic;
	all_done: out std_logic    
); end component;

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
-- FIXME here's where encoder #1 and encoder pipe #1 would go

ENCODER_PIPE_CTRL: encoder_pipeline_controller generic map (N=>1) port map (
	clk => clk,
	encoder_done(0) => encoder0_done,
	timestep => timestep,
	fifo_full(0) => encoder0_fifo_almost_full,
	encode_next => encoder_next_population
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
-- FIXME here's where filter #1 would go

DECODER_TOP_HALF: decoder_unit_top_half_1d port map (
	clk => clk,
	rst => rst,
	encoder_fifo_u => to_slv(filter0_y),
	encoder_fifo_empty => decoder_empty,
	encoder_fifo_rd_en => filter0_ack,
	pc0 => decoder_pc0,
	pc1 => decoder_pc1,
	pc2 => decoder_pc2,
	pc3 => decoder_pc3,
	pc4 => decoder_pc4,
	pc5 => decoder_pc5,
	pc6 => decoder_pc6,
	pc7 => decoder_pc7,
	pc_valid => decoder_valid,
	pc_ack => decoder_ack,
	
	pc_prog_addr => prog_addr(13 downto 0),
	pc_prog_we => prog_pc_we,
	pc_prog_data => prog_data(11 downto 0),
	
	normal_prog_addr => prog_addr(1 downto 0),
	normal_prog_we => prog_pc_lfsr_we,
	normal_prog_data => prog_data(31 downto 0)
);
decoder_empty <= '1' when filter0_ready = '0' else '0';

DECODER_BOTTOM_HALF: decoder_unit_bottom_half_1d generic map (
	shift => 0,
	skip_count => 0
) port map (
	clk => clk,
	rst => rst,
	pc0 => decoder_pc0,
	pc1 => decoder_pc1,
	pc2 => decoder_pc2,
	pc3 => decoder_pc3,
	pc4 => decoder_pc4,
	pc5 => decoder_pc5,
	pc6 => decoder_pc6,
	pc7 => decoder_pc7,
	pc_ready => decoder_valid,
	pc_ack => decoder_ack,
	
	prog_ok => prog_ok,
	prog_addr => prog_addr(5 downto 0),
	prog_we => prog_decoder_memory_we,
	prog_data => prog_data(11 downto 0),
	
	dv0_addr => decoder0_dv_addr,
	dv0_we => decoder0_dv_we,
	dv0_data => decoder0_dv_data,
	dv1_addr => decoder1_dv_addr,
	dv1_we => decoder1_dv_we,
	dv1_data => decoder1_dv_data,
	dv2_addr => decoder2_dv_addr,
	dv2_we => decoder2_dv_we,
	dv2_data => decoder2_dv_data,
	dv3_addr => decoder3_dv_addr,
	dv3_we => decoder3_dv_we,
	dv3_data => decoder3_dv_data,
	
	timestep => timestep,
	all_done => all_done	
);

end architecture Behavioural;
