library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity population_unit_1d is port (
	clk: in std_logic;
	rst: in std_logic;
	timestep: in std_logic;
	encoder_done: out std_logic; -- from encoder pipeline controller
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
        
        encode_next: out std_logic;
		  timestep_complete: out std_logic
    ); end component;
signal encoder_next_population: std_logic;
signal encoder_timestep_complete: std_logic;

component dual_encoding_frontend port (
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
  
); end component dual_encoding_frontend;

signal encoded_value: sfixed(1 downto -10);
signal encoded_value_ready: std_logic;
signal encoded_value_empty: std_logic;
signal encoded_value_ack: std_logic;
signal encoder_unit_done: std_logic_vector(1 downto 0);
signal encoder_fifo_almost_full: std_logic_vector(1 downto 0);

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
    pc_valid: out std_logic;
    pc_ack: in std_logic;
    
    -- programming interface
    pc_prog_addr: in std_logic_vector(13 downto 0); -- top bit unused (due to only 7 PCs); 12 downto 10 chooses a PC; 9 downto 0 addresses in PC
    pc_prog_we: in std_logic;
    pc_prog_data: in std_logic_vector(11 downto 0)
); end component;
signal decoder_empty: std_logic;
signal decoder_pc0: std_logic_vector(11 downto 0);
signal decoder_pc1: std_logic_vector(11 downto 0);
signal decoder_pc2: std_logic_vector(11 downto 0);
signal decoder_pc3: std_logic_vector(11 downto 0);
signal decoder_pc4: std_logic_vector(11 downto 0);
signal decoder_pc5: std_logic_vector(11 downto 0);
signal decoder_pc6: std_logic_vector(11 downto 0);
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
	pc_ready: in std_logic;
	pc_ack: out std_logic;

	prog_ok: in std_logic;
	prog_addr: in std_logic_vector(5 downto 0); -- top 2 bits select which of the 4 DVs are being decoded; bottom 4 choose a decoder buffer for that DV
	prog_we: in std_logic;
	prog_data: in std_logic_vector(11 downto 0);

        normal_prog_addr: in std_logic_vector(3 downto 0); -- top 2 bits select a DV; bottom 2 bits select an LFSR
        normal_prog_we: in std_logic;
        normal_prog_data: in std_logic_vector(31 downto 0);

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

ENCODER_PIPE_CTRL: encoder_pipeline_controller generic map (N=>2) port map (
	clk => clk,
	encoder_done => encoder_unit_done,
	timestep => timestep,
	fifo_full => encoder_fifo_almost_full,
	encode_next => encoder_next_population,
	timestep_complete => encoder_timestep_complete
);
encoder_done <= encoder_timestep_complete;

ENCODING_FRONTEND: dual_encoding_frontend port map (
	clk => clk,
	rst => rst,
	encoder0_dv_addr => encoder0_dv_addr,
	encoder0_dv_port => encoder0_dv_port,
	encoder0_dv_data => encoder0_dv_data,
	encoder1_dv_addr => encoder1_dv_addr,
	encoder1_dv_port => encoder1_dv_port,
	encoder1_dv_data => encoder1_dv_data,
	encoded_value => encoded_value,
	encoded_value_ready => encoded_value_ready,
	encoded_value_ack => encoded_value_ack,
	encoder_done => encoder_unit_done,
	encoder_fifo_almost_full => encoder_fifo_almost_full,
	encoder_next_population => encoder_next_population,
	prog_ok => prog_ok,
	prog_addr => prog_addr,
	prog_data => prog_data,
	prog_encoder_we => prog_encoder_we,
	prog_pc_filter_we => prog_pc_filter_we
);

encoded_value_empty <= '1' when (encoded_value_ready = '0') else '0';

DECODER_TOP_HALF: decoder_unit_top_half_1d port map (
	clk => clk,
	rst => rst,
	encoder_fifo_u => to_slv(encoded_value),
	encoder_fifo_empty => encoded_value_empty,
	encoder_fifo_rd_en => encoded_value_ack,
	pc0 => decoder_pc0,
	pc1 => decoder_pc1,
	pc2 => decoder_pc2,
	pc3 => decoder_pc3,
	pc4 => decoder_pc4,
	pc5 => decoder_pc5,
	pc6 => decoder_pc6,
	pc_valid => decoder_valid,
	pc_ack => decoder_ack,
	
	pc_prog_addr => prog_addr(13 downto 0),
	pc_prog_we => prog_pc_we,
	pc_prog_data => prog_data(11 downto 0)
	
);
decoder_empty <= '1' when encoded_value_ready = '0' else '0'; -- FIXME what is this signal used for?

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
	pc_ready => decoder_valid,
	pc_ack => decoder_ack,
	
	prog_ok => prog_ok,
	prog_addr => prog_addr(5 downto 0),
	prog_we => prog_decoder_memory_we,
	prog_data => prog_data(11 downto 0),

   normal_prog_addr => prog_addr(3 downto 0),
	normal_prog_we => prog_pc_lfsr_we,
	normal_prog_data => prog_data(31 downto 0),
	
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
