library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

library std;
use std.textio.all;

use work.port_types.all;

entity nengo_rt_tl is generic (
    SIMULATION: string := "FALSE"
); port (
    clk_125: in std_logic;
    rst: in std_logic;    
    -- 3 MSBs are target type:
    -- 0x0: Decoded Value buffers
    -- 0x1: Encoder instruction lists
    -- 0x2: PC filter characteristics
    -- 0x3: PC LFSRs
    -- 0x4: Principal Component sample space
    -- 0x5: Decoder memory (circular buffers)
    -- 0x6: Output channel instruction lists
    -- 0x7: not used
    -- Addressing LSBs vary by target:
    -- Decoded Value buffers use 19: the highest 8 address an individual DV buffer,
    -- and the lowest 11 address within the buffer.
    -- Encoder instruction lists use 9: the 7 highest address a population unit, and the lowest 2 address
    -- one of (up to) four individual encoders.
    -- PC filter characteristics use 11: the 7 highest address a population unit, the next 2 bits address
    -- one of the four first-order filters,
    -- and the lowest 2 bits address the A,B,C,D coefficients.
    -- PC LFSRs use 9: the 7 highest address a population unit, and the lowest 2 bits address one of the four LFSRs.
    -- Principal components use 21: the 7 highest address all PCs in one population unit, the next 4 address
    -- an individual PC, and the lowest 10 address within the PC.
    -- Decoder memory uses 13: the 7 highest address a population unit,
    -- the next 2 choose which of the 4 DVs is being decoded, 
    -- and the lowest 4 address one of the 16 DV decoder circular buffers.
	 -- Output channel instruction lists use 7, which directly address one of 128 (maximum) output channels.
    prog_addr: in std_logic_vector(23 downto 0); 
    prog_we: in std_logic; -- active HIGH pulse; ignored while prog_ok is LOW.
    -- Again, the important bits in this field vary depending on what's being programmed:
    -- Decoded value buffers use the lowest 12.
    -- Encoder instruction lists use all 40.
    -- PC filter characteristics use the lowest 12.
    -- PC LFSRs use the lowest 32.
    -- Principal components use the lowest 12.
    -- Decoder memory uses the lowest 12.
	 -- Output channel instruction lists use the lowest 36.
    prog_data: in std_logic_vector(39 downto 0);
    prog_ack: out std_logic;
    prog_nyet: out std_logic;
    prog_error: out std_logic;
    prog_ok: out std_logic; -- HIGH when programming is allowed, i.e. after system reset and before run start
    page_block_addr: in std_logic_vector(5 downto 0);
    page_word_addr: in std_logic_vector(10 downto 0);
    page_we: in std_logic;
    page_lock: in std_logic;
    page_data: in std_logic_vector(11 downto 0);
    start: in std_logic; -- Pulse HIGH to begin execution. Ignored while prog_ok is LOW.
    pause: in std_logic; -- Pulse HIGH to pause execution after current timestep. If start also asserted
                         -- on same timestep, single-step the simulation.
    running: out std_logic;
    timestep_overflow: out std_logic; -- Strobed HIGH when a timeout has occurred.
	 
	 output0_data: out std_logic_vector(11 downto 0);
	 output0_we: out std_logic;
	 output0_done: out std_logic
    );
end entity;

architecture rtl of nengo_rt_tl is

constant POPULATION_UNITS_1D: integer := 2;
constant POPULATION_UNITS_2D: integer := 1;
constant NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS: integer := 2*(POPULATION_UNITS_1D + POPULATION_UNITS_2D);
constant NUMBER_OF_DV_BANKS: integer := NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS + 1;
constant NUMBER_OF_INPUT_DV_BANKS: integer := NUMBER_OF_DV_BANKS - NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS;
constant NUMBER_OF_OUTPUT_CHANNELS: integer := 1; -- must be less than or equal to NUMBER_OF_ENCODERS

constant NUMBER_OF_ENCODERS: integer := 2*POPULATION_UNITS_1D + 4*POPULATION_UNITS_2D;

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

attribute mark_debug: string;
attribute keep: string;

component synchronizer   generic(
    G_INIT_VALUE    : std_logic := '0'; -- initial value of all flip-flops in the module
    G_NUM_GUARD_FFS : positive  := 1);  -- number of guard flip-flops after the synchronizing flip-flop
  port(
    i_reset : in  std_logic;            -- asynchronous, high-active
    i_clk   : in  std_logic;            -- destination clock
    i_data  : in  std_logic;
    o_data  : out std_logic); end component synchronizer;

component reset_controller port (
    clk: in std_logic;
    rst_i: in std_logic;
    rst_o: out std_logic;
    rst_done: out std_logic
); end component;
signal system_reset: std_logic;
signal system_reset_done: std_logic;
signal enable_programming: std_logic;
signal run_started: std_logic;

type ci_type is record
    -- write-enable lines for programmable components; number of lines is determined by number of address bits decoded in prog_addr
    -- (not all of these may be connected to components depending on how many population units, etc. are instantiated)
    prog_dv_we: std_logic_vector(255 downto 0);
    prog_encoder_we: std_logic_vector(511 downto 0);
    prog_pc_filter_we: std_logic_vector(511 downto 0);
    prog_pc_lfsr_we: std_logic_vector(127 downto 0);
    prog_pc_we: std_logic_vector(127 downto 0);
    prog_decoder_memory_we: std_logic_vector(127 downto 0);
	 prog_output_channel_we: std_logic_vector(127 downto 0);
    -- registered programming address and data
    prog_reg_addr: std_logic_vector(20 downto 0); -- 3 MSBs already decoded into prog_*_we
    prog_reg_data: std_logic_vector(39 downto 0);    
    -- response signal
    resp_ack: std_logic;
    resp_nyet: std_logic;
    resp_error: std_logic;
end record;

constant reg_reset: ci_type := (
    prog_dv_we => (others=>'0'),
    prog_encoder_we => (others=>'0'),
    prog_pc_filter_we => (others=>'0'),
    prog_pc_lfsr_we => (others=>'0'),
    prog_pc_we => (others=>'0'),
    prog_decoder_memory_we => (others=>'0'),
	 prog_output_channel_we => (others=>'0'),
    prog_reg_addr => (others=>'0'),
    prog_reg_data => (others=>'0'),
    resp_ack => '0',
    resp_nyet => '0',
    resp_error => '0'
);
signal reg: ci_type := reg_reset;
signal ci_next: ci_type;

component timestep_sequencer generic (
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
); end component;
signal all_decoders_done: std_logic;
signal timestep_done: std_logic; -- decoders done and outputs done
signal timestep: std_logic; attribute mark_debug of timestep: signal is "true";
signal timestep_overflow_stb: std_logic;

component mux_to_dv_port     generic (
        N: integer; -- number of encoders
        -- assuming we are using 256 DV blocks, we have 512 DV ports and therefore need 9 bits to identify a DV port uniquely (port# + dv block#)
        -- (and the remaining 11 bits to identify an address within that DV)
        decode: std_logic_vector(8 downto 0) := "000000000"
    );
    Port ( 
        clk : in STD_LOGIC;
        data: in encoder_addresses(0 to N-1);
        output: out std_logic_vector(10 downto 0);
        selected: out std_logic_vector(N-1 downto 0)
    ); end component;
    
component mux_to_encoding_controller generic(
            N: integer -- number of DV data ports
        );
        Port ( 
            clk : in STD_LOGIC;
            data: in dv_data(N-1 downto 0);
            sel: in std_logic_vector(N-1 downto 0);
            output: out std_logic_vector(11 downto 0)
        ); end component;    

component dv_double_buffer port (
        clk: in std_logic;
        rst: in std_logic;
        swap_banks: in std_logic; -- active-high strobe
        -- read-only port 0
        rd0_addr: in std_logic_vector(10 downto 0);
        rd0_data: out std_logic_vector(11 downto 0);
        -- read-only port 1
        rd1_addr: in std_logic_vector(10 downto 0);
        rd1_data: out std_logic_vector(11 downto 0);
        -- write-only port 0
        wr0_addr: in std_logic_vector(10 downto 0);
        wr0_we: in std_logic;
        wr0_data: in std_logic_vector(11 downto 0);
        -- write-only port 1
        wr1_addr: in std_logic_vector(10 downto 0);
        wr1_we: in std_logic;
        wr1_data: in std_logic_vector(11 downto 0);
                
        -- programming interface
        prog_ok: in std_logic; -- when 1, ignores both write-only ports and allows writes from the programming port to address both banks simultaneously
        prog_addr: in std_logic_vector(10 downto 0);
        prog_we: in std_logic;
        prog_data: in std_logic_vector(11 downto 0)
); end component;


signal swap_banks: std_logic_vector(NUMBER_OF_DV_BANKS-1 downto 0);
type dv_addr_type is array(0 to NUMBER_OF_DV_BANKS-1) of std_logic_vector(10 downto 0);
signal dv_rd0_addr: dv_addr_type;
signal dv_rd0_data: dv_data(NUMBER_OF_DV_BANKS-1 downto 0);
signal dv_rd1_addr: dv_addr_type;
signal dv_rd1_data: dv_data(NUMBER_OF_DV_BANKS-1 downto 0);
signal dv_wr0_addr: dv_addr_type;
signal dv_wr0_we: std_logic_vector(NUMBER_OF_DV_BANKS-1 downto 0);
signal dv_wr0_data: dv_data(NUMBER_OF_DV_BANKS-1 downto 0);
signal dv_wr1_addr: dv_addr_type;
signal dv_wr1_we: std_logic_vector(NUMBER_OF_DV_BANKS-1 downto 0);
signal dv_wr1_data: dv_data(NUMBER_OF_DV_BANKS-1 downto 0);

signal page_en: std_logic_vector(63 downto 0); -- for input DV banks
signal page_wr_en: std_logic_vector(63 downto 0); -- page_we qualified with page_en(N)

component bank_lock port (
    clk: in std_logic;
    rst: in std_logic;
    en: in std_logic;
    lock: in std_logic;
    timestep: in std_logic;
    swap_banks: out std_logic
); end component;


signal encoder_addr: encoder_addresses(0 to NUMBER_OF_ENCODERS-1);
signal encoder_data: dv_data(0 to NUMBER_OF_ENCODERS-1);

type encoder_selection_type is array(0 to 2*NUMBER_OF_DV_BANKS-1) of std_logic_vector(NUMBER_OF_ENCODERS-1 downto 0);
signal encoder_select: encoder_selection_type; -- the idea is that encoder_select(X)(Y) is asserted when mux_to_dv_port for DV port X got addressed by encoder Y
type reverse_encoder_selection_type is array(0 to NUMBER_OF_ENCODERS-1) of std_logic_vector(2*NUMBER_OF_DV_BANKS-1 downto 0);
signal reverse_encoder_select: reverse_encoder_selection_type; -- effectively, reverse_encoder_select(X) is the concatenation of encoder_select(*)(X)

-- addresses provided from output channel
signal output_channel_dv_addr: encoder_addresses(0 to NUMBER_OF_OUTPUT_CHANNELS-1);
-- duplicates encoder data
signal output_channel_dv_data: dv_data(0 to NUMBER_OF_OUTPUT_CHANNELS-1);
-- encoder_addr_to_mux is the signal that actually connects to each mux_to_dv_port component;
-- the first (NUMBER_OF_OUTPUT_CHANNELS) of these are pre-muxed between encoder_addr and output_channel_addr,
-- and the remaining ones are wired directly to encoder_addr
signal encoder_addr_to_mux: encoder_addresses(0 to NUMBER_OF_ENCODERS-1);

component output_channel port (
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
); end component;
-- actual data/we path leaving output channel
signal output_channel_data: dv_data(0 to NUMBER_OF_OUTPUT_CHANNELS-1);
signal output_channel_we: std_logic_vector(NUMBER_OF_OUTPUT_CHANNELS-1 downto 0);

signal output_done: std_logic_vector(NUMBER_OF_OUTPUT_CHANNELS-1 downto 0);
signal all_outputs_done: std_logic;

component encoder_output_coordinator port (
  clk: in std_logic;
  rst: in std_logic;
  all_encoders_done: in std_logic;
  all_outputs_done: in std_logic;
  mux_select: out std_logic;            -- 0 selects encoders, 1 selects outputs
  output_start: out std_logic           -- strobed for 1 cycle to signal outputs                                      
); end component;
signal encoder_output_mux_select: std_logic;
signal output_start: std_logic;

component population_unit_1d port (
	clk: in std_logic;
	rst: in std_logic;
	timestep: in std_logic;
	encoder_done: out std_logic;
	all_done: out std_logic; -- from decoder bottom half.
	
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
); end component population_unit_1d;

component population_unit_2d port (
	clk: in std_logic;
	rst: in std_logic;
	timestep: in std_logic;
	encoder_done: out std_logic; -- from encoder pipeline controller
	all_done: out std_logic; -- from decoder bottom half
	
	-- X dimension encoder#0,1
	
	-- encoder 0 connection to DV interconnect
	encoder0_dv_addr: out std_logic_vector(18 downto 0);
	encoder0_dv_port: out std_logic;
	encoder0_dv_data: in std_logic_vector(11 downto 0);
	-- encoder 1 connection to DV interconnect
	encoder1_dv_addr: out std_logic_vector(18 downto 0);
	encoder1_dv_port: out std_logic;
	encoder1_dv_data: in std_logic_vector(11 downto 0);
	
	-- Y dimension encoder#2,3
	
	-- encoder 2 connection to DV interconnect
	encoder2_dv_addr: out std_logic_vector(18 downto 0);
	encoder2_dv_port: out std_logic;
	encoder2_dv_data: in std_logic_vector(11 downto 0);
	-- encoder 3 connection to DV interconnect
	encoder3_dv_addr: out std_logic_vector(18 downto 0);
	encoder3_dv_port: out std_logic;
	encoder3_dv_data: in std_logic_vector(11 downto 0);
	
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
	prog_encoder_we: in std_logic_vector(3 downto 0);
	prog_pc_filter_we: in std_logic_vector(3 downto 0);
	prog_pc_lfsr_we: in std_logic;
	prog_pc_we: in std_logic;
	prog_decoder_memory_we: in std_logic
	
); end component population_unit_2d;


constant NUMBER_OF_POPULATION_UNITS: integer := POPULATION_UNITS_1D + POPULATION_UNITS_2D;
signal encoder_done: std_logic_vector(NUMBER_OF_POPULATION_UNITS-1 downto 0);
signal all_encoders_done: std_logic;
signal all_done: std_logic_vector(NUMBER_OF_POPULATION_UNITS-1 downto 0); -- FIXME this should be renamed something like "decoder_done" or "population_done"



begin

SYS_RST: reset_controller port map (
    clk => clk_125,
    rst_i => rst,
    rst_o => system_reset,
    rst_done => system_reset_done
);

enable_programming <= '1' when (system_reset_done = '1' and run_started = '0') else '0';
prog_ok <= enable_programming;
running <= run_started;
prog_ack <= reg.resp_ack;
prog_nyet <= reg.resp_nyet;
prog_error <= reg.resp_error;

COMB: process(reg, system_reset, prog_addr, prog_we, prog_data, enable_programming)
    variable ci: ci_type;
    variable prog_addr_type: std_logic_vector(2 downto 0);
    variable prog_addr_sub: std_logic_vector(20 downto 0);

    variable dv_index: unsigned(7 downto 0);
    variable encoder_index: unsigned(8 downto 0);
    variable pc_filter_index: unsigned(8 downto 0);
    variable pc_lfsr_index: unsigned(6 downto 0);
    variable pc_index: unsigned(6 downto 0);
    variable decoder_index: unsigned(6 downto 0);
	 variable output_channel_index: unsigned(6 downto 0);

begin
    ci := reg;
    -- self-clearing
    ci.prog_dv_we := (others=>'0');
    ci.prog_encoder_we := (others=>'0');
    ci.prog_pc_filter_we := (others=>'0');
    ci.prog_pc_lfsr_we := (others=>'0');
    ci.prog_pc_we := (others=>'0');
    ci.prog_decoder_memory_we := (others=>'0');
	 ci.prog_output_channel_we := (others=>'0');
    ci.resp_ack := '0';
    ci.resp_nyet := '0';
    ci.resp_error := '0';
    
    prog_addr_type := prog_addr(23 downto 21);
    prog_addr_sub := prog_addr(20 downto 0);
    
    dv_index := unsigned(prog_addr_sub(18 downto 11));
    encoder_index := unsigned(prog_addr_sub(8 downto 0));
    pc_filter_index := unsigned(prog_addr_sub(10 downto 2));
    pc_lfsr_index := unsigned(prog_addr_sub(8 downto 2));
    pc_index := unsigned(prog_addr_sub(20 downto 14));
    decoder_index := unsigned(prog_addr_sub(12 downto 6));
	 output_channel_index := unsigned(prog_addr_sub(6 downto 0));
    
    if(system_reset = '1') then
        ci := reg_reset;
    else
        if(prog_we = '1') then
            if(enable_programming = '1') then
                ci.prog_reg_addr := prog_addr_sub;
                ci.prog_reg_data := prog_data;
                -- decode prog_addr_type
                case prog_addr_type is
                    when "000" => -- DV buffer
                        ci.prog_dv_we(to_integer(dv_index)) := '1';
                        ci.resp_ack := '1';
                    when "001" => -- encoder
                        ci.prog_encoder_we(to_integer(encoder_index)) := '1';
                        ci.resp_ack := '1';
                    when "010" => -- PC filter
                        ci.prog_pc_filter_we(to_integer(pc_filter_index)) := '1';
                        ci.resp_ack := '1';
                    when "011" => -- PC LFSR
                        ci.prog_pc_lfsr_we(to_integer(pc_lfsr_index)) := '1';
                        ci.resp_ack := '1';
                    when "100" => -- PC samples
                        ci.prog_pc_we(to_integer(pc_index)) := '1';
                        ci.resp_ack := '1';
                    when "101" => -- decoder memory      
                        ci.prog_decoder_memory_we(to_integer(decoder_index)) := '1';
                        ci.resp_ack := '1';
						  when "110" => -- output channel
						      ci.prog_output_channel_we(to_integer(output_channel_index)) := '1';
							   ci.resp_ack := '1';
                    when others =>
                        ci.resp_error := '1';
                end case;
            else    
                ci.resp_nyet := '1';
            end if;
        end if;
    end if;
    
    ci_next <= ci;
end process COMB;                         

SEQ: process(clk_125, ci_next)
begin
    if(rising_edge(clk_125)) then
        reg <= ci_next;
    end if;
end process SEQ;


SEQUENCER: timestep_sequencer generic map (
    SIMULATION => SIMULATION,
    -- SIMULATION => "TRUE", -- CHEATING to get faster runs
    CLOCKS_PER_TIMESTEP => 125000
) port map (
    clk => clk_125,
    rst => system_reset,
    start => start,
    pause => pause,
    done => timestep_done,
    
    running => run_started,
    timestep => timestep,
    timestep_overflow => timestep_overflow_stb
);

timestep_done <= all_decoders_done and all_outputs_done;

-- FIXME BLATANT CHEATING to stop XST from optimizing most of the design out
timestep_overflow <= timestep_overflow_stb or dv_rd0_data(0)(0) or dv_rd0_data(0)(1) or dv_rd0_data(0)(2) or dv_rd0_data(0)(3)
or dv_rd0_data(0)(4) or dv_rd0_data(0)(5) or dv_rd0_data(0)(6) or dv_rd0_data(0)(7) or dv_rd0_data(0)(8) or dv_rd0_data(0)(9)
or dv_rd0_data(0)(10) or dv_rd0_data(0)(11);

DECODE_PAGE_EN: for I in 0 to 63 generate
    page_en(I) <= '1' when (page_block_addr = std_logic_vector(to_unsigned(I, 6))) else '0';
    page_wr_en(I) <= page_en(I) and page_we;
end generate;

DEFAULT_SWAP_BANKS: for I in 0 to NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS-1 generate 
    swap_banks(I) <= timestep;
end generate;
INPUT_SWAP_BANKS: for I in NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS to NUMBER_OF_DV_BANKS-1 generate
    LOCK: bank_lock port map (
        clk => clk_125,
        rst => system_reset,
        en => page_en(I-NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS), 
        lock => page_lock,
        timestep => timestep,
        swap_banks => swap_banks(I)
    );
end generate;

REVERSE_ENCODER_OUTER: for X in 0 to NUMBER_OF_ENCODERS-1 generate
    REVERSE_ENCODER_INNER: for Y in 0 to 2*NUMBER_OF_DV_BANKS-1 generate
        reverse_encoder_select(X)(Y) <= encoder_select(Y)(X);
    end generate;
end generate;

-- input banks are addressed starting at 192
INPUT_BANKS: for I in 0 to NUMBER_OF_INPUT_DV_BANKS-1 generate
	INPUT_BANK: dv_double_buffer port map (
		 clk => clk_125,
		 rst => system_reset,
		 swap_banks => swap_banks(NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS + I),
		 rd0_addr => dv_rd0_addr(NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS + I),
		 rd0_data => dv_rd0_data(NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS + I),
		 rd1_addr => dv_rd1_addr(NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS + I),
		 rd1_data => dv_rd1_data(NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS + I),
		 wr0_addr => page_word_addr,
		 wr0_we => page_wr_en(I),
		 wr0_data => page_data,
		 wr1_addr => "00000000000",
		 wr1_we => '0',
		 wr1_data => X"000",
		 prog_ok => enable_programming,
		 prog_addr => reg.prog_reg_addr(10 downto 0),
		 prog_we => reg.prog_dv_we(192 + I),
		 prog_data => reg.prog_reg_data(11 downto 0)
	);
	MUX_TO_INPUT_DV_PORT0: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 -- decode => "0" & X"C0"
		 decode => "0" & std_logic_vector(to_unsigned(192 + I, 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr_to_mux,
		 output => dv_rd0_addr(NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS + I),
		 selected => encoder_select(NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS + I)
	);
	MUX_TO_INPUT_DV_PORT1: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 -- decode => "0" & X"C0"
		 decode => "1" & std_logic_vector(to_unsigned(192 + I, 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr_to_mux,
		 output => dv_rd1_addr(NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS + I),
		 selected => encoder_select(NUMBER_OF_DV_BANKS + NUMBER_OF_DV_BANKS_RESERVED_FOR_POPULATION_UNITS + I)
	);
end generate;

POPULATIONS_1D: for I in 0 to POPULATION_UNITS_1D-1 generate
	POPULATION_UNIT: population_unit_1d port map (
		clk => clk_125,
		rst => system_reset,
		timestep => timestep,
		encoder_done => encoder_done(I),
		all_done => all_done(I),
				
		encoder0_dv_addr => encoder_addr(2*I)(18 downto 0),
		encoder0_dv_port => encoder_addr(2*I)(19),
		encoder0_dv_data => encoder_data(2*I),		
		encoder1_dv_addr => encoder_addr(2*I+1)(18 downto 0),
		encoder1_dv_port => encoder_addr(2*I+1)(19),
		encoder1_dv_data => encoder_data(2*I+1),
		
		decoder0_dv_addr => dv_wr0_addr(2*I),
		decoder0_dv_we => dv_wr0_we(2*I),
		decoder0_dv_data => dv_wr0_data(2*I),
		decoder1_dv_addr => dv_wr1_addr(2*I),
		decoder1_dv_we => dv_wr1_we(2*I),
		decoder1_dv_data => dv_wr1_data(2*I),
		decoder2_dv_addr => dv_wr0_addr(2*I+1),
		decoder2_dv_we => dv_wr0_we(2*I+1),
		decoder2_dv_data => dv_wr0_data(2*I+1),
		decoder3_dv_addr => dv_wr1_addr(2*I+1),
		decoder3_dv_we => dv_wr1_we(2*I+1),
		decoder3_dv_data => dv_wr1_data(2*I+1),
		
		prog_ok => enable_programming,
		prog_addr => reg.prog_reg_addr,
		prog_data => reg.prog_reg_data,
		prog_encoder_we => reg.prog_encoder_we(4*I+1 downto 4*I), -- 2 and 3 not used
		prog_pc_filter_we => reg.prog_pc_filter_we(4*I+1 downto 4*I), -- 2 and 3 not used
		prog_pc_lfsr_we => reg.prog_pc_lfsr_we(I),
		prog_pc_we => reg.prog_pc_we(I),
		prog_decoder_memory_we => reg.prog_decoder_memory_we(I)
	);
	MUX_TO_ENCODER0: mux_to_encoding_controller generic map (
		N => 2*NUMBER_OF_DV_BANKS
	) port map (
		 clk => clk_125,
		 data(NUMBER_OF_DV_BANKS-1 downto 0) => dv_rd0_data,
		 data(2*NUMBER_OF_DV_BANKS - 1 downto NUMBER_OF_DV_BANKS) => dv_rd1_data,
		 sel => reverse_encoder_select(2*I),
		 output => encoder_data(2*I)
	);
	MUX_TO_ENCODER1: mux_to_encoding_controller generic map (
		N => 2*NUMBER_OF_DV_BANKS
	) port map (
		 clk => clk_125,
		 data(NUMBER_OF_DV_BANKS-1 downto 0) => dv_rd0_data,
		 data(2*NUMBER_OF_DV_BANKS - 1 downto NUMBER_OF_DV_BANKS) => dv_rd1_data,
		 sel => reverse_encoder_select(2*I+1),
		 output => encoder_data(2*I+1)
	);
	DV_BANK0: dv_double_buffer port map (
		 clk => clk_125,
		 rst => system_reset,
		 swap_banks => swap_banks(2*I),
		 rd0_addr => dv_rd0_addr(2*I),
		 rd0_data => dv_rd0_data(2*I),
		 rd1_addr => dv_rd1_addr(2*I),
		 rd1_data => dv_rd1_data(2*I),
		 wr0_addr => dv_wr0_addr(2*I),
		 wr0_we => dv_wr0_we(2*I),
		 wr0_data => dv_wr0_data(2*I),
		 wr1_addr => dv_wr1_addr(2*I),
		 wr1_we => dv_wr1_we(2*I),
		 wr1_data => dv_wr1_data(2*I),
		 prog_ok => enable_programming,
		 prog_addr => reg.prog_reg_addr(10 downto 0),
		 prog_we => reg.prog_dv_we(2*I),
		 prog_data => reg.prog_reg_data(11 downto 0)
	);
	MUX_TO_DV_PORT0_BANK0: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 decode => "0" & std_logic_vector(to_unsigned(2*I, 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr_to_mux,
		 output => dv_rd0_addr(2*I),
		 selected => encoder_select(2*I)
	);
	MUX_TO_DV_PORT1_BANK0: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 decode => "1" & std_logic_vector(to_unsigned(2*I, 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr_to_mux,
		 output => dv_rd1_addr(2*I),
		 selected => encoder_select(NUMBER_OF_DV_BANKS + 2*I)
	);
	DV_BANK1: dv_double_buffer port map (
		 clk => clk_125,
		 rst => system_reset,
		 swap_banks => swap_banks(2*I+1),
		 rd0_addr => dv_rd0_addr(2*I+1),
		 rd0_data => dv_rd0_data(2*I+1),
		 rd1_addr => dv_rd1_addr(2*I+1),
		 rd1_data => dv_rd1_data(2*I+1),
		 wr0_addr => dv_wr0_addr(2*I+1),
		 wr0_we => dv_wr0_we(2*I+1),
		 wr0_data => dv_wr0_data(2*I+1),
		 wr1_addr => dv_wr1_addr(2*I+1),
		 wr1_we => dv_wr1_we(2*I+1),
		 wr1_data => dv_wr1_data(2*I+1),
		 prog_ok => enable_programming,
		 prog_addr => reg.prog_reg_addr(10 downto 0),
		 prog_we => reg.prog_dv_we(2*I+1),
		 prog_data => reg.prog_reg_data(11 downto 0)
	);
	MUX_TO_DV_PORT0_BANK1: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 decode => "0" & std_logic_vector(to_unsigned(2*I+1, 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr_to_mux,
		 output => dv_rd0_addr(2*I+1),
		 selected => encoder_select(2*I+1)
	);
	MUX_TO_DV_PORT1_BANK1: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 decode => "1" & std_logic_vector(to_unsigned(2*I+1, 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr_to_mux,
		 output => dv_rd1_addr(2*I+1),
		 selected => encoder_select(NUMBER_OF_DV_BANKS + 2*I+1)
	);
end generate;

POPULATIONS_2D: for I in 0 to POPULATION_UNITS_2D-1 generate
	POPULATION_UNIT: population_unit_2d port map (
		clk => clk_125,
		rst => system_reset,
		timestep => timestep,
		encoder_done => encoder_done(POPULATION_UNITS_1D+I),
		all_done => all_done(POPULATION_UNITS_1D+I),
				
		encoder0_dv_addr => encoder_addr(2*POPULATION_UNITS_1D + 4*I)(18 downto 0),
		encoder0_dv_port => encoder_addr(2*POPULATION_UNITS_1D + 4*I)(19),
		encoder0_dv_data => encoder_data(2*POPULATION_UNITS_1D + 4*I),		
		encoder1_dv_addr => encoder_addr(2*POPULATION_UNITS_1D + 4*I+1)(18 downto 0),
		encoder1_dv_port => encoder_addr(2*POPULATION_UNITS_1D + 4*I+1)(19),
		encoder1_dv_data => encoder_data(2*POPULATION_UNITS_1D + 4*I+1),		
		encoder2_dv_addr => encoder_addr(2*POPULATION_UNITS_1D + 4*I+2)(18 downto 0),
		encoder2_dv_port => encoder_addr(2*POPULATION_UNITS_1D + 4*I+2)(19),
		encoder2_dv_data => encoder_data(2*POPULATION_UNITS_1D + 4*I+2),		
		encoder3_dv_addr => encoder_addr(2*POPULATION_UNITS_1D + 4*I+3)(18 downto 0),
		encoder3_dv_port => encoder_addr(2*POPULATION_UNITS_1D + 4*I+3)(19),
		encoder3_dv_data => encoder_data(2*POPULATION_UNITS_1D + 4*I+3),
		
		decoder0_dv_addr => dv_wr0_addr(2*(POPULATION_UNITS_1D+I)),
		decoder0_dv_we => dv_wr0_we(2*(POPULATION_UNITS_1D+I)),
		decoder0_dv_data => dv_wr0_data(2*(POPULATION_UNITS_1D+I)),
		decoder1_dv_addr => dv_wr1_addr(2*(POPULATION_UNITS_1D+I)),
		decoder1_dv_we => dv_wr1_we(2*(POPULATION_UNITS_1D+I)),
		decoder1_dv_data => dv_wr1_data(2*(POPULATION_UNITS_1D+I)),
		decoder2_dv_addr => dv_wr0_addr(2*(POPULATION_UNITS_1D+I)+1),
		decoder2_dv_we => dv_wr0_we(2*(POPULATION_UNITS_1D+I)+1),
		decoder2_dv_data => dv_wr0_data(2*(POPULATION_UNITS_1D+I)+1),
		decoder3_dv_addr => dv_wr1_addr(2*(POPULATION_UNITS_1D+I)+1),
		decoder3_dv_we => dv_wr1_we(2*(POPULATION_UNITS_1D+I)+1),
		decoder3_dv_data => dv_wr1_data(2*(POPULATION_UNITS_1D+I)+1),
		
		prog_ok => enable_programming,
		prog_addr => reg.prog_reg_addr,
		prog_data => reg.prog_reg_data,
		prog_encoder_we => reg.prog_encoder_we(4*(95-I)+3 downto 4*(95-I)),
		prog_pc_filter_we => reg.prog_pc_filter_we(4*(95-I)+3 downto 4*(95-I)),
		prog_pc_lfsr_we => reg.prog_pc_lfsr_we((95-I)),
		prog_pc_we => reg.prog_pc_we((95-I)),
		prog_decoder_memory_we => reg.prog_decoder_memory_we((95-I))
	);
	MUX_TO_ENCODER0: mux_to_encoding_controller generic map (
		N => 2*NUMBER_OF_DV_BANKS
	) port map (
		 clk => clk_125,
		 data(NUMBER_OF_DV_BANKS-1 downto 0) => dv_rd0_data,
		 data(2*NUMBER_OF_DV_BANKS - 1 downto NUMBER_OF_DV_BANKS) => dv_rd1_data,
		 sel => reverse_encoder_select(2*POPULATION_UNITS_1D + 4*I),
		 output => encoder_data(2*POPULATION_UNITS_1D + 4*I)
	);
	MUX_TO_ENCODER1: mux_to_encoding_controller generic map (
		N => 2*NUMBER_OF_DV_BANKS
	) port map (
		 clk => clk_125,
		 data(NUMBER_OF_DV_BANKS-1 downto 0) => dv_rd0_data,
		 data(2*NUMBER_OF_DV_BANKS - 1 downto NUMBER_OF_DV_BANKS) => dv_rd1_data,
		 sel => reverse_encoder_select(2*POPULATION_UNITS_1D + 4*I+1),
		 output => encoder_data(2*POPULATION_UNITS_1D + 4*I+1)
	);
	MUX_TO_ENCODER2: mux_to_encoding_controller generic map (
		N => 2*NUMBER_OF_DV_BANKS
	) port map (
		 clk => clk_125,
		 data(NUMBER_OF_DV_BANKS-1 downto 0) => dv_rd0_data,
		 data(2*NUMBER_OF_DV_BANKS - 1 downto NUMBER_OF_DV_BANKS) => dv_rd1_data,
		 sel => reverse_encoder_select(2*POPULATION_UNITS_1D + 4*I+2),
		 output => encoder_data(2*POPULATION_UNITS_1D + 4*I+2)
	);
	MUX_TO_ENCODER3: mux_to_encoding_controller generic map (
		N => 2*NUMBER_OF_DV_BANKS
	) port map (
		 clk => clk_125,
		 data(NUMBER_OF_DV_BANKS-1 downto 0) => dv_rd0_data,
		 data(2*NUMBER_OF_DV_BANKS - 1 downto NUMBER_OF_DV_BANKS) => dv_rd1_data,
		 sel => reverse_encoder_select(2*POPULATION_UNITS_1D + 4*I+3),
		 output => encoder_data(2*POPULATION_UNITS_1D + 4*I+3)
	);
	DV_BANK0: dv_double_buffer port map (
		 clk => clk_125,
		 rst => system_reset,
		 swap_banks => swap_banks(2*(POPULATION_UNITS_1D+I)),
		 rd0_addr => dv_rd0_addr(2*(POPULATION_UNITS_1D+I)),
		 rd0_data => dv_rd0_data(2*(POPULATION_UNITS_1D+I)),
		 rd1_addr => dv_rd1_addr(2*(POPULATION_UNITS_1D+I)),
		 rd1_data => dv_rd1_data(2*(POPULATION_UNITS_1D+I)),
		 wr0_addr => dv_wr0_addr(2*(POPULATION_UNITS_1D+I)),
		 wr0_we => dv_wr0_we(2*(POPULATION_UNITS_1D+I)),
		 wr0_data => dv_wr0_data(2*(POPULATION_UNITS_1D+I)),
		 wr1_addr => dv_wr1_addr(2*(POPULATION_UNITS_1D+I)),
		 wr1_we => dv_wr1_we(2*(POPULATION_UNITS_1D+I)),
		 wr1_data => dv_wr1_data(2*(POPULATION_UNITS_1D+I)),
		 prog_ok => enable_programming,
		 prog_addr => reg.prog_reg_addr(10 downto 0),
		 prog_we => reg.prog_dv_we(2*(95 - I)),
		 prog_data => reg.prog_reg_data(11 downto 0)
	);
	MUX_TO_DV_PORT0_BANK0: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 decode => "0" & std_logic_vector(to_unsigned(2*(95 - I), 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr_to_mux,
		 output => dv_rd0_addr(2*(POPULATION_UNITS_1D+I)),
		 selected => encoder_select(2*(POPULATION_UNITS_1D+I))
	);
	MUX_TO_DV_PORT1_BANK0: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 decode => "1" & std_logic_vector(to_unsigned(2*(95 - I), 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr_to_mux,
		 output => dv_rd1_addr(2*(POPULATION_UNITS_1D+I)),
		 selected => encoder_select(NUMBER_OF_DV_BANKS + 2*(POPULATION_UNITS_1D+I))
	);
	DV_BANK1: dv_double_buffer port map (
		 clk => clk_125,
		 rst => system_reset,
		 swap_banks => swap_banks(2*(POPULATION_UNITS_1D+I)+1),
		 rd0_addr => dv_rd0_addr(2*(POPULATION_UNITS_1D+I)+1),
		 rd0_data => dv_rd0_data(2*(POPULATION_UNITS_1D+I)+1),
		 rd1_addr => dv_rd1_addr(2*(POPULATION_UNITS_1D+I)+1),
		 rd1_data => dv_rd1_data(2*(POPULATION_UNITS_1D+I)+1),
		 wr0_addr => dv_wr0_addr(2*(POPULATION_UNITS_1D+I)+1),
		 wr0_we => dv_wr0_we(2*(POPULATION_UNITS_1D+I)+1),
		 wr0_data => dv_wr0_data(2*(POPULATION_UNITS_1D+I)+1),
		 wr1_addr => dv_wr1_addr(2*(POPULATION_UNITS_1D+I)+1),
		 wr1_we => dv_wr1_we(2*(POPULATION_UNITS_1D+I)+1),
		 wr1_data => dv_wr1_data(2*(POPULATION_UNITS_1D+I)+1),
		 prog_ok => enable_programming,
		 prog_addr => reg.prog_reg_addr(10 downto 0),
		 prog_we => reg.prog_dv_we(2*(95 - I)+1),
		 prog_data => reg.prog_reg_data(11 downto 0)
	);
	MUX_TO_DV_PORT0_BANK1: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 decode => "0" & std_logic_vector(to_unsigned(2*(95-I)+1, 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr_to_mux,
		 output => dv_rd0_addr(2*(POPULATION_UNITS_1D+I)+1),
		 selected => encoder_select(2*(POPULATION_UNITS_1D+I)+1)
	);
	MUX_TO_DV_PORT1_BANK1: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 decode => "1" & std_logic_vector(to_unsigned(2*(95-I)+1, 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr_to_mux,
		 output => dv_rd1_addr(2*(POPULATION_UNITS_1D+I)+1),
		 selected => encoder_select(NUMBER_OF_DV_BANKS + 2*(POPULATION_UNITS_1D+I)+1)
	);
end generate;

all_encoders_done <= and_reduce(encoder_done);
all_decoders_done <= and_reduce(all_done);

OUTPUT_COORDINATOR: encoder_output_coordinator port map (
	clk => clk_125,
	rst => system_reset,
	all_encoders_done => all_encoders_done,
	all_outputs_done => all_outputs_done,
	mux_select => encoder_output_mux_select,
	output_start => output_start
);
all_outputs_done <= and_reduce(output_done);

-- encoders that share an address line with an output channel are multiplexed
ENCODER_ADDR_MUX: for I in 0 to NUMBER_OF_OUTPUT_CHANNELS-1 generate
	encoder_addr_to_mux(I) <= output_channel_dv_addr(I) when encoder_output_mux_select = '1' else encoder_addr(I);
end generate;
-- encoders that don't share an address line with an output channel are passed through
ENCODER_ADDR_PASSTHROUGH: for I in NUMBER_OF_OUTPUT_CHANNELS to NUMBER_OF_ENCODERS-1 generate
	encoder_addr_to_mux(I) <= encoder_addr(I);
end generate;

OUTPUT_CHANNEL_DV_DATA_PASSTHROUGH: for I in 0 to NUMBER_OF_OUTPUT_CHANNELS-1 generate
	output_channel_dv_data(I) <= encoder_data(I);
end generate;

OUTPUT_CHANNELS: for I in 0 to NUMBER_OF_OUTPUT_CHANNELS-1 generate
	OUTCHAN: output_channel port map (
		clk => clk_125,
		rst => system_reset,
		start => output_start, 
		dv_addr => output_channel_dv_addr(I)(18 downto 0),
		dv_port => output_channel_dv_addr(I)(19),
		dv_data => output_channel_dv_data(I),
		channel_data => output_channel_data(I),
		channel_we => output_channel_we(I),
		done => output_done(I),
		
		prog_ok => enable_programming,
		prog_we => reg.prog_output_channel_we(I),
		prog_data => reg.prog_reg_data(35 downto 0)
	);
end generate;

output0_data <= output_channel_data(0);
output0_we <= output_channel_we(0);
output0_done <= output_done(0);

end architecture rtl;
