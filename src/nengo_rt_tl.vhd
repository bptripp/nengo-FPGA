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
    -- 0x6: not used
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
    prog_addr: in std_logic_vector(23 downto 0); 
    prog_we: in std_logic; -- active HIGH pulse; ignored while prog_ok is LOW.
    -- Again, the important bits in this field vary depending on what's being programmed:
    -- Decoded value buffers use the lowest 12.
    -- Encoder instruction lists use all 40.
    -- PC filter characteristics use the lowest 12.
    -- PC LFSRs use the lowest 32.
    -- Principal components use the lowest 12.
    -- Decoder memory uses the lowest 12.
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
    timestep_overflow: out std_logic -- Strobed HIGH when a timeout has occurred.
    );
end entity;

architecture rtl of nengo_rt_tl is

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
constant NUMBER_OF_DV_BANKS: integer := 9;

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


constant NUMBER_OF_ENCODERS: integer := 8;
signal encoder_addr: encoder_addresses(0 to NUMBER_OF_ENCODERS-1);
signal encoder_data: dv_data(0 to NUMBER_OF_ENCODERS-1);

type encoder_selection_type is array(0 to 2*NUMBER_OF_DV_BANKS-1) of std_logic_vector(NUMBER_OF_ENCODERS-1 downto 0);
signal encoder_select: encoder_selection_type; -- the idea is that encoder_select(X)(Y) is asserted when mux_to_dv_port for DV port X got addressed by encoder Y
type reverse_encoder_selection_type is array(0 to NUMBER_OF_ENCODERS-1) of std_logic_vector(2*NUMBER_OF_DV_BANKS-1 downto 0);
signal reverse_encoder_select: reverse_encoder_selection_type; -- effectively, reverse_encoder_select(X) is the concatenation of encoder_select(*)(X)

component population_unit_1d port (
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
); end component population_unit_1d;
constant NUMBER_OF_POPULATION_UNITS: integer := 8;
signal all_done: std_logic_vector(NUMBER_OF_POPULATION_UNITS-1 downto 0);

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

begin
    ci := reg;
    -- self-clearing
    ci.prog_dv_we := (others=>'0');
    ci.prog_encoder_we := (others=>'0');
    ci.prog_pc_filter_we := (others=>'0');
    ci.prog_pc_lfsr_we := (others=>'0');
    ci.prog_pc_we := (others=>'0');
    ci.prog_decoder_memory_we := (others=>'0');
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
    --SIMULATION => SIMULATION,
    SIMULATION => "TRUE", -- FIXME CHEATING to get faster runs
    CLOCKS_PER_TIMESTEP => 125000
) port map (
    clk => clk_125,
    rst => system_reset,
    start => start,
    pause => pause,
    done => all_decoders_done,
    
    running => run_started,
    timestep => timestep,
    timestep_overflow => timestep_overflow_stb
);

-- FIXME BLATANT CHEATING to stop XST from optimizing most of the design out
timestep_overflow <= timestep_overflow_stb or dv_rd0_data(0)(0) or dv_rd0_data(0)(1) or dv_rd0_data(0)(2) or dv_rd0_data(0)(3)
or dv_rd0_data(0)(4) or dv_rd0_data(0)(5) or dv_rd0_data(0)(6) or dv_rd0_data(0)(7) or dv_rd0_data(0)(8) or dv_rd0_data(0)(9)
or dv_rd0_data(0)(10) or dv_rd0_data(0)(11);

DECODE_PAGE_EN: for I in 0 to 63 generate
    page_en(I) <= '1' when (page_block_addr = std_logic_vector(to_unsigned(I, 6))) else '0';
    page_wr_en(I) <= page_en(I) and page_we;
end generate;

DEFAULT_SWAP_BANKS: for I in 0 to 7 generate
    swap_banks(I) <= timestep;
end generate;
INPUT_SWAP_BANKS: for I in 8 to NUMBER_OF_DV_BANKS-1 generate
    LOCK: bank_lock port map (
        clk => clk_125,
        rst => system_reset,
        en => page_en(I-1),
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

DV_BANKS: for I in 0 to 7 generate
	DV_BANK: dv_double_buffer port map (
		 clk => clk_125,
		 rst => system_reset,
		 swap_banks => swap_banks(I),
		 rd0_addr => dv_rd0_addr(I),
		 rd0_data => dv_rd0_data(I),
		 rd1_addr => dv_rd1_addr(I),
		 rd1_data => dv_rd1_data(I),
		 wr0_addr => dv_wr0_addr(I),
		 wr0_we => dv_wr0_we(I),
		 wr0_data => dv_wr0_data(I),
		 wr1_addr => dv_wr1_addr(I),
		 wr1_we => dv_wr1_we(I),
		 wr1_data => dv_wr1_data(I),
		 prog_ok => enable_programming,
		 prog_addr => reg.prog_reg_addr(10 downto 0),
		 prog_we => reg.prog_dv_we(I),
		 prog_data => reg.prog_reg_data(11 downto 0)
	);
	MUX_TO_DV_PORT0: mux_to_dv_port generic map (
		 N => NUMBER_OF_ENCODERS,
		 decode => "0" & std_logic_vector(to_unsigned(I, 8))
	) port map (
		 clk => clk_125,
		 data => encoder_addr,
		 output => dv_rd0_addr(I),
		 selected => encoder_select(I)
	);
end generate DV_BANKS;

INPUT_BANK192: dv_double_buffer port map (
    clk => clk_125,
    rst => system_reset,
    swap_banks => swap_banks(8),
    rd0_addr => dv_rd0_addr(8),
    rd0_data => dv_rd0_data(8),
    rd1_addr => dv_rd1_addr(8),
    rd1_data => dv_rd1_data(8),
    wr0_addr => page_word_addr,
    wr0_we => page_wr_en(0),
    wr0_data => page_data,
    wr1_addr => "00000000000",
    wr1_we => '0',
    wr1_data => X"000",
    prog_ok => enable_programming,
    prog_addr => reg.prog_reg_addr(10 downto 0),
    prog_we => reg.prog_dv_we(192),
    prog_data => reg.prog_reg_data(11 downto 0)
);
MUX_TO_DV192_PORT0: mux_to_dv_port generic map (
    N => NUMBER_OF_ENCODERS,
    decode => "0" & X"C0"
) port map (
    clk => clk_125,
    data => encoder_addr,
    output => dv_rd0_addr(8),
    selected => encoder_select(8)
);

-- FIXME the following generate blocks must be updated when more DV blocks are added
encoder_select(9 to 17) <= (others=>(others=>'0'));
FAKE_DV_RD1: for I in 0 to 8 generate
	dv_rd1_addr(I) <= dv_rd0_addr(0);
end generate;


POPULATION_UNITS: for I in 0 to NUMBER_OF_POPULATION_UNITS-1 generate
	POPULATION_UNIT: population_unit_1d port map (
		clk => clk_125,
		rst => system_reset,
		timestep => timestep,
		all_done => all_done(I),
		
		encoder0_dv_addr => encoder_addr(I)(18 downto 0),
		encoder0_dv_port => encoder_addr(I)(19),
		encoder0_dv_data => encoder_data(I),
		-- FIXME temporarily not using encoder #1
		encoder1_dv_addr => open,
		encoder1_dv_port => open,
		encoder1_dv_data => (others=>'0'),
		
		decoder0_dv_addr => dv_wr0_addr(I),
		decoder0_dv_we => dv_wr0_we(I),
		decoder0_dv_data => dv_wr0_data(I),
		decoder1_dv_addr => dv_wr1_addr(I),
		decoder1_dv_we => dv_wr1_we(I),
		decoder1_dv_data => dv_wr1_data(I),
		-- FIXME temporarily not using decoders #2 and #3
		decoder2_dv_addr => open,
		decoder2_dv_we => open,
		decoder2_dv_data => open,
		decoder3_dv_addr => open,
		decoder3_dv_we => open,
		decoder3_dv_data => open,
		
		prog_ok => enable_programming,
		prog_addr => reg.prog_reg_addr,
		prog_data => reg.prog_reg_data,
		prog_encoder_we => reg.prog_encoder_we(2*I+1 downto 2*I), -- 2 and 3 reserved
		prog_pc_filter_we => reg.prog_pc_filter_we(2*I+1 downto 2*I), -- 2 and 3 reserved
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
		 sel => reverse_encoder_select(I),
		 output => encoder_data(I)
	);
end generate POPULATION_UNITS;

all_decoders_done <= and_reduce(all_done);

end architecture rtl;
