library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

library std;
use std.textio.all;

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
    start: in std_logic; -- Pulse HIGH to begin execution. Ignored while prog_ok is LOW.
    pause: in std_logic; -- Pulse HIGH to pause execution after current timestep. If start also asserted
                         -- on same timestep, single-step the simulation.
    running: out std_logic;
    timestep_overflow: out std_logic -- Strobed HIGH when a timeout has occurred.
    );
end entity;

architecture rtl of nengo_rt_tl is

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
signal timestep: std_logic;

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

signal swap_banks: std_logic;
signal dv_rd0_addr: std_logic_vector(10 downto 0);
signal dv_rd0_data: std_logic_vector(11 downto 0);
signal dv_rd1_addr: std_logic_vector(10 downto 0);
signal dv_rd1_data: std_logic_vector(11 downto 0);
signal dv_wr0_addr: std_logic_vector(10 downto 0);
signal dv_wr0_we: std_logic;
signal dv_wr0_data: std_logic_vector(11 downto 0);
-- wr1
signal dv_prog_addr: std_logic_vector(10 downto 0);
signal dv_prog_we: std_logic;
signal dv_prog_data: std_logic_vector(11 downto 0); 

component delay_line generic (
    N: natural := 1; -- port width
    T: natural := 0
); port (
    clk: in std_logic;
    d: in std_logic_vector(N-1 downto 0);
    q: out std_logic_vector(N-1 downto 0)
); end component;
signal dv_rd0_data_delayed: std_logic_vector(11 downto 0);
signal dv_rd1_data_delayed: std_logic_vector(11 downto 0);

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
signal encoder_next_population: std_logic;
signal encoder_dv_addr: std_logic_vector(18 downto 0);
signal encoder_dv_port: std_logic;
signal encoder_dv_data: std_logic_vector(11 downto 0);
signal encoder_sum: sfixed(1 downto -10);
signal encoder_done: std_logic;
signal encoder_we: std_logic;
signal encoder_prog_we: std_logic;
signal encoder_prog_data: std_logic_vector(39 downto 0);

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
signal encoder_fifo_din: std_logic_vector(11 downto 0);
signal encoder_fifo_we: std_logic;
signal encoder_fifo_re: std_logic;
signal encoder_fifo_dout: std_logic_vector(11 downto 0);
signal encoder_fifo_full: std_logic;
signal encoder_fifo_almost_full: std_logic;
signal encoder_fifo_empty: std_logic;

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
signal h1_u: sfixed(1 downto -10);
signal h1_valid: std_logic;
signal h1_y: sfixed(1 downto -10);
signal h1_ready: std_logic;
signal h1_ready_stb: std_logic;
signal h1_ack: std_logic;
signal h1_prog_addr: std_logic_vector(1 downto 0);
signal h1_prog_we: std_logic;
signal h1_prog_data: std_logic_vector(11 downto 0);

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
signal decoder_u: std_logic_vector(11 downto 0);
signal decoder_empty: std_logic;
signal decoder_rd_en: std_logic;
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

signal decoder_pc_prog_addr: std_logic_vector(13 downto 0);
signal decoder_pc_prog_we: std_logic;
signal decoder_pc_prog_data: std_logic_vector(11 downto 0);

signal decoder_normal_prog_addr: std_logic_vector(1 downto 0);
signal decoder_normal_prog_we: std_logic;
signal decoder_normal_prog_data: std_logic_vector(31 downto 0);

    type PrincipalComponentMemoryType is array(0 to 1023) of std_logic_vector(11 downto 0);
          
    impure function InitPCFromFile (FileName: in string) return PrincipalComponentMemoryType is
        FILE ROMFile : text is in FileName;
        variable ROMFileLine : line;
        variable ROM: PrincipalComponentMemoryType;
        variable tmp: bit_vector(11 downto 0);
    begin
        for I in PrincipalComponentMemoryType'range loop
            readline(ROMFile, ROMFileLine);
            read(ROMFileLine, tmp);
            ROM(I) := to_stdlogicvector(tmp);
        end loop;
        return ROM;
    end function;  

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
signal decoder_bottom_half_all_done: std_logic;

component decoder_fifo PORT (
    rst : IN STD_LOGIC;
    wr_clk : IN STD_LOGIC;
    rd_clk : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(511 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(511 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC;
    wr_data_count : OUT STD_LOGIC_VECTOR(5 DOWNTO 0)
  ); end component;
signal decoder_coefficient_fifo_rst: std_logic;
signal decoder_coefficient_fifo_rst_125: std_logic;
signal decoder_coefficient_fifo_din: std_logic_vector(511 downto 0);
signal decoder_coefficient_fifo_wr_en: std_logic;
signal decoder_coefficient_fifo_full: std_logic;
signal decoder_coefficient_fifo_dout: std_logic_vector(511 downto 0);
signal decoder_coefficient_fifo_empty: std_logic;
signal decoder_coefficient_fifo_wr_data_count: std_logic_vector(5 downto 0);

component shift_controller generic (
    N: natural := 1; -- number of acknowledge lines
    C: unsigned(7 downto 0) -- shift count to load all decoders, usually 72 or 144 for 96 1-D or 96 2-D population units respectively
); 
port (
    clk: in std_logic;
    rst: in std_logic;
    invalidate: in std_logic;
    fifo_data: in std_logic_vector(511 downto 0);
    fifo_empty: in std_logic;
    fifo_rd_en: out std_logic;
    shift_data: out std_logic_vector(511 downto 0);
    shift_clear: out std_logic;
    shift_en: out std_logic;
    shift_ack: in std_logic_vector(N-1 downto 0)
); end component;
signal shift_fifo_rd_en: std_logic;
signal shift_data: std_logic_vector(511 downto 0);
signal shift_clear: std_logic;
signal shift_en: std_logic;
signal shift_ack: std_logic_vector(0 downto 0);
signal shctl_invalidate: std_logic;
signal shctl_invalidate_125: std_logic;

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
    CLOCKS_PER_TIMESTEP => 125000
) port map (
    clk => clk_125,
    rst => system_reset,
    start => start,
    pause => pause,
    done => all_decoders_done,
    
    running => run_started,
    timestep => timestep,
    timestep_overflow => timestep_overflow
);

swap_banks <= timestep;
DV_BANK0: dv_double_buffer port map (
    clk => clk_125,
    rst => system_reset,
    swap_banks => swap_banks,
    rd0_addr => dv_rd0_addr,
    rd0_data => dv_rd0_data,
    rd1_addr => dv_rd1_addr,
    rd1_data => dv_rd1_data,
    wr0_addr => dv_wr0_addr,
    wr0_we => dv_wr0_we,
    wr0_data => dv_wr0_data,
    wr1_addr => "00000000000",
    wr1_we => '0',
    wr1_data => X"000",
    prog_ok => enable_programming,
    prog_addr => reg.prog_reg_addr(10 downto 0),
    prog_we => reg.prog_dv_we(0),
    prog_data => reg.prog_reg_data(11 downto 0)
);

-- to simulate the multi-cycle delay of the interconnect
DELAY_RD0: delay_line generic map (
    N => 12,
    T => 2
) port map (
    clk => clk_125,
    d => dv_rd0_data,
    q => dv_rd0_data_delayed
);
DELAY_RD1: delay_line generic map (
    N => 12,
    T => 2
) port map (
    clk => clk_125,
    d => dv_rd1_data,
    q => dv_rd1_data_delayed
);

ENCODER: encoder_unit port map (
    clk => clk_125,
    rst => system_reset,
    next_population => encoder_next_population,
    dv_addr => encoder_dv_addr,
    dv_port => encoder_dv_port,
    dv_data => encoder_dv_data,
    sum => encoder_sum,
    done => encoder_done,
    we => encoder_we,
    
    prog_ok => enable_programming,
    prog_we => reg.prog_encoder_we(0),
    prog_data => reg.prog_reg_data(39 downto 0)
);
dv_rd0_addr <= encoder_dv_addr(10 downto 0);
encoder_dv_data <= dv_rd0_data_delayed;

ENCODER_PIPE_CTRL: encoder_pipeline_controller generic map (
        N => 1
    )
    port map (
        clk => clk_125,
        encoder_done(0) => encoder_done,
        timestep => timestep,
        fifo_full(0) => encoder_fifo_almost_full,
        
        encode_next => encoder_next_population
    );

ENCODER_PIPE: encoder_fifo  PORT MAP (
    clk => clk_125,
    rst => system_reset,
    din => encoder_fifo_din,
    wr_en => encoder_fifo_we,
    rd_en => encoder_fifo_re,
    dout => encoder_fifo_dout,
    full => encoder_fifo_full,
    almost_full => encoder_fifo_almost_full,
    empty => encoder_fifo_empty
  );
encoder_fifo_din <= to_slv(encoder_sum);
encoder_fifo_we <= encoder_we;

H1: first_order_filter_unit port map (
    clk => clk_125,
    rst => system_reset,
    u => h1_u,
    valid => h1_valid,
    y => h1_y,
    ready => h1_ready,
    ready_stb => h1_ready_stb,
    ack => h1_ack,

    prog_addr => reg.prog_reg_addr(1 downto 0),
    prog_we => reg.prog_pc_filter_we(0),
    prog_data => reg.prog_reg_data(11 downto 0)
);
h1_u <= to_sfixed(encoder_fifo_dout, 1,-10);
h1_valid <= '1' when (encoder_fifo_empty = '0') else '0';
encoder_fifo_re <= h1_ready_stb;

DECODER_TOP_HALF: decoder_unit_top_half_1d port map (
    clk => clk_125,
    rst => system_reset,
    encoder_fifo_u => decoder_u,
    encoder_fifo_empty => decoder_empty,
    encoder_fifo_rd_en => decoder_rd_en,
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
    
    pc_prog_addr => reg.prog_reg_addr(13 downto 0),
    pc_prog_we => reg.prog_pc_we(0),
    pc_prog_data => reg.prog_reg_data(11 downto 0),
    
    normal_prog_addr => reg.prog_reg_addr(1 downto 0),
    normal_prog_we => reg.prog_pc_lfsr_we(0),
    normal_prog_data => reg.prog_reg_data(31 downto 0)
);
decoder_u <= to_slv(h1_y);
decoder_empty <= '1' when h1_ready = '0' else '0';
h1_ack <= decoder_rd_en;

DECODER_BOTTOM_HALF: decoder_unit_bottom_half_1d generic map (
    shift => 0,
    skip_count => 0
) port map (
    clk => clk_125,
    rst => system_reset,
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
    
    prog_ok => enable_programming,
    prog_addr => reg.prog_reg_addr(5 downto 0),
    prog_we => reg.prog_decoder_memory_we(0),
    prog_data => reg.prog_reg_data(11 downto 0),
    
    dv0_addr => dv_wr0_addr,
    dv0_we => dv_wr0_we,
    dv0_data => dv_wr0_data,
    dv1_addr => open,
    dv1_we => open,
    dv1_data => open,
    dv2_addr => open,
    dv2_we => open,
    dv2_data => open,
    dv3_addr => open,
    dv3_we => open,
    dv3_data => open,
    timestep => timestep,
    all_done => decoder_bottom_half_all_done
);
all_decoders_done <= decoder_bottom_half_all_done; -- correct for one PC

end architecture rtl;
