library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity principal_component_1d_programmable_tb is
end entity;

architecture sim of principal_component_1d_programmable_tb is

signal clk: std_logic;
constant CLOCK_PERIOD: time := 8 ns;

component reset_controller port (
    clk: in std_logic;
    rst_i: in std_logic;
    rst_o: out std_logic;
    rst_done: out std_logic
); end component;
signal rst_i: std_logic;
signal rst: std_logic;
signal rst_done: std_logic;
signal prog_ok: std_logic;
signal timestep: std_logic;

component dv_double_buffer port (
        clk: in std_logic;
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
        wr1_data: in std_logic_vector(11 downto 0)
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

procedure PROGRAM_DV (
    signal addr: out std_logic_vector(10 downto 0);
    signal we: out std_logic;
    signal data: out std_logic_vector(11 downto 0)
) is
    variable addr_counter: unsigned(10 downto 0) := (others=>'0');
begin
    we <= '0';
    for I in 0 to 2047 loop
        addr_counter := to_unsigned(I, 11);
        if(I > 1023) then
            -- input range (U)
            data <= std_logic_vector(to_sfixed(0.5, 1,-10));
        else
            -- feedback range (X)
            data <= X"000";
        end if;
        addr <= std_logic_vector(addr_counter);
        we <= '1';
        wait for CLOCK_PERIOD;
    end loop;
    we <= '0';
end procedure PROGRAM_DV; 

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

procedure PROGRAM_ENCODER (
    signal we: out std_logic;
    signal data: out std_logic_vector(39 downto 0)
) is
    variable addr_counter: unsigned(9 downto 0) := (others=>'0');
    variable insn_last: std_logic;
    variable insn_delay: std_logic_vector(6 downto 0) := (others=>'0');
    variable insn_port: std_logic := '0';    
begin
    we <= '0';
    for I in 0 to 1023 loop
        addr_counter := to_unsigned(I, 10);
        -- program 1.0 * Xi
        insn_last := '0';        
        data <= insn_last & insn_delay & insn_port & "00000000" & "0" & std_logic_vector(addr_counter) & to_slv(to_sfixed(1.0, 1,-10));
        we <= '1';
        wait for CLOCK_PERIOD;
        -- program 0.1 * Ui
        insn_last := '1';
        data <= insn_last & insn_delay & insn_port & "00000000" & "1" & std_logic_vector(addr_counter) & to_slv(to_sfixed(0.1, 1,-10));
        we <= '1';
        wait for CLOCK_PERIOD;
    end loop;
    we <= '0';
end procedure PROGRAM_ENCODER;

component encoder_fifo  PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(11 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC
  ); end component;
signal encoder_fifo_din: std_logic_vector(11 downto 0);
signal encoder_fifo_we: std_logic;
signal encoder_fifo_re: std_logic;
signal encoder_fifo_dout: std_logic_vector(11 downto 0);
signal encoder_fifo_full: std_logic;
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
    
    -- shift registers    
    shreg_data: in std_logic_vector(31 downto 0); -- 8 coefficients per decoder * 4 decoders
    shreg_shift: in std_logic;
    shreg_clear: in std_logic;
    shreg_ready: in std_logic;
    shreg_ack: out std_logic;
    
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

CLKGEN: process
begin
    clk <= '0';
    loop
        clk <= '0';
        wait for CLOCK_PERIOD/2;
        clk <= '1';
        wait for CLOCK_PERIOD/2;
    end loop;
end process CLKGEN;

SYS_RST: reset_controller port map (
    clk => clk,
    rst_i => rst_i,
    rst_o => rst,
    rst_done => rst_done
);

DV_BANK0: dv_double_buffer port map (
    clk => clk,
    swap_banks => swap_banks,
    rd0_addr => dv_rd0_addr,
    rd0_data => dv_rd0_data,
    rd1_addr => dv_rd1_addr,
    rd1_data => dv_rd1_data,
    wr0_addr => dv_wr0_addr,
    wr0_we => dv_wr0_we,
    wr0_data => dv_wr0_data,
    wr1_addr => dv_prog_addr,
    wr1_we => dv_prog_we,
    wr1_data => dv_prog_data
);

ENCODER: encoder_unit port map (
    clk => clk,
    rst => rst,
    next_population => encoder_next_population,
    dv_addr => encoder_dv_addr,
    dv_port => encoder_dv_port,
    dv_data => encoder_dv_data,
    sum => encoder_sum,
    done => encoder_done,
    we => encoder_we,
    
    prog_ok => prog_ok,
    prog_we => encoder_prog_we,
    prog_data => encoder_prog_data
);
dv_rd0_addr <= encoder_dv_addr(10 downto 0);
encoder_dv_data <= dv_rd0_data;

ENCODER_PIPE_CTRL: encoder_pipeline_controller generic map (
        N => 1
    )
    port map (
        clk => clk,
        encoder_done(0) => encoder_done,
        timestep => timestep,
        fifo_full(0) => encoder_fifo_full,
        
        encode_next => encoder_next_population
    );

ENCODER_PIPE: encoder_fifo  PORT MAP (
    clk => clk,
    rst => rst,
    din => encoder_fifo_din,
    wr_en => encoder_fifo_we,
    rd_en => encoder_fifo_re,
    dout => encoder_fifo_dout,
    full => encoder_fifo_full,
    empty => encoder_fifo_empty
  );
encoder_fifo_din <= to_slv(encoder_sum);
encoder_fifo_we <= encoder_we;

tb: process
begin
    rst_i <= '1';
    prog_ok <= '0';
    timestep <= '0';
    encoder_prog_we <= '0';
    swap_banks <= '0';
    dv_prog_we <= '0';
    
    wait for CLOCK_PERIOD*2;
    rst_i <= '0';
    wait until rst_done = '1';
    prog_ok <= '1';
    
    PROGRAM_DV(dv_prog_addr, dv_prog_we, dv_prog_data);
    wait for CLOCK_PERIOD;
    swap_banks <= '1';
    wait for CLOCK_PERIOD;        
    swap_banks <= '0';
    wait for CLOCK_PERIOD;
    PROGRAM_DV(dv_prog_addr, dv_prog_we, dv_prog_data);
    wait for CLOCK_PERIOD;
    
    PROGRAM_ENCODER(encoder_prog_we, encoder_prog_data);
    
    wait;
end process tb;

end architecture sim;