library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity prefetch_controller is generic (
    N: positive := 33; -- FIFO depth
    T: positive := 12 -- number of transfers per timeslice
); port (
    clk: in std_logic;
    rst: in std_logic;
    -- programming interface
    prog_addr: in std_logic_vector(17 downto 0);
    prog_we: in std_logic;
    prog_data: in std_logic_vector(7 downto 0);
    prog_busy: out std_logic;
    prog_done: out std_logic;
    -- decoder FIFO interface
    fifo_rst: out std_logic;
    fifo_we: out std_logic;
    fifo_data: out std_logic_vector(511 downto 0);
    fifo_count: in std_logic_vector(5 downto 0); -- FIXME check port width
    -- DDR3 interface
    ddr3_rst: out std_logic;
    ddr3_calibration_complete: in std_logic;
    ddr3_ui_ready: in std_logic;
    ddr3_addr: out std_logic_vector(27 downto 0);
    ddr3_cmd: out std_logic_vector(2 downto 0);
    ddr3_en: out std_logic;
    
    ddr3_wdf_data: out std_logic_vector(511 downto 0);
    ddr3_wdf_we: out std_logic;
    ddr3_wdf_ready: in std_logic;
    
    ddr3_read_data: in std_logic_vector(511 downto 0);
    ddr3_read_valid: in std_logic
); end entity prefetch_controller;

architecture rtl of prefetch_controller is

component prefetch_read_unit port (
    clk: in std_logic;
    invalidate: in std_logic;            
    ddr3_read_data: in std_logic_vector(511 downto 0);
    ddr3_read_valid: in std_logic;
    rx_read: out std_logic;    
    fifo_we: out std_logic;
    fifo_data: out std_logic_vector(511 downto 0)
); end component;
signal rx_read: std_logic; -- read-acknowledge strobe from read unit

component prefetch_count_monitor port (
    clk: in std_logic;
    rst: in std_logic;
    issue_read: in std_logic;
    rx_read: in std_logic;
    count: out unsigned(5 downto 0)
); end component prefetch_count_monitor;
signal outstanding_read_count: unsigned(5 downto 0);

type state_type is (state_reset, state_wait_for_ddr3, state_prefetch);
type ci_type is record
    state: state_type;
    ddr3_rst: std_logic;
    timeslice: unsigned(9 downto 0);
    xferno: unsigned(7 downto 0);
    prefetch_invalidate: std_logic;
    issue_read: std_logic;
    ddr3_addr: std_logic_vector(27 downto 0);
    ddr3_cmd: std_logic_vector(2 downto 0);
    ddr3_en: std_logic;
end record;

constant reg_reset: ci_type := (
    state => state_reset,
    ddr3_rst => '0',
    timeslice => (others=>'0'),
    xferno => (others=>'0'),
    prefetch_invalidate => '1',
    issue_read => '0',
    ddr3_addr => (others=>'0'),
    ddr3_cmd => (others=>'0'),
    ddr3_en => '0'
);

constant LAST_XFERNO: unsigned(7 downto 0) := to_unsigned(T-1, 8);

-- Convert a virtual address (timeslice + xferno pair) into a DDR3 physical address.
function virt2phys (timeslice: unsigned(9 downto 0); xferno: unsigned(7 downto 0))
return std_logic_vector is
    variable timeslice_slv: std_logic_vector(9 downto 0);
    variable xferno_slv: std_logic_vector(7 downto 0);
    variable retval: std_logic_vector(27 downto 0);
begin
    timeslice_slv := std_logic_vector(timeslice);
    xferno_slv := std_logic_vector(xferno);
    -- 0000000 TTT NNNNNNNN TTTTTTT 000
    retval(27 downto 21) := (others=>'0');
    retval(20 downto 18) := timeslice_slv(9 downto 7);
    retval(17 downto 10) := xferno_slv;
    retval(9 downto 3) := timeslice_slv(6 downto 0);
    retval(2 downto 0) := (others=>'0');
    return retval;
end function virt2phys;

signal reg: ci_type := reg_reset;
signal ci_next: ci_type;

begin

ddr3_rst <= reg.ddr3_rst;
ddr3_addr <= reg.ddr3_addr;
ddr3_cmd <= reg.ddr3_cmd;
ddr3_en <= reg.ddr3_en;

COMB: process(reg, rst, ddr3_calibration_complete, ddr3_ui_ready, outstanding_read_count, fifo_count)
    variable ci: ci_type;
    variable reserved_read_count: unsigned(6 downto 0);
begin
    ci := reg;
    reserved_read_count := outstanding_read_count + unsigned("0" & fifo_count);
    -- self-clearing signals
    ci.ddr3_rst := '0';
    ci.issue_read := '0';
    ci.ddr3_en := '0';
    
    if(rst = '1') then
        ci := reg_reset;
    else
        case reg.state is
            when state_reset =>
                ci.ddr3_rst := '1';
                ci.state := state_wait_for_ddr3;
            when state_wait_for_ddr3 =>
                if(ddr3_calibration_complete = '1' and ddr3_ui_ready = '1') then
                    ci.state := state_prefetch;
                    ci.prefetch_invalidate := '0';
                end if;
            when state_prefetch =>
                if(reserved_read_count < N and ddr3_ui_ready = '1') then
                    -- issue the read
                    ci.ddr3_addr := virt2phys(reg.timeslice, reg.xferno);
                    ci.ddr3_cmd := "001"; -- UG586 says this is the Read command
                    ci.ddr3_en := '1';
                    ci.issue_read := '1';
                    -- increment loop counters
                    if(reg.xferno = LAST_XFERNO) then
                        ci.timeslice := reg.timeslice + X"1"; -- this will automatically wrap around, which is what we want.
                        ci.xferno := (others=>'0');
                    else
                        ci.xferno := reg.xferno + X"1";
                    end if;
                end if;
        end case;
    end if;
    
    ci_next <= ci;
end process COMB;

SEQ: process(clk, ci_next)
begin
    if(rising_edge(clk)) then
        reg <= ci_next;
    end if;
end process SEQ;


READ_UNIT: prefetch_read_unit port map (
    clk => clk,
    invalidate => reg.prefetch_invalidate,
    ddr3_read_data => ddr3_read_data,
    ddr3_read_valid => ddr3_read_valid,
    rx_read => rx_read,
    fifo_we => fifo_we,
    fifo_data => fifo_data
);

COUNT_MONITOR: prefetch_count_monitor port map (
    clk => clk,
    rst => rst,
    issue_read => reg.issue_read,
    rx_read => rx_read,
    count => outstanding_read_count
);

end architecture rtl;