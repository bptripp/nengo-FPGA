library ieee;
use ieee.std_logic_1164.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity first_order_filter_unit is port (
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
);
end entity;

architecture rtl of first_order_filter_unit is

component programmable_multiplexed_1filter Port ( 
    clk : in STD_LOGIC;
    rst: in std_logic;
    x : in sfixed (1 downto -16);
    u : in sfixed (1 downto -10);
    valid : in STD_LOGIC;
    x1 : out sfixed (1 downto -16);
    y : out sfixed (1 downto -10);
    ready : out STD_LOGIC; -- connect this to next stage data-valid
    ack: in std_logic; -- connect this to next stage data-acknowledge
    we: out std_logic; -- connect this to FIFO acknowledge (assuming FWFT) and FIFO write-enable
    prog_addr: in std_logic_vector(1 downto 0); -- "00"=A, "01"=B, "10"=C, "11"=D
    prog_we: in std_logic;
    prog_data: in std_logic_vector(11 downto 0)
); end component;

component filter_state_fifo_reset_controller port (
    clk: in std_logic;
    rst: in std_logic;
    fifo_rst: out std_logic;
    fifo_full: in std_logic;
    fifo_data: out std_logic_vector(17 downto 0);
    fifo_we: out std_logic;
    
    user_data: in std_logic_vector(17 downto 0);
    user_we: in std_logic
); end component;           

component filter_state_fifo  PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(17 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(17 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC
); end component;

signal filter_x: sfixed(1 downto -16);
signal filter_x1: sfixed(1 downto -16);
signal filter_we: std_logic;
signal fifo_full: std_logic;
signal fifo_din: std_logic_vector(17 downto 0);
signal fifo_dout: std_logic_vector(17 downto 0);

signal controlled_rst: std_logic;
signal controlled_din: std_logic_vector(17 downto 0);
signal controlled_we: std_logic;

begin

FILTER: programmable_multiplexed_1filter port map (
    clk => clk,
    rst => rst,
    x => filter_x,
    u => u,
    valid => valid,    
    x1 => filter_x1,
    y => y,
    ready => ready,
    ack => ack,
    we => filter_we,
    prog_addr => prog_addr,
    prog_we => prog_we,
    prog_data => prog_data    
);

ready_stb <= filter_we; -- cheating, but it serves the same purpose

fifo_din <= std_logic_vector(filter_x1);
filter_x <= to_sfixed(fifo_dout, 1, -16);

FIFO: filter_state_fifo port map (
    clk => clk,
    rst => controlled_rst,
    din => controlled_din,
    wr_en => controlled_we,
    rd_en => filter_we,
    dout => fifo_dout,
    full => fifo_full,
    empty => open
);

RST_CTL: filter_state_fifo_reset_controller port map (
    clk => clk,
    rst => rst,
    fifo_rst => controlled_rst,
    fifo_full => fifo_full,
    fifo_data => controlled_din,
    fifo_we => controlled_we,
    user_data => fifo_din,
    user_we => filter_we
);

end architecture rtl; 