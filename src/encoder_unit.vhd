library ieee;
use ieee.std_logic_1164.all;

library ieee_proposed;
use ieee_proposed.fixed_pkg.all;

entity encoder_unit is
end entity;

architecture rtl of encoder_unit is
component encoding_controller Port ( 
    clk : in STD_LOGIC;
    rst : in STD_LOGIC;
    next_population: in std_logic;
    next_insn: out std_logic;
    insn_data : in STD_LOGIC_VECTOR (39 downto 0);
    dv_addr : out STD_LOGIC_VECTOR (18 downto 0);
    dv_port : out STD_LOGIC;
    dv_data : in STD_LOGIC_VECTOR (11 downto 0);
    sum: out sfixed(1 downto -10);
    done: out std_logic
); end component;

component instruction_buffer PORT (
    clk : IN STD_LOGIC;
    rst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(39 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(39 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC
  ); end component;

begin
end architecture rtl;
