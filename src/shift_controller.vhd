library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity shift_controller is
generic (
    N: natural := 1; -- number of acknowledge lines
    C: unsigned(7 downto 0) -- shift count to load all decoders, usually 72 or 144 for 96 1-D or 96 2-D population units respectively
); 
port (
    clk: in std_logic;
    rst: in std_logic;
    fifo_data: in std_logic_vector(511 downto 0);
    fifo_empty: in std_logic;
    fifo_rd_en: out std_logic;
    shift_data: out std_logic_vector(511 downto 0);
    shift_clear: out std_logic;
    shift_en: out std_logic;
    shift_ack: in std_logic_vector(N-1 downto 0)
); end entity;

architecture rtl of shift_controller is
    type ci_type is record
        shift_count: unsigned(7 downto 0);
        data: std_logic_vector(511 downto 0);
        en: std_logic;
        read_strobe: std_logic;
        clear: std_logic;
        ack: std_logic_vector(N-1 downto 0);
    end record;
    constant ACK_ALL_ONES: std_logic_vector(N-1 downto 0) := (others=>'1');
    constant reg_reset: ci_type := (
        shift_count => C,
        data => (others=>'0'),
        en => '0',
        read_strobe => '0',
        clear => '0',
        ack => (others=>'0')
    );
    signal reg: ci_type;
    signal ci_next: ci_type;
begin

fifo_rd_en <= reg.read_strobe;
shift_data <= reg.data;
shift_clear <= reg.clear;
shift_en <= reg.en;

COMB: process(clk, reg, rst, fifo_data, fifo_empty, shift_ack)
    variable ci: ci_type;
    variable next_shift_count: unsigned(7 downto 0);
begin
    ci := reg;
    -- self-clearing flags
    ci.en := '0';
    ci.read_strobe := '0';
    ci.clear := '0';
    
    if(reg.shift_count = X"00") then
        next_shift_count := X"00";
    else
        next_shift_count := reg.shift_count - X"01";
    end if;
    
    -- automatically accumulate asserted ack signals
    ci.ack := reg.ack OR shift_ack;
    
    if(rst = '1') then
        ci := reg_reset;
    else
        if(reg.shift_count = X"00") then
            if(reg.ack = ACK_ALL_ONES) then
                ci.clear := '1';
                ci.shift_count := C;
                ci.ack := (others=>'0');
            end if;
        else    
            if(fifo_empty = '0') then
                ci.shift_count := next_shift_count;
                ci.read_strobe := '1';
                ci.en := '1';
                ci.data := fifo_data;
            end if;
        end if;
    end if;
    
    ci_next <= ci;
end process COMB;

SEQ: process(clk, ci_next)
begin
    if(rising_edge(clk)) then
        reg <= ci_next;
    end if;
end process SEQ;

end architecture rtl;