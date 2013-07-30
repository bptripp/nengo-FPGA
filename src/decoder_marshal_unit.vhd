-- One half of a CDC for the decoder programming interface; this part lives in the 125 MHz domain
-- and shoves address/data information into a FIFO to be unmarshalled in the 200 MHz domain into the prefetch controller.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity decoder_marshal_unit is port (
    clk: in std_logic;
    rst: in std_logic;
    addr: in std_logic_vector(17 downto 0);
    we: in std_logic;
    data: in std_logic_vector(7 downto 0);
    busy: out std_logic; -- pretty much exactly fifo_full
    -- FIFO
    -- write pattern: addr hi(9), addr lo(9), 64x data(8)
    fifo_rst: out std_logic;
    fifo_din: out std_logic_vector(8 downto 0);
    fifo_we: out std_logic;
    fifo_prog_full: in std_logic -- assumed to be at capacity-4
); end entity decoder_marshal_unit;

architecture rtl of decoder_marshal_unit is
    type state_type is (state_idle, state_write_addr_lo, state_write_data);
    type ci_type is record
        state: state_type;
        data_count: unsigned(5 downto 0);
        fifo_rst: std_logic;
        fifo_data: std_logic_vector(8 downto 0);
        fifo_we: std_logic;
        addr_lo: std_logic_vector(8 downto 0);
    end record;    
    constant reg_reset: ci_type := (
        state => state_idle,
        data_count => (others=>'0'),
        fifo_rst => '1',
        fifo_data => (others=>'0'),
        fifo_we => '0',
        addr_lo => (others=>'0')
    );
    signal reg: ci_type := reg_reset;
    signal ci_next: ci_type;
    
    signal delayed_we: std_logic;
    signal delayed_we_1: std_logic;
    
    signal delayed_data: std_logic_vector(7 downto 0);
    signal delayed_data_1: std_logic_vector(7 downto 0);
begin

    busy <= fifo_prog_full; -- should work
    fifo_rst <= reg.fifo_rst;
    fifo_din <= reg.fifo_data;
    fifo_we <= reg.fifo_we;

    DELAY: process(clk, we, data, delayed_we_1, delayed_data_1)
    begin
        if(rising_edge(clk)) then
            delayed_we <= delayed_we_1;
            delayed_we_1 <= we;
        
            delayed_data <= delayed_data_1;
            delayed_data_1 <= data;
        end if;
    end process DELAY;

    COMB: process(reg, rst, addr, we, data, fifo_prog_full, delayed_we, delayed_data)
        variable ci: ci_type;
    begin
        ci := reg;
        -- self-clearing flags
        ci.fifo_rst := '0';
        ci.fifo_we := '0';
        
        if(rst = '1') then
            ci := reg_reset;
        else
            case reg.state is
                when state_idle =>
                    if(we = '1' and fifo_prog_full = '0') then
                        ci.state := state_write_addr_lo;
                        ci.fifo_data := addr(17 downto 9);
                        ci.fifo_we := '1';
                        ci.addr_lo := addr(8 downto 0);
                        ci.data_count := (others=>'0');
                    end if;
                when state_write_addr_lo =>                    
                    ci.state := state_write_data;
                    ci.fifo_data := reg.addr_lo;
                    ci.fifo_we := '1';
                when state_write_data =>
                    if(delayed_we = '1') then
                        ci.data_count := reg.data_count + "1";
                        ci.fifo_data := "0" & delayed_data;
                        ci.fifo_we := '1';
                        if(reg.data_count = "111111") then
                            -- this was the last write
                            ci.state := state_idle;
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

end architecture rtl;
