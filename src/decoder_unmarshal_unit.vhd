-- One half of a CDC for the decoder programming interface; this part lives in the 200 MHz domain
-- and grabs address/data information out of the FIFO to then be shoved into the prefetch controller.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity decoder_unmarshal_unit is port (
    clk: in std_logic;
    rst: in std_logic;
    fifo_dout: in std_logic_vector(8 downto 0); -- assuming FWFT
    fifo_re: out std_logic;
    fifo_empty: in std_logic;
    -- prefetch
    prefetch_addr: out std_logic_vector(17 downto 0);
    prefetch_we: out std_logic;
    prefetch_data: out std_logic_vector(7 downto 0);
    prefetch_busy: in std_logic
); end entity decoder_unmarshal_unit;

architecture rtl of decoder_unmarshal_unit is
    type state_type is (state_idle, state_addr_lo, state_data);
    type ci_type is record
        state: state_type;
        data_count: unsigned(5 downto 0);
        fifo_re: std_logic;
        prefetch_addr: std_logic_vector(17 downto 0);
        prefetch_we: std_logic;
        prefetch_data: std_logic_vector(7 downto 0);
    end record;
    
    constant reg_reset: ci_type := (    
        state => state_idle,
        data_count => (others=>'0'),
        fifo_re => '1',
        prefetch_addr => (others=>'0'),
        prefetch_we => '0',
        prefetch_data => (others=>'0')
    );
    
    signal reg: ci_type := reg_reset;
    signal ci_next: ci_type;
    
begin

    fifo_re <= reg.fifo_re;
    prefetch_addr <= reg.prefetch_addr;
    prefetch_we <= reg.prefetch_we;
    prefetch_data <= reg.prefetch_data;
    
    COMB: process(reg, rst, fifo_dout, fifo_empty, prefetch_busy)
        variable ci: ci_type;
    begin
        ci := reg;
        -- self-clearing
        ci.fifo_re := '1'; -- assume we can always read from the FIFO
        ci.prefetch_we := '0';
        if(rst = '1') then
            ci := reg_reset;
        else
            case reg.state is
                when state_idle =>
                    if(fifo_empty = '0') then
                        ci.state := state_addr_lo;
                        ci.prefetch_addr(17 downto 9) := fifo_dout;
                        ci.data_count := (others=>'0');
                    end if;
                when state_addr_lo =>
                    if(fifo_empty = '0') then
                        ci.state := state_data;
                        ci.prefetch_addr(8 downto 0) := fifo_dout;
                    end if;
                when state_data =>
                    if(prefetch_busy = '1') then
                        ci.fifo_re := '0';
                    elsif(fifo_empty = '0') then
                        ci.prefetch_data := fifo_dout(7 downto 0);
                        ci.prefetch_we := '1';
                        ci.data_count := reg.data_count + "1";
                        if(reg.data_count = "111111") then
                            -- this was the last one
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
