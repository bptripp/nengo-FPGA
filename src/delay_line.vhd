library ieee;
use ieee.std_logic_1164.all;

entity delay_line is generic (
    N: natural := 1; -- port width
    T: natural := 0
); port (
    clk: in std_logic;
    d: in std_logic_vector(N-1 downto 0);
    q: out std_logic_vector(N-1 downto 0)
); end entity;

architecture rtl of delay_line is
    type delay_chain_type is array(T downto 0) of std_logic_vector(N-1 downto 0);
    signal delay_chain: delay_chain_type := (others=>(others=>'0'));
begin
    delay_chain(T) <= d;
    
    CHAIN: if (T > 0) generate
        LINK: for I in 0 to T-1 generate
            process(clk, delay_chain)
            begin
                if(rising_edge(clk)) then
                    delay_chain(I) <= delay_chain(I+1);
                end if;
            end process;
        end generate;
    end generate;
    
    q <= delay_chain(0);
end architecture rtl;
