library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dv_block is
    port (
    clk: in std_logic;
    rst: in std_logic;
    port0_addr: in std_logic_vector(10 downto 0);
    port0_we: in std_logic;
    port0_di: in std_logic_vector(11 downto 0);
    port0_do: out std_logic_vector(11 downto 0);

    port1_addr: in std_logic_vector(10 downto 0);
    port1_we: in std_logic;
    port1_di: in std_logic_vector(11 downto 0);
    port1_do: out std_logic_vector(11 downto 0)
    );
end entity;

architecture rtl of dv_block is
    type ram_type is array(0 to 2047) of std_logic_vector(11 downto 0);
    shared variable RAM: ram_type := (others=>X"000");
begin

    PORT0: process(clk, rst, port0_addr, port0_we, port0_di)
    begin
        if(rising_edge(clk)) then
            port0_do <= RAM(to_integer(unsigned(port0_addr)));
            if(rst = '1') then
                RAM := (others=>X"000");                
            elsif(port0_we = '1') then
                RAM(to_integer(unsigned(port0_addr))) := port0_di;
            end if;
        end if;
    end process PORT0;
    
    PORT1: process(clk, port1_addr, port1_we, port1_di)
    begin
        if(rising_edge(clk)) then
            port1_do <= RAM(to_integer(unsigned(port1_addr)));
            if(port1_we = '1') then
                RAM(to_integer(unsigned(port1_addr))) := port1_di;
            end if;
        end if;
    end process PORT1;    

end architecture rtl;