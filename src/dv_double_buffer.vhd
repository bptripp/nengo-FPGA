library ieee;
use ieee.std_logic_1164.all;

-- DV blocks are 2048x12, so 11 address bits and 12 data bits

entity dv_double_buffer is
    port (
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
    );
end entity;

architecture rtl of dv_double_buffer is

    signal active_set: std_logic := '0'; -- mux select bit
    
    signal bank0_port0_addr: std_logic_vector(10 downto 0);
    signal bank0_port0_we: std_logic;
    signal bank0_port0_di: std_logic_vector(11 downto 0);
    signal bank0_port0_do: std_logic_vector(11 downto 0);
    
    signal bank0_port1_addr: std_logic_vector(10 downto 0);
    signal bank0_port1_we: std_logic;
    signal bank0_port1_di: std_logic_vector(11 downto 0);
    signal bank0_port1_do: std_logic_vector(11 downto 0);
    
    signal bank1_port0_addr: std_logic_vector(10 downto 0);
    signal bank1_port0_we: std_logic;
    signal bank1_port0_di: std_logic_vector(11 downto 0);
    signal bank1_port0_do: std_logic_vector(11 downto 0);
    
    signal bank1_port1_addr: std_logic_vector(10 downto 0);
    signal bank1_port1_we: std_logic;
    signal bank1_port1_di: std_logic_vector(11 downto 0);
    signal bank1_port1_do: std_logic_vector(11 downto 0);
    
    component dv_block PORT (
      clka : IN STD_LOGIC;
      wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
      addra : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
      dina : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
      douta : OUT STD_LOGIC_VECTOR(11 DOWNTO 0);
      clkb : IN STD_LOGIC;
      web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
      addrb : IN STD_LOGIC_VECTOR(10 DOWNTO 0);
      dinb : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
      doutb : OUT STD_LOGIC_VECTOR(11 DOWNTO 0)
    ); end component dv_block;
        
begin

BANK0: dv_block port map (
    clka => clk,
    clkb => clk,    
    addra => bank0_port0_addr,
    wea(0) => bank0_port0_we,
    dina => bank0_port0_di,
    douta => bank0_port0_do,
    addrb => bank0_port1_addr,
    web(0) => bank0_port1_we,
    dinb => bank0_port1_di,
    doutb => bank0_port1_do    
);

BANK1: dv_block port map (
    clka => clk,
    clkb => clk,    
    addra => bank1_port0_addr,
    wea(0) => bank1_port0_we,
    dina => bank1_port0_di,
    douta => bank1_port0_do,
    addrb => bank1_port1_addr,
    web(0) => bank1_port1_we,
    dinb => bank1_port1_di,
    doutb => bank1_port1_do    
);

SWAP: process(clk, swap_banks)
begin
    if(rising_edge(clk)) then
        if(swap_banks = '1') then
            active_set <= not active_set;
        end if;
    end if;
end process SWAP;

-- mux read port 0 and write port 0 onto bank 0 port 0
bank0_port0_addr <= prog_addr when prog_ok = '1' else rd0_addr when active_set = '0' else wr0_addr;
bank0_port0_we <= prog_we when prog_ok = '1' else wr0_we when active_set = '1' else '0';
bank0_port0_di <= prog_data when prog_ok = '1' else wr0_data;

-- mux read port 0 and write port 0 onto bank 1 port 0
bank1_port0_addr <= prog_addr when prog_ok = '1' else rd0_addr when active_set = '1' else wr0_addr;
bank1_port0_we <= prog_we when prog_ok = '1' else wr0_we when active_set = '0' else '0';
bank1_port0_di <= prog_data when prog_ok = '1' else wr0_data;

-- mux bank 0 port 0 data-out and bank 1 port 0 data onto read port 0
rd0_data <= bank0_port0_do when active_set = '0' else bank1_port0_do; 

-- mux read port 1 and write port 1 onto bank 0 port 1
bank0_port1_addr <= rd1_addr when active_set = '0' else wr1_addr;
bank0_port1_we <= '0' when prog_ok = '1' else wr1_we when active_set = '1' else '0';
bank0_port1_di <= wr1_data;

-- mux read port 1 and write port 1 onto bank 1 port 1
bank1_port1_addr <= rd1_addr when active_set = '1' else wr1_addr;
bank1_port1_we <= '0' when prog_ok = '1' else wr1_we when active_set = '0' else '0';
bank1_port1_di <= wr1_data;

-- mux bank 0 port 1 data-out and bank 1 port 1 data onto read port 1
rd1_data <= bank0_port1_do when active_set = '0' else bank1_port1_do; 

end architecture rtl;