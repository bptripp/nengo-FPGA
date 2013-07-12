library ieee;
use ieee.std_logic_1164.all;

entity principal_component_2d_tb is
end entity;

architecture sim of principal_component_2d_tb is
  signal clk: std_logic;      
  constant CLOCK_PERIOD: time := 5 ns;
    
  component principal_component_2d
  generic (
      loadfile: string
  ); 
  Port (
    clk : in STD_LOGIC;
    x_in : in STD_LOGIC_VECTOR (11 downto 0); -- FIXME was (9 downto 0)
    y_in: in std_logic_vector(11 downto 0);
    data : out STD_LOGIC_VECTOR (11 downto 0);
    ready : in STD_LOGIC;
    valid : out STD_LOGIC;
               
    prog_addr: in std_logic_vector(9 downto 0);
    prog_cs: in std_logic;
    prog_we: in std_logic;
    prog_data: in std_logic_vector(11 downto 0)
   ); end component;

  signal x_in: std_logic_vector(11 downto 0);
  signal y_in: std_logic_vector(11 downto 0);
  signal data: std_logic_vector(11 downto 0);
  signal ready: std_logic;
  signal valid: std_logic;
  
  constant ZERO: std_logic := '0';
  constant ONE: std_logic := '1';
  
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

    uut: principal_component_2d
    generic map (
        loadfile => "pc_2d_0.rom"
    ) 
    port map (
      clk => clk,
      x_in => x_in,
      y_in => y_in,
      data => data,
      ready => ready,
      valid => valid,
      prog_addr => "0000000000",
      prog_cs => '0',
      prog_we => '0',
      prog_data => "000000000000"
      );

    TB: process
    begin
      x_in <= "00101" & "1101001";
      y_in <= "10110" & "0001011";
      ready <= '0';
      
      wait until falling_edge(clk);
      ready <= '1';
      wait for CLOCK_PERIOD;
      ready <= '0';

      wait;
    end process TB;
    
end architecture sim;
