library ieee;
use ieee.std_logic_1164.all;

entity output_channel_tb is
end entity;

architecture sim of output_channel_tb is
	constant CLOCK_PERIOD: time := 5 ns;
	signal clk: std_logic;
	
	component output_channel port (
	  clk: in std_logic;
	  rst: in std_logic;
	  
	  start: in std_logic;
	  dv_addr: out std_logic_vector(18 downto 0);
	  dv_port: out std_logic;
	  dv_data: in std_logic_vector(11 downto 0);
	  channel_data: out std_logic_vector(11 downto 0);
	  channel_we: out std_logic;
	  done: out std_logic;

	  prog_ok: in std_logic;
	  prog_we: in std_logic;
	  prog_data: in std_logic_vector(35 downto 0)
	); end component;
	
	signal rst: std_logic;
	signal start: std_logic;
	signal dv_addr: std_logic_vector(18 downto 0);
	signal dv_port: std_logic;
	signal dv_data: std_logic_vector(11 downto 0);
	signal channel_data: std_logic_vector(11 downto 0);
	signal channel_we: std_logic;
	signal done: std_logic;
	
	signal prog_ok: std_logic;
	signal prog_we: std_logic;
	signal prog_data: std_logic_vector(35 downto 0);	
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
	end process;
	
	uut: output_channel port map (
		clk => clk,
		rst => rst,
		start => start,
		dv_addr => dv_addr,
		dv_port => dv_port,
		dv_data => dv_data,
		channel_data => channel_data,
		channel_we => channel_we,
		done => done,
		prog_ok => prog_ok,
		prog_we => prog_we,
		prog_data => prog_data
	);
	
	tb: process
	begin
		-- initial conditions
		rst <= '1';
		start <= '0';
		dv_data <= (others=>'0'); -- FIXME external memory
		prog_ok <= '0';
		prog_we <= '0';
		prog_data <= (others=>'0');
		
		wait for CLOCK_PERIOD*3;
		rst <= '0';
		wait for CLOCK_PERIOD*3;
		
		-- program the controller
		wait until falling_edge(clk);
		prog_ok <= '1';
		prog_we <= '1';
		
		-- read 12 words from DV buffer 0 port 0, starting at address 4
		prog_data <= "0" & "0" & "00000000" & "00000000100" & "00000001011" & "0000";		
		wait for CLOCK_PERIOD;
		-- read 8 words from DV buffer 2 port 1, starting at address 0 (delay for 3 extra cycles)
		prog_data <= "1" & "1" & "00000010" & "00000000000" & "00000000111" & "0011";		
		wait for CLOCK_PERIOD;
		
		prog_we <= '0';
		prog_ok <= '0';
		
		wait for CLOCK_PERIOD*3;
		start <= '1';
		wait for CLOCK_PERIOD;
		start <= '0';
		
		wait;
		
	end process;

end architecture sim;
